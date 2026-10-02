# End-to-end proof that the opt-in service worker does not break cross-origin
# isolation.
#
# This is the one claim in the runtime-caching work that cannot be settled by
# reading the code. webR needs SharedArrayBuffer, which the browser only grants
# to a cross-origin-isolated page (COOP: same-origin + COEP: require-corp on
# the document, and CORP-compatible subresources). A service worker that
# answers with a Response it synthesized carries none of those headers, so the
# page silently drops to `crossOriginIsolated === false` and webR falls back to
# its slow channel - or fails outright.
#
# webrarian's worker therefore only ever returns the untouched network Response
# or a verbatim Cache Storage replay. The test below installs the worker, then
# loads the site a *second* time so the navigation itself is served through the
# worker's fetch handler, and asserts on that second load that
#
#   1. the worker is controlling the page (so it really is in the path), and
#   2. crossOriginIsolated is still TRUE, and
#   3. exlibris still boots webR through the worker, and
#   4. the engine landed in a cache named for this build.
#
# Heavy and network-bound; gated off CRAN like test-browser.R and driven with
# the same chromote harness (see the comments there for why the static server
# runs out of process).

test_that("the service worker keeps the page cross-origin isolated", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  # Build the basic example with the engine bundled locally and the worker on.
  proj <- withr::local_tempdir()
  file.copy(
    list.files(collection_example("basic"), full.names = TRUE),
    proj,
    recursive = TRUE
  )
  settings_set(proj, "build/service_worker" = TRUE)
  suppressMessages(bind(proj))
  site <- file.path(proj, "_site")

  expect_true(file.exists(file.path(site, "sw.js")))
  sw_src <- readLines(file.path(site, "sw.js"), warn = FALSE)
  build_id <- sub(
    '^const BUILD_ID = "([^"]+)".*$',
    "\\1",
    grep("^const BUILD_ID", sw_src, value = TRUE)
  )
  expect_length(build_id, 1)

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(site, port)
  withr::defer(stop_dir_server(server))
  url <- sprintf("http://127.0.0.1:%d/", port)

  b <- chromote::ChromoteSession$new()
  withr::defer(b$close())

  # Same pre-navigation error collector as test-browser.R: a boot failure has
  # to surface as recorded text rather than a bare timeout.
  b$Page$enable()
  b$Page$addScriptToEvaluateOnNewDocument(
    source = paste0(
      "window.__consoleErrors__ = [];",
      "window.addEventListener('error', function(e){ window.__consoleErrors__.push(String(e.message)); });",
      "window.addEventListener('unhandledrejection', function(e){ window.__consoleErrors__.push('unhandledrejection: ' + String(e.reason)); });",
      "(function(){ var o = console.error; console.error = function(){ window.__consoleErrors__.push(Array.prototype.map.call(arguments, String).join(' ')); return o.apply(console, arguments); }; })();"
    )
  )

  navigate <- function() {
    load_fired <- b$Page$loadEventFired(wait_ = FALSE)
    b$Page$navigate(url, wait_ = FALSE)
    b$wait_for(load_fired)
  }

  wait_for <- function(expr, timeout = 180) {
    deadline <- Sys.time() + timeout
    repeat {
      val <- b$Runtime$evaluate(expr, awaitPromise = TRUE)$result$value
      if (isTRUE(val)) {
        return(TRUE)
      }
      if (Sys.time() > deadline) {
        return(FALSE)
      }
      Sys.sleep(1)
    }
  }

  # --- First visit: registers and activates the worker. -------------------
  navigate()

  # The worker calls skipWaiting() + clients.claim(), so it takes control of
  # this very page without a reload; that is the signal it finished activating.
  expect_true(wait_for("!!navigator.serviceWorker.controller", timeout = 60))

  # --- Second visit: now served *through* the worker. ---------------------
  navigate()

  # It is controlling from the first byte this time, so the document response
  # itself came back through the worker's fetch handler.
  controlled <- b$Runtime$evaluate("!!navigator.serviceWorker.controller")$result$value
  expect_true(isTRUE(controlled))

  # THE assertion: replaying/passing through the response preserved COOP+COEP.
  isolated <- b$Runtime$evaluate("crossOriginIsolated")$result$value
  expect_true(isTRUE(isolated))

  # SharedArrayBuffer is the thing crossOriginIsolated actually gates, so check
  # it directly rather than trusting the flag alone.
  has_sab <- b$Runtime$evaluate(
    "typeof SharedArrayBuffer === 'function' && new SharedArrayBuffer(8).byteLength === 8"
  )$result$value
  expect_true(isTRUE(has_sab))

  # And webR really does boot through the worker: exlibris only clears its
  # loading overlay on a successful boot.
  expect_true(wait_for("!!document.querySelector('.cm-editor')"))
  expect_true(wait_for("document.querySelector('.loading-overlay') === null"))

  # The engine landed in a cache keyed to this build.
  cache_names <- b$Runtime$evaluate(
    "caches.keys().then(function (k) { return JSON.stringify(k); })",
    awaitPromise = TRUE
  )$result$value
  names_vec <- jsonlite::fromJSON(cache_names)
  expect_true(
    any(grepl(build_id, names_vec, fixed = TRUE)),
    info = paste(names_vec, collapse = " | ")
  )

  # R.wasm is the single biggest asset on the wire (~18 MB) and the whole point
  # of the exercise. (webr.mjs is deliberately not checked: the webR client is
  # bundled inside exlibris-r.js, so it is never fetched as a separate file.)
  cached_engine <- wait_for(
    sprintf(
      "caches.open(%s).then(function (c) { return c.keys(); }).then(function (k) {
       return k.some(function (r) { return /R\\.wasm$/.test(r.url); }); })",
      jsonlite::toJSON(names_vec[grepl(build_id, names_vec, fixed = TRUE)][1], auto_unbox = TRUE)
    ),
    timeout = 60
  )
  expect_true(cached_engine)

  # The cached copies must still carry the isolation headers, or the visit that
  # is served out of this cache would be the one that loses SharedArrayBuffer.
  # Checked on both the document (COOP is what makes the *page* isolated) and
  # an engine subresource (COEP/Content-Type is what makes it loadable).
  header_of <- function(path, header) {
    b$Runtime$evaluate(
      sprintf(
        "caches.match(new URL('%s', location.href).href).then(function (r) {
         return r ? String(r.headers.get('%s')) : 'MISS'; })",
        path,
        header
      ),
      awaitPromise = TRUE
    )$result$value
  }
  expect_equal(header_of("./", "cross-origin-opener-policy"), "same-origin")
  expect_equal(header_of("./", "cross-origin-embedder-policy"), "require-corp")
  engine_wasm <- sprintf("webr/v%s/R.wasm", collection_settings(proj)$webr$version)
  expect_equal(header_of(engine_wasm, "cross-origin-embedder-policy"), "require-corp")
  expect_equal(header_of(engine_wasm, "content-type"), "application/wasm")

  errs <- b$Runtime$evaluate("JSON.stringify(window.__consoleErrors__ || [])")$result$value
  errs_vec <- jsonlite::fromJSON(errs)
  expect_equal(length(errs_vec), 0L, info = paste(errs_vec, collapse = " | "))
})

test_that("turning the service worker off retires it for returning visitors", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  proj <- withr::local_tempdir()
  file.copy(list.files(collection_example("basic"), full.names = TRUE), proj, recursive = TRUE)
  settings_set(proj, "build.service-worker" = TRUE)
  suppressMessages(bind(proj))
  site <- file.path(proj, "_site")

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(site, port)
  withr::defer(stop_dir_server(server))
  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))

  page$navigate()
  expect_true(page$wait_for("!!navigator.serviceWorker.controller", timeout = 60))

  settings_set(proj, "build.service-worker" = FALSE)
  suppressMessages(bind(proj))

  # A navigation triggers the browser's update check, which installs the
  # retire worker; it deletes its caches, unregisters and reloads the page.
  page$navigate()
  expect_true(page$wait_for(
    "navigator.serviceWorker.getRegistrations().then(function (r) { return r.length === 0; })",
    timeout = 60
  ))
  page$navigate()
  expect_true(page$wait_for("!navigator.serviceWorker.controller", timeout = 30))
  expect_true(page$wait_for(
    "caches.keys().then(function (k) { return !k.some(function (n) { return n.indexOf('webrarian:') === 0; }); })",
    timeout = 30
  ))
})

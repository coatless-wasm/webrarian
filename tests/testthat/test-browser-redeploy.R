# A returning visitor must get the new build after a redeploy, even from a
# host that honors the immutable rules in _headers. The version
# list holds one engine line (0.6.x), so the URL that changes here is the
# viewer bundle's content-hashed name; the user's files keep their URL and
# revalidate.

# Serve `site` on `port` the way a host applying _headers does: the engine and
# the hashed viewer bundle immutable, everything else revalidating. Call it
# again after a rebuild: the bundle's file name is part of the configuration.
serve_like_a_host <- function(site, port) {
  isolation <- list(
    "Cross-Origin-Opener-Policy" = "same-origin",
    "Cross-Origin-Embedder-Policy" = "require-corp"
  )
  immutable <- c(isolation, list("Cache-Control" = "public, max-age=31536000, immutable"))
  revalidate <- c(isolation, list("Cache-Control" = "no-cache"))
  bundle <- fs::dir_ls(site, regexp = "exlibris-r\\.[0-9a-f]{8}\\.(js|css)$")
  paths <- c(
    stats::setNames(
      lapply(bundle, function(f) httpuv::staticPath(f, fallthrough = FALSE, headers = immutable)),
      paste0("/", fs::path_file(bundle))
    ),
    list(
      "/webr" = httpuv::staticPath(
        fs::path(site, "webr"),
        fallthrough = FALSE,
        headers = immutable
      ),
      "/" = httpuv::staticPath(site, indexhtml = TRUE, fallthrough = FALSE, headers = revalidate)
    )
  )
  httpuv::startServer(
    "127.0.0.1",
    port,
    list(
      call = function(req) {
        list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not Found")
      },
      staticPaths = paths
    )
  )
}

test_that("a redeploy reaches a returning visitor: a new viewer bundle and new file contents", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(tempdir(), "webrarian-test-cache"))

  # The engine in the site, served immutable like the hashed bundle.
  root <- local_collection(bundle_engine = TRUE)
  writeLines('cat("BUILD:", "one", "\\n")', fs::path(root, "build.R"))
  suppressMessages(settings_set(
    root,
    "files.include" = list("build.R"),
    "repl.auto-run" = list("build.R")
  ))
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  bundle_js <- function() {
    fs::path_file(fs::dir_ls(site, regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"))
  }
  first_js <- bundle_js()

  port <- httpuv::randomPort()
  current <- new.env()
  current$server <- serve_like_a_host(site, port)
  withr::defer(httpuv::stopServer(current$server))

  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
  terminal_has <- function(text) {
    page$wait_for(
      sprintf(
        "((document.querySelector('.xterm-rows') || document.body).innerText || '').indexOf(%s) >= 0",
        jsonlite::toJSON(text, auto_unbox = TRUE)
      ),
      timeout = 180
    )
  }
  module_src <- function() {
    page$eval("document.querySelector('script[type=module]').getAttribute('src')")
  }

  page$navigate()
  expect_true(terminal_has("BUILD: one"), info = paste(page$errors(), collapse = " | "))
  expect_equal(module_src(), paste0("./", first_js))

  # Redeploy with a rebuilt viewer bundle (same engine) and an edited file.
  src <- fs::path(withr::local_tempdir(), "viewer")
  fs::dir_copy(viewer_source_dir(), src)
  cat("\n// rebuilt\n", file = fs::path(src, "exlibris-r.js"), append = TRUE)
  local_mocked_bindings(viewer_source_dir = function() src)
  writeLines('cat("BUILD:", "two", "\\n")', fs::path(root, "build.R"))
  suppressMessages(bind(root))
  second_js <- bundle_js()
  expect_false(identical(first_js, second_js))
  expect_false(fs::file_exists(fs::path(site, first_js)))

  httpuv::stopServer(current$server)
  current$server <- serve_like_a_host(site, port)

  page$navigate()
  expect_true(terminal_has("BUILD: two"), info = paste(page$errors(), collapse = " | "))
  expect_equal(module_src(), paste0("./", second_js))
  expect_true(isTRUE(page$eval(sprintf(
    "performance.getEntriesByType('resource').some(function (e) { return e.name.endsWith(%s); })",
    jsonlite::toJSON(paste0("/", second_js), auto_unbox = TRUE)
  ))))
})

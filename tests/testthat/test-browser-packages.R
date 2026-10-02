# The engine lives under webr/v<version>/, and the page itself may live under
# a sub-path (a GitHub Pages project site). webR resolves engine-base-url and
# the package repositories inside its worker, so they must reach it as
# absolute URLs (viewer_url_resolver_script()). This boots a real
# engine and installs a real bundled package.

# Serve `site` at `prefix` with the isolation headers webR needs. Everything
# outside the prefix (the browser's /favicon.ico) is a 404 from httpuv's I/O
# thread: an R request handler could not run while chromote blocks the session.
serve_under_prefix <- function(site, prefix, env = parent.frame()) {
  isolation <- list(
    "Cross-Origin-Opener-Policy" = "same-origin",
    "Cross-Origin-Embedder-Policy" = "require-corp"
  )
  empty <- withr::local_tempdir(.local_envir = env)
  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1",
    port,
    list(
      call = function(req) {
        list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not Found")
      },
      staticPaths = stats::setNames(
        list(
          httpuv::staticPath(site, indexhtml = TRUE, fallthrough = FALSE, headers = isolation),
          httpuv::staticPath(empty, indexhtml = FALSE, fallthrough = FALSE)
        ),
        c(prefix, "/")
      )
    )
  )
  withr::defer(httpuv::stopServer(server), envir = env)
  sprintf("http://127.0.0.1:%d%s/", port, prefix)
}

# An offline site (build.offline: true) that bundles glue and runs check.R.
local_offline_glue_site <- function(env = parent.frame()) {
  root <- local_collection(env = env, bundle_engine = TRUE)
  writeLines(
    c("library(glue)", 'cat(glue("GLUE_OK {1 + 1}"), "\\n")'),
    fs::path(root, "check.R")
  )
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("glue"),
    "files.include" = list("check.R"),
    "repl.auto-run" = list("check.R"),
    "build.offline" = TRUE
  ))
  suppressMessages(bind(root))
  fs::path(root, "_site")
}

terminal_shows <- function(text) {
  sprintf(
    "((document.querySelector('.xterm-rows') || document.body).innerText || '').indexOf(%s) >= 0",
    jsonlite::toJSON(text, auto_unbox = TRUE)
  )
}

test_that("an offline site served from a sub-path boots its versioned engine and installs a bundled package", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(tempdir(), "webrarian-test-cache"))

  site <- local_offline_glue_site()
  wire <- read_site_config(site)
  version <- wire[["engine-version"]]
  expect_true(fs::file_exists(fs::path(site, "webr", paste0("v", version), "R.wasm")))
  expect_length(
    fs::dir_ls(fs::path(site, "repo"), recurse = TRUE, regexp = "glue_[^/]*\\.tgz$"),
    1L
  )
  expect_equal(wire[["engine-base-url"]], sprintf("./webr/v%s/", version))
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_length(wire$packages$repos, 0L)
  expect_true(wire$offline)

  page <- local_browser_page(serve_under_prefix(site, "/course"))
  page$navigate()
  expect_true(
    page$wait_for(terminal_shows("GLUE_OK 2"), timeout = 180),
    info = paste(page$errors(), collapse = " | ")
  )
})

# Parent spec, "Offline sites and share links": on an offline site an open
# link's ?packages= can add only bundled packages; anything else ends with a
# ✗ and a reason, never a request to another origin.
test_that("an offline site fails a link's unbundled package with a reason, and names no other origin", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(tempdir(), "webrarian-test-cache"))

  url <- serve_under_prefix(local_offline_glue_site(), "/course")
  origin <- sub("^(https?://[^/]+).*$", "\\1", url)
  # R6 is on repo.r-wasm.org but not in this site's repo/.
  page <- local_browser_page(paste0(url, "?packages=R6"))
  requested <- character()
  page$session$Network$enable()
  page$session$Network$requestWillBeSent(callback_ = function(msg) {
    requested <<- c(requested, msg$request$url)
  })
  page$navigate()

  expect_true(
    page$wait_for(terminal_shows("✗ R6: "), timeout = 180),
    info = paste(page$errors(), collapse = " | ")
  )
  expect_true(page$wait_for(terminal_shows("GLUE_OK 2"), timeout = 60))

  # What the page hands webR's worker: the offline flag, and every engine and
  # package source on the site's own origin (after the resolver script).
  expect_true(isTRUE(page$eval("window.__VIEWER_CONFIG__.offline === true")))
  sources <- jsonlite::fromJSON(page$eval(paste0(
    "JSON.stringify([window.__VIEWER_CONFIG__['engine-base-url'], ",
    "window.__VIEWER_CONFIG__.packages['repo-url']]",
    ".concat(window.__VIEWER_CONFIG__.packages.repos || []))"
  )))
  expect_true(all(startsWith(sources, paste0(origin, "/"))), info = paste(sources, collapse = " "))
  # Every request the page itself made stayed on the origin. (CDP's page
  # session does not report a dedicated worker's own fetches; checked in a
  # scratch run on 2026-09-25. The worker can only reach the sources above.)
  off_origin <- requested[!startsWith(requested, origin) & !grepl("^(data|blob):", requested)]
  expect_length(off_origin, 0L)
})

# The viewer links the site's licenses (the license exception):
# LICENSES/index.html, which emit_licenses() writes into every site.
test_that("a built site's footer links to its LICENSES page, and the link resolves", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  # local_collection() builds with build.bundle-engine: false, so the engine comes from the CDN;
  # the footer is drawn before the engine finishes booting.
  root <- local_collection()
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  expect_true(fs::file_exists(fs::path(site, "LICENSES", "index.html")))

  page <- local_browser_page(serve_under_prefix(site, "/course"))
  page$navigate()
  expect_true(
    page$wait_for("!!document.querySelector('.shell-footer a')", timeout = 60),
    info = paste(page$errors(), collapse = " | ")
  )
  expect_identical(page$eval("document.querySelector('.shell-footer a').textContent"), "Licenses")
  expect_identical(
    page$eval("document.querySelector('.shell-footer a').getAttribute('href')"),
    "LICENSES/index.html"
  )
  expect_true(page$eval(paste0(
    "fetch(new URL(document.querySelector('.shell-footer a').getAttribute('href'), document.baseURI))",
    ".then(function (r) { return r.ok ? r.text() : ''; })",
    ".then(function (t) { return t.indexOf('<title>Licenses</title>') >= 0; })"
  )))
})

# exlibris mounts the image and installs nothing: the package loads
# from /exlibris/library/1, not from R's own library.
test_that("a site that bundles its packages loads them from the mounted library image", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(tempdir(), "webrarian-test-cache"))

  # The real engine and glue, bundled (this test downloads them into the cache above).
  root <- local_collection(bundle_engine = TRUE)
  writeLines(
    c(
      "library(glue)",
      'cat(glue("GLUE_OK {1 + 1}"), "\\n")',
      'cat("GLUE_FROM", find.package("glue"), "\\n")'
    ),
    fs::path(root, "check.R")
  )
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("glue"),
    "files.include" = list("check.R"),
    "repl.auto-run" = list("check.R")
  ))
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  expect_length(unlist(read_site_config(site)$packages[["library-images"]]), 1L)

  page <- local_browser_page(serve_under_prefix(site, "/course"))
  page$navigate()
  expect_true(
    page$wait_for(
      "((document.querySelector('.xterm-rows') || document.body).innerText || '').indexOf('GLUE_FROM /exlibris/library/1/glue') >= 0",
      timeout = 180
    ),
    info = paste(page$errors(), collapse = " | ")
  )
  expect_true(grepl("GLUE_OK 2", page$terminal_text(), fixed = TRUE))
})

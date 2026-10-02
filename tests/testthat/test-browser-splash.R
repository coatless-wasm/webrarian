# If the viewer bundle cannot be loaded, the host splash must say so instead
# of spinning forever (host side).

test_that("a missing viewer bundle turns the splash into an error with a Reload button", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  root <- local_collection()
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  fs::file_delete(fs::dir_ls(site, regexp = "exlibris-r[^/]*\\.js$"))

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(site, port)
  withr::defer(stop_dir_server(server))

  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
  page$navigate()

  expect_true(page$wait_for(
    "document.getElementById('webrarian-loading').getAttribute('data-state') === 'error'",
    timeout = 15
  ))
  status <- page$eval("document.getElementById('webrarian-status').textContent")
  expect_match(status, "could not be loaded")
  expect_true(isTRUE(page$eval("!!document.querySelector('#webrarian-status button')")))
})

test_that("a wrong-Content-Type 200 response for the viewer bundle also turns the splash into an error", {
  # Not just the 404 case above: a module script gets strict MIME-type
  # checking per the HTML module-script-fetch algorithm, so a non-JS
  # Content-Type on an otherwise-200 response is a fetch failure too, caught
  # by the same capturing `error` listener on window (R/html.R), which
  # filters only on the element being a module script, not on why it failed.
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  root <- local_collection()
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  bundle <- fs::dir_ls(site, regexp = "exlibris-r[^/]*\\.js$")
  bundle_path <- paste0("/", fs::path_file(bundle))

  port <- httpuv::randomPort()
  server <- serve_dir_isolated_with_content_type_override(
    site,
    port,
    stats::setNames(list("text/html; charset=utf-8"), bundle_path)
  )
  withr::defer(stop_dir_server(server))

  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
  page$navigate()

  expect_true(page$wait_for(
    "document.getElementById('webrarian-loading').getAttribute('data-state') === 'error'",
    timeout = 15
  ))
  status <- page$eval("document.getElementById('webrarian-status').textContent")
  expect_match(status, "could not be loaded")
  expect_true(isTRUE(page$eval("!!document.querySelector('#webrarian-status button')")))
})

test_that("a viewer that mounts after the timeout still takes over from the error", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  dir <- withr::local_tempdir()
  writeLines(
    c(
      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body>",
      generate_loading_html(loading_title = "Opening", loading_message = "Loading webR..."),
      '<div id="root"></div>',
      generate_loading_dismiss_script(1),
      # Stands in for a viewer bundle on a slow connection: it mounts into #root
      # three seconds in, two seconds after the one-second timeout fired.
      "<script>setTimeout(function () { document.getElementById('root').appendChild(document.createElement('div')); }, 3000);</script>",
      "</body></html>"
    ),
    fs::path(dir, "index.html")
  )

  page <- local_browser_page(paste0("file://", fs::path_real(fs::path(dir, "index.html"))))
  page$navigate()

  expect_true(page$wait_for(
    "document.getElementById('webrarian-loading').getAttribute('data-state') === 'error'",
    timeout = 10
  ))
  expect_true(page$wait_for(
    paste0(
      "(function () { var s = document.getElementById('webrarian-loading');",
      " return s.classList.contains('hidden') && !s.hasAttribute('data-state'); })()"
    ),
    timeout = 10
  ))
})

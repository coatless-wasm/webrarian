# Real Docker. Runs only when asked (WEBRARIAN_TEST_DOCKER=true), which the
# ubuntu CI job with a Docker daemon sets; never on CRAN.

test_that("the local-package example compiles with real Docker and installs its package", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("WEBRARIAN_TEST_DOCKER"), "true"),
    "set WEBRARIAN_TEST_DOCKER=true to run"
  )
  skip_if_not(identical(docker_status(), "running"), "Docker daemon not running")

  root <- fs::path(withr::local_tempdir(), "local-package")
  suppressMessages(collection_example("local-package", dest = root))
  suppressMessages(bind(root))

  site <- fs::path(root, "_site")
  wire <- read_site_config(site)
  expect_true("demotools" %in% unlist(wire$packages$install))
  r_line <- resolve_webr_version(wire[["engine-version"]])$r_version
  contrib <- fs::path(site, "repo", "bin", "emscripten", "contrib", r_line)
  expect_length(fs::dir_ls(contrib, regexp = "demotools_[^/]*\\.tgz$"), 1L)
})

test_that("an online site installs its compiled package from the site's own repo/", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("WEBRARIAN_TEST_DOCKER"), "true"),
    "set WEBRARIAN_TEST_DOCKER=true to run"
  )
  skip_if_not(identical(docker_status(), "running"), "Docker daemon not running")
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  root <- fs::path(withr::local_tempdir(), "local-package")
  suppressMessages(collection_example("local-package", dest = root))
  # demotools exists nowhere but the site's repo/, so loading it proves the
  # CDN engine's blob: worker reached <site>/repo through an absolute URL.
  writeLines(
    'library(demotools); cat("DEMOTOOLS_OK", exists("describe_vector"), "\\n")',
    fs::path(root, "check.R")
  )
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list(),
    "repl.auto-run" = list("check.R"),
    "build.bundle-engine" = FALSE
  ))
  suppressMessages(bind(root))
  wire <- read_site_config(fs::path(root, "_site"))
  expect_equal(wire$packages[["repo-url"]], "./repo")

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(fs::path(root, "_site"), port)
  withr::defer(stop_dir_server(server))
  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
  page$navigate()
  expect_true(
    page$wait_for(
      "((document.querySelector('.xterm-rows') || document.body).innerText || '').indexOf('DEMOTOOLS_OK TRUE') >= 0",
      timeout = 180
    ),
    info = paste(page$errors(), collapse = " | ")
  )
})

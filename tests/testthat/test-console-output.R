# Build console output.

test_that("bind() closes its progress steps before the summary, and the hint names the collection", {
  root <- local_collection()
  msgs <- testthat::capture_messages(bind(root))
  complete <- max(grep("Build complete", msgs, fixed = TRUE))
  after <- msgs[seq_along(msgs) > complete]
  expect_false(any(grepl("Emitting viewer", after, fixed = TRUE)))
  expect_true(any(grepl(sprintf('reading_room("%s")', root), after, fixed = TRUE)))
})

test_that("bind() inside watch mode prints no preview hint", {
  root <- local_collection()
  webrarian_env$in_watch <- TRUE
  withr::defer(webrarian_env$in_watch <- FALSE)
  msgs <- testthat::capture_messages(bind(root))
  expect_false(any(grepl("reading_room", msgs, fixed = TRUE)))
})

test_that("catalog() says when it edits .gitignore", {
  withr::with_tempdir({
    msgs <- testthat::capture_messages(catalog(detect = FALSE))
    expect_true(any(grepl(".gitignore", msgs, fixed = TRUE)))
  })
})

test_that("the engine download streams to disk and reports each step once", {
  local_isolated_cache()
  base <- withr::local_tempdir()
  release <- fs::path(base, "webr-0.6.0")
  fs::dir_create(fs::path(release, "vfs"))
  for (f in c(
    "R.wasm",
    "R.js",
    "webr-worker.js",
    "webr.mjs",
    "libRblas.so",
    "vfs/usr.data",
    "webr.mjs.map"
  )) {
    writeLines("x", fs::path(release, f))
  }
  tarball <- fs::path(base, "webr.tar.gz")
  withr::with_dir(base, utils::tar(tarball, "webr-0.6.0", compression = "gzip", tar = "internal"))

  seen_path <- NULL
  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      seen_path <<- path
      fs::file_copy(tarball, path, overwrite = TRUE)
      structure(list(status_code = 200L), class = "httr2_response")
    },
    resp_status = function(resp) 200L,
    .package = "httr2"
  )
  # The fake release is not webR 0.6.0's real tarball: skip its pinned checksum.
  local_mocked_bindings(webr_release_checksums = function(version) NULL)

  msgs <- testthat::capture_messages(dir <- webr_assets_download("0.6.0"))
  expect_false(is.null(seen_path))
  expect_true(fs::file_exists(fs::path(dir, "R.wasm")))
  expect_false(fs::file_exists(fs::path(dir, "webr.mjs.map")))
  expect_true(cache_is_complete(dir))
  expect_equal(sum(grepl("webR engine", msgs, fixed = TRUE)), 1L)
})

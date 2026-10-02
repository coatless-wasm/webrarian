# The package-repository module (R/repo.R), against a local fixture repo.

skip_if_not_installed("httpuv")

test_that("indexes of several repositories merge, and the first repository wins", {
  a <- local_fixture_repo(list(shared = c(Version = "1.0"), onlya = c(Version = "1.0")))
  b <- local_fixture_repo(list(shared = c(Version = "2.0"), onlyb = c(Version = "1.0")))
  idx <- suppressMessages(fetch_repo_index(c(a$url, b$url), "4.6"))
  expect_setequal(rownames(idx$packages), c("shared", "onlya", "onlyb"))
  expect_equal(unname(idx$packages["shared", "Version"]), "1.0")
  expect_match(idx$source[["onlyb"]], b$url, fixed = TRUE)
})

test_that("an empty or unreachable repository is reported, not skipped silently", {
  good <- local_fixture_repo(list(glue = c(Version = "1.0")))
  empty <- local_fixture_repo(list())
  msgs <- testthat::capture_messages(
    idx <- fetch_repo_index(c(good$url, empty$url, "http://127.0.0.1:9"), "4.6")
  )
  expect_true(any(grepl("no WebAssembly packages for R 4.6", msgs)))
  expect_true(any(grepl("Could not read the package index", msgs)))
  expect_equal(idx$empty, empty$url)
  expect_equal(idx$failed, "http://127.0.0.1:9")
  expect_equal(rownames(idx$packages), "glue")
})

test_that("file names from an index are only a plain package name and version", {
  idx <- matrix(
    c("../evil", "1.0", "ok", "1.0/../../x", "fine", "1.2.3.9000"),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("Package", "Version"))
  )
  expect_null(get_package_filename("../evil", idx))
  expect_null(get_package_filename("ok", idx))
  expect_equal(get_package_filename("fine", idx), "fine_1.2.3.9000.tgz")
  expect_equal(
    is_valid_package_name(c("R6", "a", "data.table", "my_pkg", "2x", "x.")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("a transitive dependency no repository has is a warning naming it", {
  local_isolated_cache()
  repo <- local_fixture_repo(list(
    alpha = c(Version = "1.0", Imports = "beta, gamma, stats"),
    gamma = c(Version = "1.0")
  ))
  out <- withr::local_tempdir()
  expect_warning(
    suppressMessages(download_webr_packages("alpha", out, "4.6", repo$url)),
    "beta"
  )
  expect_true(fs::file_exists(fs::path(out, "alpha_1.0.tgz")))
  expect_true(fs::file_exists(fs::path(out, "gamma_1.0.tgz")))
})

test_that("a requested package no repository has is an error before anything downloads", {
  local_isolated_cache()
  repo <- local_fixture_repo(list(alpha = c(Version = "1.0")))
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(download_webr_packages(c("alpha", "nopkg"), out, "4.6", repo$url)),
    "nopkg"
  )
  expect_false(fs::file_exists(fs::path(out, "alpha_1.0.tgz")))
})

test_that("a missing or corrupt binary of a requested package is an error that says why", {
  local_isolated_cache()
  repo <- local_fixture_repo(
    list(alpha = c(Version = "1.0"), beta = c(Version = "1.0")),
    missing = "alpha",
    corrupt = "beta"
  )
  out <- withr::local_tempdir()
  expect_error(suppressMessages(download_webr_packages("alpha", out, "4.6", repo$url)), "404")
  expect_error(suppressMessages(download_webr_packages("beta", out, "4.6", repo$url)), "checksum")
  expect_false(fs::file_exists(fs::path(out, "beta_1.0.tgz")))
})

test_that("a file already in place is reused only when its checksum matches", {
  local_isolated_cache()
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "0")
  repo <- local_fixture_repo(list(alpha = c(Version = "1.0")))
  out <- withr::local_tempdir()
  suppressMessages(download_webr_packages("alpha", out, "4.6", repo$url))
  good <- unname(tools::md5sum(fs::path(out, "alpha_1.0.tgz")))

  writeLines("truncated", fs::path(out, "alpha_1.0.tgz"))
  res <- suppressMessages(download_webr_packages("alpha", out, "4.6", repo$url))
  expect_equal(unname(res$status[["alpha"]]), "downloaded")
  expect_equal(unname(tools::md5sum(fs::path(out, "alpha_1.0.tgz"))), good)

  res <- suppressMessages(download_webr_packages("alpha", out, "4.6", repo$url))
  expect_equal(unname(res$status[["alpha"]]), "present")
})

test_that("the cache helper ignores a caller's WEBRARIAN_PACKAGE_CACHE", {
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "0")
  local_isolated_cache()
  expect_true(webr_package_cache_enabled())
})

test_that("the summary counts downloads, cache hits and files already present separately", {
  local_isolated_cache()
  repo <- local_fixture_repo(list(alpha = c(Version = "1.0")))
  first <- testthat::capture_messages(download_webr_packages(
    "alpha",
    withr::local_tempdir(),
    "4.6",
    repo$url
  ))
  expect_true(any(grepl("1 downloaded, 0 from the cache, 0 already present", first, fixed = TRUE)))
  second <- testthat::capture_messages(download_webr_packages(
    "alpha",
    withr::local_tempdir(),
    "4.6",
    repo$url
  ))
  expect_true(any(grepl("0 downloaded, 1 from the cache, 0 already present", second, fixed = TRUE)))
})

test_that("the local index is valid DCF, is gzipped too, and lists what is present", {
  dir <- withr::local_tempdir()
  rows <- matrix(
    c("B", "2.0", NA, "A", "1.0", "x (>= 1),\n    y"),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("Package", "Version", "Imports"))
  )
  write_repo_index(dir, rows)

  txt <- read.dcf(fs::path(dir, "PACKAGES"))
  expect_equal(unname(txt[, "Package"]), c("A", "B"))
  expect_equal(unname(txt[1, "Imports"]), "x (>= 1), y")
  expect_equal(unname(read.dcf(gzfile(fs::path(dir, "PACKAGES.gz")))[, "Package"]), c("A", "B"))
  expect_equal(rownames(readRDS(fs::path(dir, "PACKAGES.rds"))), c("A", "B"))
})

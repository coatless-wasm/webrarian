# Tests for R/assets.R — OFFLINE parts only.
# We never download; the cache is redirected to a temp dir via R_USER_CACHE_DIR
# so tools::R_user_dir("webrarian", "cache") points somewhere disposable.

test_that("webr_assets_version() honors WEBRARIAN_WEBR_VERSION env var", {
  withr::local_envvar(WEBRARIAN_WEBR_VERSION = "9.9.9")
  expect_identical(webr_assets_version(), "9.9.9")
})

test_that("webr_assets_version() falls back to default_webr_version()", {
  withr::local_envvar(WEBRARIAN_WEBR_VERSION = "")
  expect_identical(webr_assets_version(), default_webr_version())
})

test_that("webr_cache_path() sits under the redirected R_user_dir cache", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  path <- webr_cache_path()
  # R_user_dir inserts an "R/" component, so the path lives under cache_root.
  expect_true(startsWith(fs::path(path), fs::path(cache_root)))
  expect_identical(fs::path_file(path), "webrarian")
})

test_that("webr_assets_dir() is under the cache and carries the version", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  dir <- webr_assets_dir("0.5.8")
  expect_identical(
    fs::path(webr_cache_path()),
    fs::path(fs::path_dir(dir))
  )
  expect_identical(fs::path_file(dir), "webr-0.5.8")
})

test_that("webr_packages_cache_dir() lives under the cache, optionally versioned", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  base <- webr_packages_cache_dir()
  expect_identical(fs::path(fs::path_dir(base)), fs::path(webr_cache_path()))
  expect_identical(fs::path_file(base), "packages")

  versioned <- webr_packages_cache_dir("4.5")
  expect_identical(fs::path_file(versioned), "4.5")
  expect_identical(fs::path(fs::path_dir(versioned)), fs::path(base))
})

test_that("webr_assets_list() returns empty character() when cache is absent", {
  cache_root <- withr::local_tempdir()
  # Point at a subdir that does not exist yet so webr_cache_path() is missing.
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(cache_root, "nope"))

  expect_false(fs::dir_exists(webr_cache_path()))
  expect_identical(webr_assets_list(), character())
})

test_that("webr_assets_list() returns empty character() for an empty cache dir", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  fs::dir_create(webr_cache_path())
  expect_identical(webr_assets_list(), character())
})

test_that("webr_assets_list() finds and sorts hand-created version dirs", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  for (v in c("0.5.7", "0.5.8")) {
    fs::dir_create(webr_assets_dir(v))
    writeLines(v, fs::path(webr_assets_dir(v), ".webrarian-complete"))
  }

  result <- webr_assets_list()
  expect_setequal(result, c("0.5.7", "0.5.8"))
  # Descending order: newest first.
  expect_identical(result[1], "0.5.8")
})

test_that("webr_assets_remove(NULL) is a no-op returning empty character()", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  expect_identical(webr_assets_remove(NULL), character())
  expect_identical(webr_assets_remove(character()), character())
})

test_that("webr_assets_remove() deletes a hand-created version dir", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  dir <- webr_assets_dir("0.5.8")
  ensure_dir(dir)
  writeLines("x", fs::path(dir, "marker.txt"))
  expect_true(fs::dir_exists(dir))

  removed <- suppressMessages(webr_assets_remove("0.5.8"))
  expect_identical(removed, "0.5.8")
  expect_false(fs::dir_exists(dir))
})

test_that("webr_assets_remove() warns and returns empty for missing versions", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  removed <- suppressMessages(webr_assets_remove("1.2.3"))
  expect_identical(removed, character())
})

test_that("webr_cache_info() reports non-existent cache as exists = FALSE", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = fs::path(cache_root, "nope"))

  info <- suppressMessages(webr_cache_info())
  expect_false(info$exists)
  expect_identical(info$versions, character())
  expect_identical(info$total_size, 0)
})

test_that("webr_cache_info() reports hand-created versions and exists = TRUE", {
  cache_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_root)

  dir <- webr_assets_dir("0.5.8")
  ensure_dir(dir)
  writeLines("hello", fs::path(dir, "file.txt"))
  writeLines("0.5.8", fs::path(dir, ".webrarian-complete"))

  info <- suppressMessages(webr_cache_info())
  expect_true(info$exists)
  expect_true("0.5.8" %in% info$versions)
  expect_gt(info$total_size, 0)
  expect_identical(fs::path(info$path), fs::path(webr_cache_path()))
})

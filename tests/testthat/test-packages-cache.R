# Tests for the webR package download cache (R/assets.R) and the way
# R/build.R's download path consumes it.
#
# Everything here is OFFLINE: the cache is redirected to a temp dir via
# R_USER_CACHE_DIR, and network calls are replaced with mocked bindings.

# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------

local_cache_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  # WEBRARIAN_PACKAGE_CACHE is an on/off switch; the caller's value must not
  # decide whether these tests hit the cache.
  withr::local_envvar(R_USER_CACHE_DIR = root, WEBRARIAN_PACKAGE_CACHE = NA, .local_envir = env)
  # A stale option from another test must not leak into this one.
  withr::local_options(webrarian.package_cache = NULL, .local_envir = env)
  root
}

write_tgz <- function(path, contents = "package bytes") {
  writeBin(charToRaw(contents), path)
  path
}

# --------------------------------------------------------------------------
# webr_package_cache_enabled()
# --------------------------------------------------------------------------

test_that("the package cache is on by default", {
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = NA)
  withr::local_options(webrarian.package_cache = NULL)
  expect_true(webr_package_cache_enabled())
})

test_that("WEBRARIAN_PACKAGE_CACHE turns the cache off", {
  withr::local_options(webrarian.package_cache = NULL)
  for (value in c("0", "false", "FALSE", "no", "off", " Off ")) {
    withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = value)
    expect_false(webr_package_cache_enabled(), label = value)
  }
})

test_that("a truthy WEBRARIAN_PACKAGE_CACHE keeps the cache on", {
  withr::local_options(webrarian.package_cache = NULL)
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "1")
  expect_true(webr_package_cache_enabled())
})

test_that("the webrarian.package_cache option wins over the env var", {
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "1")
  withr::local_options(webrarian.package_cache = FALSE)
  expect_false(webr_package_cache_enabled())

  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "0")
  withr::local_options(webrarian.package_cache = TRUE)
  expect_true(webr_package_cache_enabled())
})

# --------------------------------------------------------------------------
# put / get round trip
# --------------------------------------------------------------------------

test_that("a cache miss returns NULL", {
  local_cache_root()
  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
})

test_that("put stores the file under the versioned cache dir", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))

  dest <- webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  expect_true(fs::file_exists(dest))
  expect_identical(
    fs::path(fs::path_dir(dest)),
    fs::path(webr_packages_cache_dir("4.5"))
  )
  expect_identical(unname(tools::md5sum(dest)), unname(tools::md5sum(src)))
})

test_that("put leaves no .part temp files behind", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))

  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  leftovers <- fs::dir_ls(
    webr_packages_cache_dir("4.5"),
    all = TRUE,
    type = "file",
    glob = "*.part"
  )
  expect_length(leftovers, 0)
})

test_that("get serves an entry verified against the upstream MD5", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  md5 <- unname(tools::md5sum(src))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  hit <- webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", md5)

  expect_false(is.null(hit))
  expect_identical(unname(tools::md5sum(hit)), md5)
})

test_that("get serves an entry using the recorded digest when upstream has none", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  # NA_character_ mimics a repository whose PACKAGES.rds omits MD5sum.
  hit <- webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_)

  expect_false(is.null(hit))
  expect_identical(unname(tools::md5sum(hit)), unname(tools::md5sum(src)))
})

test_that("the cache index records the algorithm and digest", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  index <- jsonlite::read_json(webr_package_cache_index_path("4.5"))
  expect_identical(index[["dplyr_1.1.4.tgz"]]$algo, "md5")
  expect_identical(
    index[["dplyr_1.1.4.tgz"]]$digest,
    unname(tools::md5sum(src))
  )
})

test_that("the cache is namespaced by R version", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  expect_null(webr_package_cache_get("4.4", "dplyr_1.1.4.tgz", NA_character_))
  expect_false(is.null(
    webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_)
  ))
})

# --------------------------------------------------------------------------
# integrity: corrupt entries are rejected, never served
# --------------------------------------------------------------------------

test_that("a truncated cache entry is rejected and evicted", {
  local_cache_root()
  src <- write_tgz(
    fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"),
    strrep("x", 512)
  )
  cached <- webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  # Simulate an interrupted write: the file is there, but half of it.
  writeBin(charToRaw(strrep("x", 16)), cached)

  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
  expect_false(fs::file_exists(cached))
  expect_null(webr_package_cache_index_get("4.5", "dplyr_1.1.4.tgz"))
})

test_that("an entry whose bytes disagree with the upstream MD5 is rejected", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  wrong_md5 <- strrep("0", 32)
  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", wrong_md5))
})

test_that("an entry with no recorded digest is not served", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  cached <- webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  fs::file_delete(webr_package_cache_index_path("4.5"))

  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
  # The bytes are kept (the index, not the file, is what went missing).
  expect_true(fs::file_exists(cached))
})

test_that("a damaged index is treated as empty rather than trusted", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  writeLines("{not json", webr_package_cache_index_path("4.5"))

  expect_identical(webr_package_cache_index_read("4.5"), list())
  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
})

test_that("an index entry from an unknown digest algorithm fails closed", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  jsonlite::write_json(
    list("dplyr_1.1.4.tgz" = list(algo = "blake3", digest = "deadbeef")),
    webr_package_cache_index_path("4.5"),
    auto_unbox = TRUE
  )

  expect_null(webr_package_cache_index_get("4.5", "dplyr_1.1.4.tgz"))
  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
})

# --------------------------------------------------------------------------
# escape hatch
# --------------------------------------------------------------------------

test_that("disabling the cache makes get miss and put a no-op", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  withr::local_options(webrarian.package_cache = FALSE)
  expect_null(webr_package_cache_get("4.5", "dplyr_1.1.4.tgz", NA_character_))
  expect_null(webr_package_cache_put("4.5", "other_1.0.0.tgz", src))
  expect_false(fs::file_exists(webr_package_cache_file("4.5", "other_1.0.0.tgz")))
})

# --------------------------------------------------------------------------
# cache entry names come from a remote index, so they must stay inside the dir
# --------------------------------------------------------------------------

test_that("is_safe_cache_filename rejects anything but a plain base name", {
  expect_true(is_safe_cache_filename("dplyr_1.1.4.tgz"))

  expect_false(is_safe_cache_filename("../escape.tgz"))
  expect_false(is_safe_cache_filename("nested/pkg.tgz"))
  expect_false(is_safe_cache_filename("nested\\pkg.tgz"))
  expect_false(is_safe_cache_filename("/abs/pkg.tgz"))
  expect_false(is_safe_cache_filename(".."))
  expect_false(is_safe_cache_filename(""))
  expect_false(is_safe_cache_filename(NA_character_))
  expect_false(is_safe_cache_filename(c("a.tgz", "b.tgz")))
})

test_that("a traversing file name is neither stored nor served", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "evil.tgz"))

  expect_null(webr_package_cache_put("4.5", "../evil.tgz", src))
  expect_null(webr_package_cache_get("4.5", "../evil.tgz", NA_character_))
  expect_false(fs::file_exists(fs::path(webr_packages_cache_dir(), "evil.tgz")))
})

# --------------------------------------------------------------------------
# get_package_md5()
# --------------------------------------------------------------------------

test_that("get_package_md5 reads the MD5sum column from the index", {
  avail <- matrix(
    c("dplyr", "1.1.4", "ABCDEF0123456789abcdef0123456789"),
    nrow = 1,
    dimnames = list("dplyr", c("Package", "Version", "MD5sum"))
  )
  expect_identical(
    get_package_md5("dplyr", avail),
    "abcdef0123456789abcdef0123456789"
  )
})

test_that("get_package_md5 returns NA when the repo publishes no checksum", {
  no_col <- matrix(
    c("dplyr", "1.1.4"),
    nrow = 1,
    dimnames = list("dplyr", c("Package", "Version"))
  )
  expect_identical(get_package_md5("dplyr", no_col), NA_character_)

  na_cell <- matrix(
    c("dplyr", "1.1.4", NA_character_),
    nrow = 1,
    dimnames = list("dplyr", c("Package", "Version", "MD5sum"))
  )
  expect_identical(get_package_md5("dplyr", na_cell), NA_character_)

  expect_identical(get_package_md5("dplyr", NULL), NA_character_)
  expect_identical(get_package_md5("missing", na_cell), NA_character_)
})

# --------------------------------------------------------------------------
# download_webr_packages(): miss populates, hit reuses, corruption re-downloads
# --------------------------------------------------------------------------

# Fake repository: one package, served from memory, counting every request.
local_fake_repo <- function(env = parent.frame()) {
  state <- new.env(parent = emptyenv())

  state$payload <- charToRaw(strrep("dplyr wasm bytes ", 64))
  payload_file <- tempfile()
  writeBin(state$payload, payload_file)
  payload_md5 <- unname(tools::md5sum(payload_file))
  unlink(payload_file)

  state$index <- matrix(
    c("dplyr", "1.1.4", NA_character_, NA_character_, NA_character_, "MIT", payload_md5),
    nrow = 1,
    dimnames = list(
      "dplyr",
      c("Package", "Version", "Depends", "Imports", "Suggests", "License", "MD5sum")
    )
  )
  state$index_requests <- 0L
  state$pkg_requests <- 0L

  testthat::local_mocked_bindings(
    webr_request = function(url, timeout = 60, max_tries = 3) list(url = url),
    .env = env
  )
  # httr2 reaches the network only through these two calls in the download path.
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      if (grepl("PACKAGES\\.rds$", req$url)) {
        state$index_requests <- state$index_requests + 1L
        list(kind = "index")
      } else {
        state$pkg_requests <- state$pkg_requests + 1L
        list(kind = "pkg")
      }
    },
    resp_body_raw = function(resp, ...) {
      if (identical(resp$kind, "index")) {
        f <- tempfile()
        on.exit(unlink(f), add = TRUE)
        saveRDS(state$index, f)
        readBin(f, "raw", file.size(f))
      } else {
        state$payload
      }
    },
    .env = env,
    .package = "httr2"
  )

  state
}

test_that("first build downloads, second build serves from the cache", {
  local_cache_root()
  repo <- local_fake_repo()

  out1 <- withr::local_tempdir()
  suppressMessages(download_webr_packages(
    "dplyr",
    out1,
    "4.5",
    "https://repo.r-wasm.org",
    include_deps = FALSE
  ))

  expect_true(fs::file_exists(fs::path(out1, "dplyr_1.1.4.tgz")))
  expect_identical(repo$pkg_requests, 1L)
  # The download populated the cache.
  expect_true(fs::file_exists(webr_package_cache_file("4.5", "dplyr_1.1.4.tgz")))

  # Second build into a *fresh* output dir, exactly like bind(clean = TRUE).
  out2 <- withr::local_tempdir()
  suppressMessages(download_webr_packages(
    "dplyr",
    out2,
    "4.5",
    "https://repo.r-wasm.org",
    include_deps = FALSE
  ))

  expect_true(fs::file_exists(fs::path(out2, "dplyr_1.1.4.tgz")))
  # No second package download: the counter did not move.
  expect_identical(repo$pkg_requests, 1L)
  expect_identical(
    unname(tools::md5sum(fs::path(out2, "dplyr_1.1.4.tgz"))),
    unname(tools::md5sum(fs::path(out1, "dplyr_1.1.4.tgz")))
  )
})

test_that("a corrupt cache entry is re-downloaded rather than served", {
  local_cache_root()
  repo <- local_fake_repo()

  out1 <- withr::local_tempdir()
  suppressMessages(download_webr_packages(
    "dplyr",
    out1,
    "4.5",
    "https://repo.r-wasm.org",
    include_deps = FALSE
  ))
  expect_identical(repo$pkg_requests, 1L)

  # Truncate the cached entry, as an interrupted build would.
  cached <- webr_package_cache_file("4.5", "dplyr_1.1.4.tgz")
  writeBin(charToRaw("trunc"), cached)

  out2 <- withr::local_tempdir()
  suppressMessages(download_webr_packages(
    "dplyr",
    out2,
    "4.5",
    "https://repo.r-wasm.org",
    include_deps = FALSE
  ))

  expect_identical(repo$pkg_requests, 2L)
  built <- fs::path(out2, "dplyr_1.1.4.tgz")
  expect_true(fs::file_exists(built))
  # The bundle got the real bytes, not the truncated ones.
  expect_identical(as.integer(fs::file_size(built)), length(repo$payload))
})

test_that("disabling the cache forces a download every build", {
  local_cache_root()
  repo <- local_fake_repo()
  withr::local_envvar(WEBRARIAN_PACKAGE_CACHE = "0")

  for (i in 1:2) {
    out <- withr::local_tempdir()
    suppressMessages(download_webr_packages(
      "dplyr",
      out,
      "4.5",
      "https://repo.r-wasm.org",
      include_deps = FALSE
    ))
    expect_true(fs::file_exists(fs::path(out, "dplyr_1.1.4.tgz")))
  }

  expect_identical(repo$pkg_requests, 2L)
  expect_false(fs::dir_exists(webr_packages_cache_dir("4.5")))
})

test_that("a download whose checksum disagrees with the index is a build failure", {
  local_cache_root()
  repo <- local_fake_repo()
  # Advertise a digest the served bytes cannot match.
  repo$index[1, "MD5sum"] <- strrep("0", 32)

  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(download_webr_packages(
      "dplyr",
      out,
      "4.5",
      "https://repo.r-wasm.org",
      include_deps = FALSE
    )),
    "Could not obtain requested package"
  )
  expect_false(fs::file_exists(fs::path(out, "dplyr_1.1.4.tgz")))
  # Nothing bad was cached.
  expect_false(fs::file_exists(webr_package_cache_file("4.5", "dplyr_1.1.4.tgz")))
})

# --------------------------------------------------------------------------
# reporting and clearing
# --------------------------------------------------------------------------

test_that("webr_cache_info() reports real package cache contents", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"), strrep("z", 300))
  webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)

  info <- suppressMessages(webr_cache_info())

  expect_identical(nrow(info$packages), 1L)
  expect_identical(info$packages$r_version, "4.5")
  expect_identical(info$packages$packages, 1L)
  expect_gt(info$packages$size, 0)
})

test_that("webr_cache_info() reports an empty package cache", {
  local_cache_root()
  expect_identical(nrow(webr_package_cache_summary()), 0L)

  info <- suppressMessages(webr_cache_info())
  expect_identical(nrow(info$packages), 0L)
})

test_that("webr_cache_clear() removes the package cache too", {
  local_cache_root()
  src <- write_tgz(fs::path(withr::local_tempdir(), "dplyr_1.1.4.tgz"))
  cached <- webr_package_cache_put("4.5", "dplyr_1.1.4.tgz", src)
  expect_true(fs::file_exists(cached))

  suppressMessages(webr_cache_clear())

  expect_false(fs::file_exists(cached))
  expect_false(fs::dir_exists(webr_packages_cache_dir()))
  expect_identical(nrow(webr_package_cache_summary()), 0L)
})

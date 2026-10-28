# The webR engine cache: only complete downloads are listed or
# reused, a download is verified and swapped in whole, outdated engines are
# pruned, and nothing is written through an old symlinked install.

local_engine_cache <- function(env = parent.frame()) {
  # A cache path containing "webr-": the old listing matched it everywhere.
  root <- fs::path(withr::local_tempdir(.local_envir = env), "webr-cache-test")
  fs::dir_create(root)
  withr::local_envvar(R_USER_CACHE_DIR = root, WEBRARIAN_PACKAGE_CACHE = NA, .local_envir = env)
  webr_cache_path()
}

# A finished engine download, last used at `used`.
complete_engine <- function(version, used = Sys.time()) {
  dir <- webr_assets_dir(version)
  fs::dir_create(dir)
  writeLines("wasm", fs::path(dir, "R.wasm"))
  writeLines(version, fs::path(dir, ".webrarian-complete"))
  Sys.setFileTime(fs::path(dir, ".webrarian-complete"), used)
  dir
}

days_ago <- function(n) Sys.time() - n * 24 * 3600

# One tar entry: a ustar header, then `body` padded to whole blocks.
tar_entry <- function(name, type, body) {
  header <- raw(512L)
  header[seq_len(nchar(name, "bytes"))] <- charToRaw(name)
  header[101:108] <- c(charToRaw("0000644"), as.raw(0L))
  header[125:136] <- c(charToRaw(sprintf("%011o", length(body))), as.raw(0L))
  header[157] <- charToRaw(type)
  header[258:265] <- c(charToRaw("ustar"), as.raw(0L), charToRaw("00"))
  header[149:156] <- charToRaw(strrep(" ", 8L))
  header[149:156] <- c(charToRaw(sprintf("%06o", sum(as.integer(header)))), as.raw(c(0L, 0x20)))
  c(header, body, raw(-length(body) %% 512L))
}

# A pax record, "<length> <key>=<value>\n", its length counting its own digits.
pax_record <- function(key, value) {
  n <- nchar(key, "bytes") + length(value) + 3L
  len <- n + nchar(n + nchar(n))
  c(charToRaw(paste0(len, " ", key, "=")), value, charToRaw("\n"))
}

# A gzipped tar of `files` (name = contents) laid out the way macOS's tar
# writes webR's release: a pax header before each entry with a binary
# com.apple.provenance xattr.
write_mac_tarball <- function(path, files) {
  provenance <- as.raw(c(0x01, 0x00, 0x03, 0x65, 0xc5, 0x4d, 0x75, 0xa3, 0xc8, 0xb9, 0xd9))
  con <- gzfile(path, "wb")
  on.exit(close(con))
  for (name in names(files)) {
    xattr <- pax_record("SCHILY.xattr.com.apple.provenance", provenance)
    writeBin(tar_entry("PaxHeader", "x", xattr), con)
    writeBin(tar_entry(name, "0", charToRaw(files[[name]])), con)
  }
  writeBin(raw(1024L), con)
}

# A release tarball, served through a mocked httr2; fail = TRUE drops the
# connection mid-download, and mac = TRUE tars it the way macOS's tar does.
local_fake_release <- function(
  version = "0.6.0",
  fail = FALSE,
  mac = FALSE,
  env = parent.frame()
) {
  base <- withr::local_tempdir(.local_envir = env)
  release <- paste0("webr-", version)
  files <- c(
    "R.wasm",
    "R.js",
    "webr-worker.js",
    "webr.mjs",
    "libRblas.so",
    "vfs/usr.data",
    "repl.js"
  )
  tarball <- fs::path(base, "webr.tar.gz")
  if (mac) {
    contents <- paste0(version, " ", files, "\n")
    write_mac_tarball(tarball, stats::setNames(contents, file.path(release, files)))
  } else {
    fs::dir_create(fs::path(base, release, "vfs"))
    for (f in files) {
      writeLines(paste(version, f), fs::path(base, release, f))
    }
    withr::with_dir(
      base,
      utils::tar(tarball, release, compression = "gzip", tar = "internal")
    )
  }
  testthat::local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      if (fail) {
        writeLines("partial", path)
        stop("connection reset by peer")
      }
      fs::file_copy(tarball, path, overwrite = TRUE)
      structure(list(status_code = 200L), class = "httr2_response")
    },
    resp_status = function(resp) 200L,
    .package = "httr2",
    .env = env
  )
  tarball
}

local_pinned_checksums <- function(tarball, env = parent.frame()) {
  digest <- release_digest(tarball)
  sums <- list(sha256 = "0", md5 = "0")
  sums[[digest$algo]] <- digest$value
  testthat::local_mocked_bindings(webr_release_checksums = function(version) sums, .env = env)
}

staging_dirs <- function(cache) {
  fs::dir_ls(cache, all = TRUE, type = "directory", regexp = "/\\.staging-webr-[^/]+$")
}

test_that("a verified download is swapped in whole, engine files only", {
  cache <- local_engine_cache()
  local_pinned_checksums(local_fake_release())
  dir <- suppressMessages(webr_assets_download("0.6.0"))
  expect_true(cache_is_complete(dir))
  expect_true(fs::file_exists(fs::path(dir, "R.wasm")))
  expect_false(fs::file_exists(fs::path(dir, "repl.js")))
  expect_identical(webr_assets_list(), "0.6.0")
  expect_length(staging_dirs(cache), 0L)
})

test_that("a release tarred on a Mac unpacks, binary file attributes and all", {
  # TAR=internal makes untar() read the way it does on Windows without
  # tar.exe, so going back to untar() fails here.
  local_engine_cache()
  withr::local_envvar(TAR = "internal")
  local_pinned_checksums(local_fake_release(mac = TRUE))
  expect_no_warning(dir <- suppressMessages(webr_assets_download("0.6.0")))
  expect_identical(readLines(fs::path(dir, "R.wasm")), "0.6.0 R.wasm")
  expect_identical(readLines(fs::path(dir, "vfs", "usr.data")), "0.6.0 vfs/usr.data")
  expect_false(fs::file_exists(fs::path(dir, "repl.js")))
})

test_that("a download that does not match its pinned checksum caches nothing", {
  cache <- local_engine_cache()
  local_fake_release()
  local_mocked_bindings(webr_release_checksums = function(version) list(sha256 = "0", md5 = "0"))
  expect_error(suppressMessages(webr_assets_download("0.6.0")), class = "webrarian_error_checksum")
  expect_false(fs::dir_exists(webr_assets_dir("0.6.0")))
  expect_identical(webr_assets_list(), character())
  expect_length(staging_dirs(cache), 0L)
})

test_that("an interrupted download leaves nothing listed or reused, and the next call downloads cleanly", {
  cache <- local_engine_cache()
  local_fake_release(fail = TRUE)
  expect_error(suppressMessages(webr_assets_download("0.6.0")), "connection reset")
  expect_identical(webr_assets_list(), character())
  expect_false(cache_is_complete(webr_assets_dir("0.6.0")))
  expect_length(staging_dirs(cache), 0L)

  local_pinned_checksums(local_fake_release())
  dir <- suppressMessages(webr_assets_ensure("0.6.0"))
  expect_true(cache_is_complete(dir))
})

test_that("an old symlinked engine is replaced without writing into its target", {
  skip_on_os("windows")
  cache <- local_engine_cache()
  users_build <- withr::local_tempdir()
  writeLines("mine", fs::path(users_build, "user-build.txt"))
  fs::dir_create(cache)
  fs::link_create(users_build, webr_assets_dir("0.6.0"))
  expect_false(cache_is_complete(webr_assets_dir("0.6.0")))

  local_pinned_checksums(local_fake_release())
  dir <- suppressMessages(webr_assets_ensure("0.6.0"))
  expect_false(fs::link_exists(dir))
  expect_true(cache_is_complete(dir))
  expect_identical(list.files(users_build, all.files = TRUE, no.. = TRUE), "user-build.txt")
})

test_that("the listing ignores incomplete directories and paths that merely contain webr-", {
  cache <- local_engine_cache()
  complete_engine("0.6.0")
  fs::dir_create(webr_assets_dir("0.6.1"))
  fs::dir_create(fs::path(cache, "packages", "4.6"))
  expect_identical(webr_assets_list(), "0.6.0")
})

test_that("after a download, only the three most recently used engines are kept", {
  cache <- local_engine_cache()
  complete_engine("0.4.2", days_ago(5))
  complete_engine("0.5.9", days_ago(4))
  complete_engine("0.5.8", days_ago(3))
  complete_engine("0.6.1", days_ago(2))
  fs::dir_create(webr_assets_dir("0.6.2")) # a download that never finished
  fs::dir_create(fs::path(cache, ".staging-webr-0.6.0-999999"))
  Sys.setFileTime(fs::path(cache, ".staging-webr-0.6.0-999999"), days_ago(3))
  local_pinned_checksums(local_fake_release())
  suppressMessages(webr_assets_download("0.6.0"))
  expect_identical(webr_assets_list(), c("0.6.1", "0.6.0", "0.5.8"))
  expect_false(fs::dir_exists(webr_assets_dir("0.6.2")))
  expect_length(staging_dirs(cache), 0L)
})

test_that("a reused engine counts as used", {
  cache <- local_engine_cache()
  complete_engine("0.4.2", days_ago(9))
  complete_engine("0.5.9", days_ago(4))
  complete_engine("0.5.8", days_ago(3))
  suppressMessages(webr_assets_ensure("0.4.2"))
  local_pinned_checksums(local_fake_release())
  suppressMessages(webr_assets_download("0.6.0"))
  expect_identical(webr_assets_list(), c("0.6.0", "0.5.8", "0.4.2"))
})

test_that("after a download, package caches for R lines no kept or offered engine uses are pruned", {
  cache <- local_engine_cache()
  for (line in c("4.4", "4.6")) {
    fs::dir_create(fs::path(cache, "packages", line))
    writeLines("tgz", fs::path(cache, "packages", line, "glue_1.8.0.tgz"))
  }
  local_pinned_checksums(local_fake_release())
  suppressMessages(webr_assets_download("0.6.0"))
  expect_identical(as.character(fs::path_file(fs::dir_ls(fs::path(cache, "packages")))), "4.6")
  expect_true(fs::file_exists(fs::path(cache, "packages", "4.6", "glue_1.8.0.tgz")))

  # A kept engine this webrarian does not list (an older webrarian's 0.5.8,
  # R 4.5) may still build with any cached line, so then none is removed.
  fs::dir_create(fs::path(cache, "packages", "4.5"))
  complete_engine("0.5.8", days_ago(1))
  suppressMessages(webr_cache_prune(keep = "0.6.0"))
  expect_setequal(
    as.character(fs::path_file(fs::dir_ls(fs::path(cache, "packages")))),
    c("4.5", "4.6")
  )
})

test_that("webr_cache_clear() removes one engine version or the whole cache", {
  expect_identical(formals_text(webr_cache_clear), c(versions = "NULL"))
  cache <- local_engine_cache()
  complete_engine("0.6.0")
  fs::dir_create(fs::path(cache, "packages", "4.6"))

  removed <- suppressMessages(webr_cache_clear("0.6.0"))
  expect_identical(fs::path_file(removed), "webr-0.6.0")
  expect_true(fs::dir_exists(fs::path(cache, "packages")))
  expect_error(webr_cache_clear("../../etc"), "Invalid webR version")

  suppressMessages(webr_cache_clear())
  expect_false(fs::dir_exists(cache))
})

test_that("webr_cache_clear() returns no paths when no requested version was cached", {
  local_engine_cache()
  complete_engine("0.6.0")
  expect_identical(suppressMessages(webr_cache_clear("0.5.9")), character())
  expect_true(cache_is_complete(webr_assets_dir("0.6.0")))
})

test_that("clearing the whole cache asks first in an interactive session", {
  cache <- local_engine_cache()
  complete_engine("0.6.0")
  withr::local_options(rlang_interactive = TRUE)

  local_mocked_bindings(ask_yes_no = function(question) FALSE)
  expect_identical(suppressMessages(webr_cache_clear()), character())
  expect_true(cache_is_complete(webr_assets_dir("0.6.0")))

  local_mocked_bindings(ask_yes_no = function(question) TRUE)
  suppressMessages(webr_cache_clear())
  expect_false(fs::dir_exists(cache))
})

test_that("every tested webR version pins its release tarball's checksums", {
  for (entry in webr_versions_table()$versions) {
    expect_match(entry$tarball_sha256 %||% "", "^[0-9a-f]{64}$", info = entry$version)
    expect_match(entry$tarball_md5 %||% "", "^[0-9a-f]{32}$", info = entry$version)
  }
})

test_that("of the webr_* family only webr_cache_info() and webr_cache_clear() are exported", {
  expect_setequal(
    grep("^webr_", namespace_exports(), value = TRUE),
    c("webr_cache_info", "webr_cache_clear")
  )
  expect_false(exists(
    "webr_assets_install_link",
    envir = asNamespace("webrarian"),
    inherits = FALSE
  ))
  expect_false(exists(
    "webr_assets_install_copy",
    envir = asNamespace("webrarian"),
    inherits = FALSE
  ))
})

test_that("?webrarian documents the package options", {
  rd <- test_path("..", "..", "man", "webrarian-package.Rd")
  skip_if_not(file.exists(rd), "man/ is not in this tree")
  text <- paste(readLines(rd, warn = FALSE), collapse = "\n")
  for (name in c(
    "R_USER_CACHE_DIR",
    "webrarian.package_cache",
    "WEBRARIAN_PACKAGE_CACHE",
    "WEBRARIAN_WEBR_VERSION"
  )) {
    expect_match(text, name, fixed = TRUE, info = name)
  }
})

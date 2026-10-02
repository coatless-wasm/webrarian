# collection_mirror() against a local fixture repository.

skip_if_not_installed("httpuv")

local_mirror_env <- function(repo, env = parent.frame()) {
  local_fake_engine(env = env)
  local_isolated_cache(env = env)
  testthat::local_mocked_bindings(default_repo_url = function() repo$url, .env = env)
}

mirror_contrib <- function(out) fs::path(out, "repo", "bin", "emscripten", "contrib", "4.6")

test_that("a selective mirror bundles packages and dependencies, an index, _headers, the kill-switch sw.js, share-links off and offline", {
  repo <- local_fixture_repo(list(
    alpha = c(Version = "1.0", Imports = "beta"),
    beta = c(Version = "1.0")
  ))
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")

  suppressMessages(collection_mirror(out, packages = "alpha", webr_version = "0.6.0"))

  expect_setequal(
    unname(read.dcf(fs::path(mirror_contrib(out), "PACKAGES"))[, "Package"]),
    c("alpha", "beta")
  )
  expect_true(fs::file_exists(fs::path(out, "_headers")))
  expect_true(fs::file_exists(fs::path(out, "webr", "v0.6.0", "R.wasm")))
  # A mirror has no service worker, so it ships the kill-switch one and
  # registers none.
  expect_match(
    paste(readLines(fs::path(out, "sw.js"), warn = FALSE), collapse = "\n"),
    "self.registration.unregister()",
    fixed = TRUE
  )
  expect_false(grepl(
    "serviceWorker",
    paste(readLines(fs::path(out, "index.html"), warn = FALSE), collapse = "\n"),
    fixed = TRUE
  ))
  cfg <- read_inline_viewer_config(paste(readLines(fs::path(out, "index.html")), collapse = "\n"))
  expect_equal(cfg[["share-links"]], "off")
  expect_equal(cfg[["engine-base-url"]], "./webr/v0.6.0/")
  # Always offline: its own ./repo and nothing else.
  expect_true(cfg$offline)
  expect_equal(cfg$packages[["repo-url"]], "./repo")
  expect_length(cfg$packages$repos, 0L)
})

test_that("a requested package that cannot be mirrored is an error, not a silent gap", {
  repo <- local_fixture_repo(
    list(alpha = c(Version = "1.0"), beta = c(Version = "1.0")),
    missing = "beta"
  )
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  expect_error(
    suppressMessages(collection_mirror(out, packages = c("alpha", "beta"), webr_version = "0.6.0")),
    "beta"
  )
  expect_error(
    suppressMessages(collection_mirror(out, packages = "notapkg", webr_version = "0.6.0")),
    "notapkg"
  )
})

test_that("a corrupt download is rejected, and a verified one is kept on resume", {
  bad <- local_fixture_repo(list(alpha = c(Version = "1.0")), corrupt = "alpha")
  local_mirror_env(bad)
  out <- fs::path(withr::local_tempdir(), "mirror")
  expect_error(
    suppressMessages(collection_mirror(out, packages = "alpha", webr_version = "0.6.0")),
    "checksum"
  )

  good <- local_fixture_repo(list(alpha = c(Version = "1.0")))
  local_mocked_bindings(default_repo_url = function() good$url)
  suppressMessages(collection_mirror(out, packages = "alpha", webr_version = "0.6.0"))
  msgs <- testthat::capture_messages(collection_mirror(
    out,
    packages = "alpha",
    webr_version = "0.6.0"
  ))
  expect_true(any(grepl("1 already present", msgs, fixed = TRUE)))
})

test_that("a full mirror covers every repository and indexes only the files it has", {
  a <- local_fixture_repo(
    list(pkga = c(Version = "1.0"), pkgb = c(Version = "1.0")),
    missing = "pkgb"
  )
  b <- local_fixture_repo(list(pkgc = c(Version = "1.0")))
  local_mirror_env(a)
  out <- fs::path(withr::local_tempdir(), "mirror")

  expect_warning(
    suppressMessages(collection_mirror(out, mode = "full", repos = b$url, webr_version = "0.6.0")),
    "pkgb"
  )
  expect_setequal(
    unname(read.dcf(fs::path(mirror_contrib(out), "PACKAGES"))[, "Package"]),
    c("pkga", "pkgc")
  )
})

test_that("an empty mirror still has a (empty) package index", {
  repo <- local_fixture_repo(list())
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  suppressMessages(collection_mirror(out, webr_version = "0.6.0"))
  expect_true(fs::file_exists(fs::path(mirror_contrib(out), "PACKAGES.rds")))
  expect_equal(nrow(readRDS(fs::path(mirror_contrib(out), "PACKAGES.rds"))), 0L)
})

test_that("mirror-config.json records a UTC timestamp", {
  repo <- local_fixture_repo(list())
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  suppressMessages(collection_mirror(out, webr_version = "0.6.0"))
  info <- jsonlite::read_json(fs::path(out, "mirror-config.json"))
  stamp <- as.POSIXct(info$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_lt(abs(as.numeric(difftime(stamp, Sys.time(), units = "mins"))), 5)
})

test_that("collection_mirror() takes the shared arguments", {
  expect_identical(
    formals_text(collection_mirror),
    c(
      dest = "",
      packages = "NULL",
      mode = "c(\"packages\", \"full\")",
      share_links = "\"off\"",
      webr_version = "NULL",
      repos = "NULL",
      favicon = "NULL"
    )
  )
})

test_that("packages mode is the default, bundles dependencies, and records no base_url", {
  repo <- local_fixture_repo(list(
    alpha = c(Version = "1.0", Imports = "beta"),
    beta = c(Version = "1.0")
  ))
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  mirror <- suppressMessages(collection_mirror(out, "alpha"))
  # [R] the mirror's path, not a build result.
  expect_identical(mirror, out)
  expect_setequal(
    unname(read.dcf(fs::path(mirror_contrib(out), "PACKAGES"))[, "Package"]),
    c("alpha", "beta")
  )
  info <- jsonlite::read_json(fs::path(out, "mirror-config.json"))
  expect_equal(info$mode, "packages")
  expect_false("base_url" %in% names(info))
  # The replacement body keeps the kill-switch worker.
  expect_match(
    paste(readLines(fs::path(out, "sw.js"), warn = FALSE), collapse = "\n"),
    "self.registration.unregister()",
    fixed = TRUE
  )
})

test_that("an unknown share-links mode or a missing dest is refused before anything is written", {
  out <- fs::path(withr::local_tempdir(), "mirror")
  expect_error(collection_mirror(out, share_links = "closed"), "share_links")
  expect_error(collection_mirror(c("a", "b")), "dest")
  expect_false(fs::dir_exists(out))
})

test_that("a mirror ships LICENSES for its engine and every package it carries", {
  repo <- local_fixture_repo(list(
    alpha = c(Version = "1.0", Imports = "beta", License = "MIT"),
    beta = c(Version = "1.0", License = "GPL-3")
  ))
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  suppressMessages(collection_mirror(out, "alpha"))

  lic <- fs::path(out, "LICENSES")
  expect_setequal(
    as.character(fs::path_file(fs::dir_ls(lic))),
    c("webrarian.md", "exlibris.md", "THIRD-PARTY-r.md", "webR.md", "PACKAGES.md", "index.html")
  )
  expect_match(
    paste(readLines(fs::path(lic, "webrarian.md")), collapse = "\n"),
    "generated this mirror",
    fixed = TRUE
  )
  pkgs <- paste(readLines(fs::path(lic, "PACKAGES.md")), collapse = "\n")
  expect_match(pkgs, "This mirror bundles 2 R packages", fixed = TRUE)
  expect_match(
    pkgs,
    sprintf("| beta | 1.0 | GPL-3 | <%s/src/contrib/beta_1.0.tar.gz> |", repo$url),
    fixed = TRUE
  )
  webr <- paste(readLines(fs::path(lic, "webR.md")), collapse = "\n")
  expect_match(webr, "This mirror runs webR 0.6.0", fixed = TRUE)
  expect_match(webr, "GNU GENERAL PUBLIC LICENSE", fixed = TRUE)
})

test_that("an empty mirror says it bundles no packages", {
  repo <- local_fixture_repo(list())
  local_mirror_env(repo)
  out <- fs::path(withr::local_tempdir(), "mirror")
  suppressMessages(collection_mirror(out))
  expect_match(
    paste(readLines(fs::path(out, "LICENSES", "PACKAGES.md")), collapse = "\n"),
    "This mirror bundles no R packages.",
    fixed = TRUE
  )
})

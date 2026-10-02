# diagnose_config() must report an unreadable/broken configuration as invalid.
# Previously the error handler mutated a local copy of `result`, so a broken
# config was silently reported as valid and diagnose_collection() never
# surfaced overall = "error".

test_that("diagnose_config reports valid = FALSE when the config cannot be read", {
  root <- withr::local_tempdir() # not a collection: collection_settings() errors
  result <- suppressMessages(diagnose_config(root))
  expect_false(result$valid)
  expect_gt(length(result$issues), 0)
})

test_that("diagnose_config reports valid = TRUE for a real collection", {
  withr::with_tempdir({
    suppressMessages(catalog())
    result <- suppressMessages(diagnose_config("."))
    expect_true(result$valid)
  })
})

# --- check_inventory() and diagnose_collection() -------------------

local_offline_endpoints <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_endpoint = function(url, name) list(ok = TRUE, status = 200L, time = 1, error = NULL),
    .env = env
  )
}

test_that("check_inventory looks in the collection's extra repositories", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  main <- local_fixture_repo(list(dplyr = c(Version = "1.1.4")))
  extra <- local_fixture_repo(list(paintr = c(Version = "0.0.1")))
  local_mocked_bindings(default_repo_url = function() main$url)
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("dplyr", "paintr"),
    "packages.repos" = list(extra$url)
  ))

  res <- suppressMessages(check_inventory(path = root))
  expect_equal(res$available, c(TRUE, TRUE))
  expect_match(res$repository[[2]], extra$url, fixed = TRUE)
})

test_that("check_inventory degrades to NA with one warning when no repository answers", {
  local_isolated_cache()
  local_mocked_bindings(default_repo_url = function() "http://127.0.0.1:9")
  expect_warning(res <- suppressMessages(check_inventory(c("dplyr", "ggplot2"))), "unknown")
  expect_equal(res$available, c(NA, NA))
  expect_equal(nrow(res), 2L)
})

test_that("the problem list adds notes but never marks an installable package unavailable", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  main <- local_fixture_repo(list(processx = c(Version = "3.9.0"), rJava = c(Version = "1.0")))
  local_mocked_bindings(default_repo_url = function() main$url)
  res <- suppressMessages(check_inventory(c("processx", "rJava")))
  expect_equal(res$available, c(TRUE, TRUE))
  expect_false(is.na(res$note[[2]]))
})

test_that("diagnose_collection reports an invalid config and still reaches its summary", {
  local_offline_endpoints()
  dir <- withr::local_tempdir()
  writeLines(c("files:", "  mount_point: /x"), fs::path(dir, "_webrarian.yml"))
  msgs <- testthat::capture_messages(res <- diagnose_collection(dir))
  expect_equal(res$overall, "error")
  expect_true(any(grepl("Some checks failed", msgs)))
})

test_that("diagnose_collection lists unknown keys as warnings", {
  local_offline_endpoints()
  root <- local_collection()
  cat("reple:\n  x: 1\n", file = fs::path(root, "_webrarian.yml"), append = TRUE)
  suppressMessages(res <- diagnose_collection(root))
  expect_equal(res$overall, "warning")
  expect_true(any(grepl("repl", unlist(res$config$issues))))
})

test_that("diagnose_config flags what would make bind() fail", {
  root <- local_collection()
  suppressMessages(settings_set(root, "webr.version" = "0.7.0"))
  expect_false(suppressMessages(diagnose_config(root))$valid)

  root <- local_collection()
  suppressMessages(settings_set(root, "repl.auto-run" = list("missing.R")))
  expect_false(suppressMessages(diagnose_config(root))$valid)
})

# --- Review fix round 1 (findings 1-3) ------------------------------------

test_that("diagnose_collection reports an include pattern outside the collection as invalid", {
  local_offline_endpoints()
  root <- local_collection()
  suppressMessages(settings_set(root, "files.include" = list("../outside/*.R")))
  msgs <- testthat::capture_messages(res <- diagnose_collection(root))
  expect_false(res$config$valid)
  expect_match(res$config$issues, "outside the collection", fixed = TRUE)
  expect_equal(res$overall, "error")
  expect_true(any(grepl("Some checks failed", msgs)))
})

test_that("a repository that did not answer makes its packages unknown, and is not cached", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  main <- local_fixture_repo(list(dplyr = c(Version = "1.1.4")))
  extra <- local_fixture_repo(list(paintr = c(Version = "0.0.1")))
  local_mocked_bindings(default_repo_url = function() main$url)
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("dplyr", "paintr"),
    "packages.repos" = list(extra$url)
  ))

  index_file <- fs::path(extra$contrib, "PACKAGES.rds")
  away_file <- fs::path(extra$contrib, "PACKAGES.rds.away")
  fs::file_move(index_file, away_file)

  expect_warning(res <- suppressMessages(check_inventory(path = root)), "unknown")
  expect_equal(res$available, c(TRUE, NA))

  fs::file_move(away_file, index_file)
  res2 <- suppressMessages(check_inventory(path = root))
  expect_equal(res2$available, c(TRUE, TRUE))
})

# --- Shared signatures and a stable result --------------------

test_that("check_inventory() and diagnose_collection() take the shared arguments", {
  expect_identical(formals_text(check_inventory), c(packages = "NULL", path = "\".\""))
  expect_identical(formals_text(diagnose_collection), c(path = "\".\""))
})

# [R] check_inventory() returns a data frame, not pyodidarian's InventoryRow.
# The drift check appends its drift columns after these five, so only the leading
# five are pinned here.
test_that("check_inventory() returns a data frame led by the R columns", {
  root <- local_collection()
  res <- suppressMessages(check_inventory(path = root))
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
  expect_identical(names(res)[1:5], c("package", "available", "version", "note", "repository"))
  expect_type(res$available, "logical")
})

test_that("diagnose_collection() returns ok and one row per problem, with severity, area and message", {
  local_offline_endpoints()
  root <- local_collection()
  cat("zzzzzz: 1\n", file = fs::path(root, "_webrarian.yml"), append = TRUE)
  res <- suppressMessages(diagnose_collection(fs::path(root)))
  expect_true(res$ok)
  expect_equal(res$overall, "warning")
  expect_s3_class(res$problems, "data.frame")
  expect_named(res$problems, c("severity", "area", "message"))
  hit <- res$problems[grepl("zzzzzz", res$problems$message, fixed = TRUE), , drop = FALSE]
  expect_gte(nrow(hit), 1L)
  expect_true(all(hit$severity == "warning"))
  expect_true(all(hit$area == "config"))
})

test_that("diagnose_collection() is not ok when bind() would fail, and still reports everything", {
  local_offline_endpoints()
  local_mocked_bindings(docker_status = function(timeout = 10) "stopped")
  root <- local_collection()
  sub <- fs::path(root, "analysis")
  fs::dir_create(sub)
  pkg <- fs::path(root, "pkgs", "demotools")
  fs::dir_create(pkg)
  writeLines(c("Package: demotools", "Version: 0.1.0"), fs::path(pkg, "DESCRIPTION"))
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))

  res <- suppressMessages(diagnose_collection(sub))
  expect_false(res$ok)
  expect_equal(res$overall, "error")
  docker <- res$problems[grepl("Docker", res$problems$message, fixed = TRUE), , drop = FALSE]
  expect_equal(nrow(docker), 1L)
  expect_equal(docker$severity, "error")
  expect_equal(docker$area, "tools")
})

test_that("outside a collection only the network and tools are checked", {
  local_offline_endpoints()
  res <- suppressMessages(diagnose_collection(withr::local_tempdir()))
  expect_true(res$ok)
  expect_equal(nrow(res$problems), 0L)
  expect_named(res$problems, c("severity", "area", "message"))
  expect_identical(res$config, list())
})

# A build that bundles its packages (build.bundle-engine: true, the default,
# or build.offline: true) downloads every configured prebuilt package and
# aborts on one no repository has; a site whose page installs its packages
# leaves that to the page.
test_that("a prebuilt package no repository has fails a bundling build's diagnosis and only warns otherwise", {
  skip_if_not_installed("httpuv")
  local_offline_endpoints()
  local_isolated_cache()
  repo <- local_fixture_repo(list(dplyr = c(Version = "1.1.4")))
  local_mocked_bindings(default_repo_url = function() repo$url)
  root <- local_collection() # build.bundle-engine: false
  suppressMessages(settings_set(root, "packages.prebuilt" = list("dplyr", "notonwasm")))

  online <- suppressMessages(diagnose_collection(root))
  expect_true(online$ok)
  expect_equal(online$overall, "warning")
  missing <- online$problems[
    grepl("notonwasm", online$problems$message, fixed = TRUE),
    ,
    drop = FALSE
  ]
  expect_equal(missing$severity, "warning")
  expect_equal(missing$area, "packages")
  expect_false(any(grepl("dplyr", online$problems$message, fixed = TRUE)))

  suppressMessages(settings_set(root, "build.bundle-engine" = TRUE))
  bundled <- suppressMessages(diagnose_collection(root))
  expect_false(bundled$ok)
  expect_equal(bundled$overall, "error")
  expect_true(any(grepl(
    "notonwasm",
    bundled$problems$message[bundled$problems$severity == "error"],
    fixed = TRUE
  )))

  suppressMessages(settings_set(root, "build.bundle-engine" = FALSE, "build.offline" = TRUE))
  expect_false(suppressMessages(diagnose_collection(root))$ok)
})

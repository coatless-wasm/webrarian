# Bundled WebAssembly versions against CRAN and the author's own R.

local_cran_like <- function(
  rows,
  dir = withr::local_tempdir(.local_envir = env),
  env = parent.frame()
) {
  fs::dir_create(fs::path(dir, "src", "contrib"))
  write.dcf(rows, fs::path(dir, "src", "contrib", "PACKAGES"))
  file_url(dir)
}

# file:///C:/... on Windows, file:///tmp/... elsewhere.
file_url <- function(dir) {
  path <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  paste0("file://", if (startsWith(path, "/")) "" else "/", path)
}

test_that("version_drift() says whether the WebAssembly version is behind, current or ahead", {
  expect_identical(
    version_drift(c("1.8.1", "1.3.0", NA, "2.0", "1.0"), c("1.9.0", "1.3.0", "1.0", "1.0", NA)),
    c("behind", "current", NA, "ahead", NA)
  )
})

test_that("cran_package_versions() reads a CRAN-like repository", {
  local_isolated_cache()
  repo <- local_cran_like(data.frame(Package = c("glue", "rlang"), Version = c("1.9.0", "1.3.0")))
  expect_identical(cran_package_versions(c("glue", "nopkg"), repo), c(glue = "1.9.0", nopkg = NA))
})

test_that("cran_package_versions() gives NULL, quietly, when CRAN is skipped or cannot be reached", {
  local_isolated_cache()
  expect_null(cran_package_versions("glue", NULL))
  expect_no_warning(expect_silent(res <- cran_package_versions("glue", "http://127.0.0.1:9")))
  expect_null(res)
})

# The drift report is cached per day.
test_that("a repository's index is read at most once a day", {
  local_isolated_cache()
  dir <- withr::local_tempdir()
  repo <- local_cran_like(data.frame(Package = "glue", Version = "1.9.0"), dir)
  today <- as.Date("2026-09-25")
  expect_identical(cran_index_versions(repo, today), c(glue = "1.9.0"))
  fs::file_delete(fs::path(dir, "src", "contrib", "PACKAGES"))
  expect_identical(cran_index_versions(repo, today), c(glue = "1.9.0"))
  expect_null(cran_index_versions(repo, today + 1))
})

# A build machine with no route to CRAN would otherwise wait out the timeout (for each of
# PACKAGES.rds, .gz and plain) on every bind(), check_inventory() and diagnose_collection().
test_that("a repository that cannot be read is not asked again the same day", {
  local_isolated_cache()
  calls <- 0L
  local_mocked_bindings(read_repo_index = function(repo) {
    calls <<- calls + 1L
    NULL
  })
  today <- as.Date("2026-09-25")
  expect_null(cran_index_versions("https://cran.unreachable.example", today))
  expect_null(cran_index_versions("https://cran.unreachable.example", today))
  expect_identical(calls, 1L)
  expect_null(cran_index_versions("https://cran.unreachable.example", today + 1))
  expect_identical(calls, 2L)
})

test_that("drift_cran_repo() honors the webrarian.cran_repo option", {
  withr::local_options(webrarian.cran_repo = FALSE)
  expect_null(drift_cran_repo())
  withr::local_options(webrarian.cran_repo = "https://cran.example.org")
  expect_identical(drift_cran_repo(), "https://cran.example.org")
  withr::local_options(webrarian.cran_repo = NULL, repos = c(CRAN = "@CRAN@"))
  expect_identical(drift_cran_repo(), "https://cloud.r-project.org")
})

test_that("local_package_versions() reads the installed version, NA when absent", {
  v <- local_package_versions(c("jsonlite", "webrarianNoSuchPackage"))
  expect_identical(unname(v[["jsonlite"]]), as.character(utils::packageVersion("jsonlite")))
  expect_true(is.na(v[["webrarianNoSuchPackage"]]))
})

test_that("check_inventory() adds the CRAN and local versions and the drift", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0"), jsonlite = c(Version = "0.1.0")))
  local_mocked_bindings(
    default_repo_url = function() repo$url,
    cran_package_versions = function(packages, repo = NULL) {
      stats::setNames(c("1.9.0", "0.1.0")[seq_along(packages)], packages)
    }
  )
  res <- suppressMessages(check_inventory(c("glue", "jsonlite")))
  expect_identical(
    names(res),
    c(
      "package",
      "available",
      "version",
      "note",
      "repository",
      "cran_version",
      "local_version",
      "drift"
    )
  )
  expect_identical(res$cran_version, c("1.9.0", "0.1.0"))
  expect_identical(res$drift, c("behind", "current"))
  expect_identical(res$local_version[[2]], as.character(utils::packageVersion("jsonlite")))
  msgs <- testthat::capture_messages(check_inventory(c("glue", "jsonlite")))
  expect_true(any(grepl("older", msgs, fixed = TRUE)))
})

test_that("check_inventory() has the same eight columns when there is nothing to check", {
  root <- local_collection()
  res <- suppressMessages(check_inventory(path = root))
  expect_identical(nrow(res), 0L)
  expect_identical(
    names(res),
    c(
      "package",
      "available",
      "version",
      "note",
      "repository",
      "cran_version",
      "local_version",
      "drift"
    )
  )
})

test_that("bind() names bundled packages that lag CRAN and writes packages.json", {
  skip_if_not_installed("httpuv")
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0")))
  local_mocked_bindings(
    default_repo_url = function() repo$url,
    cran_package_versions = function(packages, repo = NULL) {
      stats::setNames(rep("1.9.0", length(packages)), packages)
    }
  )
  # build.bundle-engine: true copies glue into the site, so bind() compares it.
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  msgs <- testthat::capture_messages(bind(root))
  expect_true(any(grepl("glue 1.8.0 (CRAN 1.9.0", msgs, fixed = TRUE)))
  manifest <- jsonlite::read_json(fs::path(root, "_site", "packages.json"))
  expect_match(manifest[["generated-by"]], "^webrarian ")
  expect_identical(manifest$packages[[1]]$package, "glue")
  expect_identical(manifest$packages[[1]]$version, "1.8.0")
  expect_identical(manifest$packages[[1]][["cran-version"]], "1.9.0")
  expect_identical(manifest$packages[[1]]$drift, "behind")
  # Published with the site: nothing about the author's own R library.
  expect_false("local-version" %in% names(manifest$packages[[1]]))
})

test_that("bind() writes no packages.json for a site that bundles nothing", {
  # local_collection() builds with build.bundle-engine: false and names no package.
  root <- local_collection()
  suppressMessages(bind(root))
  expect_false(fs::file_exists(fs::path(root, "_site", "packages.json")))
})

# Every bind() rewrites packages.json at the same URL, so browsers and CDNs revalidate it, as
# they do everything else a build rewrites in place (the rule goes to _headers).
test_that("packages.json revalidates", {
  rules <- cache_rules()
  paths <- vapply(rules, `[[`, character(1), "path")
  expect_identical(rules[[match("/packages.json", paths)]]$headers[["Cache-Control"]], "no-cache")
})

test_that("the test suite never asks CRAN unless a test says so", {
  expect_false(getOption("webrarian.cran_repo"))
})

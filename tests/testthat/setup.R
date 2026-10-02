# Keep every test's downloads out of the developer's own cache. CI and the
# commands in the plans set R_USER_CACHE_DIR themselves; when nothing did,
# use one cache for this test run under tempdir().
if (!nzchar(Sys.getenv("R_USER_CACHE_DIR"))) {
  withr::local_envvar(
    R_USER_CACHE_DIR = file.path(tempdir(), "webrarian-test-cache"),
    .local_envir = testthat::teardown_env()
  )
}

# Tests never compare versions with the real CRAN (R/drift.R): a test that needs
# CRAN versions mocks cran_package_versions() or passes a file:// repository.
withr::local_options(webrarian.cran_repo = FALSE, .local_envir = testthat::teardown_env())

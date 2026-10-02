# Internal preconditions are not part of the API.

test_that("read_brand_yml() and check_requirements() are internal", {
  expect_false("read_brand_yml" %in% namespace_exports())
  expect_false("check_requirements" %in% namespace_exports())
  expect_true(is.function(read_brand_yml))
  expect_true(is.function(check_requirements))
})

test_that("user-facing errors carry classes", {
  expect_error(
    suppressMessages(settings_set(withr::local_tempdir(), "project.name" = "x")),
    class = "webrarian_error_not_collection"
  )
  expect_error(
    collection_settings(withr::local_tempdir()),
    class = "webrarian_error_not_collection"
  )
  expect_error(validate_webr_version("0.5.8; echo hi"), class = "webrarian_error_invalid_version")
  expect_error(resolve_webr_version("0.4.2"), class = "webrarian_error_invalid_version")
  expect_error(resolve_webr_version(0.6), class = "webrarian_error_invalid_version")
  expect_error(
    config_keys_to_snake(list(files = list(mount_point = "/x"))),
    class = "webrarian_error_config_key"
  )
  dir <- withr::local_tempdir()
  writeLines(c("repl:", "  auto_run: []"), fs::path(dir, "_webrarian.yml"))
  expect_error(collection_settings(dir), class = "webrarian_error_config_key")

  root <- local_collection()
  pkg <- fs::path(root, "pkgs", "demotools")
  fs::dir_create(pkg)
  writeLines(c("Package: demotools", "Version: 0.1.0"), fs::path(pkg, "DESCRIPTION"))
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))
  local_mocked_bindings(docker_status = function(timeout = 10) "missing")
  expect_error(check_requirements(root), class = "webrarian_error_docker")
})

test_that("the key user-facing errors read as they should", {
  expect_snapshot(error = TRUE, check_collection("no-such-collection"))
  expect_snapshot(error = TRUE, config_keys_to_snake(list(files = list(mount_point = "/x"))))
  root <- local_collection()
  pkg <- fs::path(root, "pkgs", "demotools")
  fs::dir_create(pkg)
  writeLines(c("Package: demotools", "Version: 0.1.0"), fs::path(pkg, "DESCRIPTION"))
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))
  local_mocked_bindings(docker_status = function(timeout = 10) "stopped")
  expect_snapshot(error = TRUE, check_requirements(root))
})

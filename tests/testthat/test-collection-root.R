# Every path-taking function finds the collection above the directory it is
# given, as collection_settings() always has.

test_that("every path-taking function finds the collection from a subdirectory", {
  # acquire_package() looks prebuilt names up; no repository is
  # contacted here, and nothing is cached in the developer's cache.
  local_mocked_bindings(fetch_webr_packages_index = function(...) NULL)
  root <- local_collection()
  sub <- fs::path(root, "analysis", "data")
  fs::dir_create(sub)
  writeLines("x <- 1", fs::path(root, "a.R"))

  suppressMessages(settings_set(sub, "project.name" = "from-sub"))
  expect_equal(collection_settings(root)$project$name, "from-sub")

  suppressMessages(acquire_file("a.R", path = sub))
  expect_identical(collection_files(sub)$files, "a.R")
  suppressMessages(acquire_package("glue", path = sub))
  expect_identical(collection_packages(sub)$package, "glue")

  suppressMessages(bind(sub))
  expect_true(fs::file_exists(fs::path(root, "_site", "index.html")))
  expect_false(fs::dir_exists(fs::path(sub, "_site")))

  suppressMessages(withdraw_package("glue", path = sub))
  suppressMessages(withdraw_file("a.R", path = sub))
  expect_length(collection_packages(root)$package, 0L)

  suppressMessages(circulate_via_github(sub))
  expect_true(fs::file_exists(fs::path(root, ".github", "workflows", "deploy-webr.yml")))

  suppressMessages(clean_shelves(sub))
  expect_false(fs::dir_exists(fs::path(root, "_site")))
})

test_that("check_inventory() reads the collection's packages from a subdirectory", {
  root <- local_collection()
  sub <- fs::path(root, "analysis")
  fs::dir_create(sub)
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  local_mocked_bindings(fetch_webr_packages_index = function(...) NULL)
  res <- suppressWarnings(suppressMessages(check_inventory(path = sub)))
  expect_identical(res$package, "glue")
})

test_that("outside any collection the error says so", {
  dir <- withr::local_tempdir()
  expect_error(
    suppressMessages(settings_set(dir, "project.name" = "x")),
    "Not a webrarian collection"
  )
  expect_error(suppressMessages(bind(dir)), "Not a webrarian collection")
})

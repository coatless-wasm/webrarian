# bind() returns what it built, with a size breakdown (the BindResult contract).

test_that("bind() returns the site path, sizes by part, packages and mode", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  writeLines("x <- 1", fs::path(root, "a.R"))
  suppressMessages(settings_set(root, "files.include" = list("a.R")))

  result <- suppressMessages(bind(root))

  expect_s3_class(result, "webrarian_bind_result")
  expect_identical(
    as.character(result$output),
    as.character(fs::path(fs::path_real(root), "_site"))
  )
  expect_named(result$size_by_part, c("engine", "packages", "files", "viewer"))
  expect_true(all(result$size_by_part >= 0))
  expect_gt(result$size_by_part[["engine"]], 0)
  expect_gt(result$size_by_part[["files"]], 0)
  site_files <- fs::dir_ls(result$output, recurse = TRUE, type = "file", all = TRUE)
  expect_equal(sum(result$size_by_part), sum(fs::file_info(site_files)$size))
  # The engine is in the site, and the site is still online (the default).
  expect_false(result$offline)
  expect_identical(result$packages, character())
})

test_that("a build with the CDN engine reports no engine bytes, its packages, and prints a summary", {
  root <- local_collection() # build.bundle-engine: false
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  result <- suppressMessages(bind(root))
  expect_false(result$offline)
  expect_equal(result$size_by_part[["engine"]], 0)
  expect_identical(result$packages, "glue")
  out <- testthat::capture_messages(print(result))
  expect_true(any(grepl("loaded from the webR CDN", out, fixed = TRUE)))
  expect_true(any(grepl("glue", out, fixed = TRUE)))
})

test_that("an offline build says so, and carries its engine even with build.bundle-engine: false", {
  local_fake_engine()
  root <- local_collection() # build.bundle-engine: false
  result <- suppressMessages(bind(root, offline = TRUE))
  expect_true(result$offline)
  expect_gt(result$size_by_part[["engine"]], 0)
  out <- testthat::capture_messages(print(result))
  expect_true(any(grepl("Offline: yes", out, fixed = TRUE)))
})

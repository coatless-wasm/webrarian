test_that("acquire_file adds paths to config", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create(c("data", "scripts"))
    writeLines("a", "data/a.csv")
    writeLines("x <- 1", "scripts/a.R")

    suppressMessages(acquire_file(c("data/", "scripts/*.R")))

    config <- collection_settings()
    expect_true("data/" %in% unlist(config$files$include))
    expect_true("scripts/*.R" %in% unlist(config$files$include))
  })
})

test_that("acquire_file doesn't duplicate", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create(c("data", "scripts"))
    writeLines("a", "data/a.csv")
    writeLines("x <- 1", "scripts/a.R")

    suppressMessages(acquire_file("data/"))
    suppressMessages(acquire_file("data/"))

    config <- collection_settings()
    expect_equal(sum(unlist(config$files$include) == "data/"), 1)
  })
})

test_that("withdraw_file removes paths", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create(c("data", "scripts"))
    writeLines("a", "data/a.csv")
    writeLines("x <- 1", "scripts/a.R")
    suppressMessages(acquire_file(c("data/", "scripts/")))

    suppressMessages(withdraw_file("data/"))

    config <- collection_settings()
    expect_false("data/" %in% unlist(config$files$include))
    expect_true("scripts/" %in% unlist(config$files$include))
  })
})

test_that("collection_files returns config patterns", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create(c("data", "scripts"))
    writeLines("a", "data/a.csv")
    writeLines("x <- 1", "scripts/a.R")
    suppressMessages(acquire_file(c("data/", "scripts/*.R")))

    result <- collection_files()
    expect_s3_class(result, "webrarian_files")
    expect_true("data/" %in% result$include)
  })
})

test_that("collection_files resolves patterns", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create("data")
    fs::file_create("data/test.csv")
    fs::file_create("data/test.rds")
    suppressMessages(acquire_file("data/"))

    result <- collection_files()
    expect_s3_class(result, "webrarian_files")
    expect_true(any(grepl("test.csv", result$files)))
  })
})

test_that("acquire_file() takes the shared arguments and leaves files.mount-point alone", {
  expect_identical(formals_text(acquire_file), c(paths = "", path = "\".\""))
  root <- local_collection()
  suppressMessages(settings_set(root, "files.mount-point" = "/home/web_user/work"))
  writeLines("x", fs::path(root, "a.R"))
  suppressMessages(acquire_file("a.R", root))
  cfg <- collection_settings(root)
  expect_true("a.R" %in% unlist(cfg$files$include))
  expect_equal(cfg$files$mount_point, "/home/web_user/work")
})

test_that("acquire_file refuses paths that match nothing or lie outside the collection", {
  withr::with_tempdir({
    suppressMessages(catalog(detect = FALSE))
    expect_error(acquire_file("missing.R"), "Nothing matches")
    expect_error(acquire_file("scripts/*.R"), "Nothing matches")
    expect_error(acquire_file("../x.R"), "outside the collection")
    expect_length(unlist(collection_settings()$files$include), 0L)
  })
})

test_that("acquire_file stores an absolute path inside the collection as a relative one", {
  withr::with_tempdir({
    suppressMessages(catalog(detect = FALSE))
    fs::dir_create("data")
    writeLines("a", "data/a.csv")
    suppressMessages(acquire_file(fs::path_abs("data")))
    expect_true("data/" %in% unlist(collection_settings()$files$include))
  })
})

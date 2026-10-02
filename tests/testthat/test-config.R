test_that("collection_settings reads configuration", {
  withr::with_tempdir({
    suppressMessages(catalog())

    config <- collection_settings()
    expect_s3_class(config, "webrarian_config")
    expect_true(!is.null(config$project$name))
    expect_true(!is.null(config$webr$version))
  })
})

test_that("collection_settings applies defaults", {
  withr::with_tempdir({
    suppressMessages(catalog())

    config <- collection_settings()
    expect_equal(config$build$output_dir, "_site")
    expect_equal(config$webr$version, default_webr_version())
  })
})

test_that("settings_get retrieves nested values", {
  withr::with_tempdir({
    suppressMessages(catalog())

    config <- collection_settings()
    expect_equal(settings_get(config, "webr.version"), config$webr$version)
    expect_equal(settings_get(config, "build.output-dir"), "_site")
  })
})

test_that("settings_get returns default for missing values", {
  withr::with_tempdir({
    suppressMessages(catalog())

    config <- collection_settings()
    expect_equal(
      settings_get(config, "nonexistent.key", default = "fallback"),
      "fallback"
    )
  })
})

test_that("collection_settings fails for non-project", {
  withr::with_tempdir({
    expect_error(
      collection_settings(),
      "Not a webrarian collection"
    )
  })
})

test_that("merge_config keeps the default when a scalar key is present but NULL", {
  # A hand-edited _webrarian.yml with a blank `version:` parses that key to
  # NULL. That must fall back to the default, not delete it.
  defaults <- list(
    webr = list(version = "0.5.8", channel = "latest"),
    name = "demo"
  )
  config <- list(webr = list(version = NULL, channel = "dev"))

  result <- merge_config(defaults, config)

  expect_equal(result$webr$version, "0.5.8") # default preserved
  expect_equal(result$webr$channel, "dev") # explicit override applied
  expect_equal(result$name, "demo") # untouched default preserved
})

test_that("merge_config keeps a whole default subtree when a top-level key is NULL", {
  defaults <- list(
    webr = list(version = "0.5.8"),
    build = list(output_dir = "_site", compress = TRUE)
  )
  config <- list(build = NULL) # `build:` line left blank

  result <- merge_config(defaults, config)

  expect_equal(result$build$output_dir, "_site")
  expect_true(result$build$compress)
})

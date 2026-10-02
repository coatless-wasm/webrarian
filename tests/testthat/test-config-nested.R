# Tests for nested config helpers in R/config.R:
# set_nested_value(), settings_get(), settings_set(), apply_config_defaults()

test_that("set_nested_value sets a top-level key and returns the list", {
  x <- list(a = 1, b = 2)
  out <- set_nested_value(x, "a", 99)
  expect_equal(out$a, 99)
  expect_equal(out$b, 2)
})

test_that("set_nested_value sets a deeply nested key by a character path vector", {
  x <- list(webr = list(version = "0.4.2", base_url = "https://example.com"))
  out <- set_nested_value(x, c("webr", "version"), "0.5.8")
  expect_equal(out$webr$version, "0.5.8")
  # Sibling key untouched
  expect_equal(out$webr$base_url, "https://example.com")
})

test_that("set_nested_value creates intermediate lists for missing paths", {
  x <- list()
  out <- set_nested_value(x, c("a", "b", "c"), "deep")
  expect_type(out$a, "list")
  expect_type(out$a$b, "list")
  expect_equal(out$a$b$c, "deep")
})

test_that("settings_get returns a nested value", {
  config <- list(webr = list(version = "0.5.8"))
  expect_equal(settings_get(config, "webr.version"), "0.5.8")
})

test_that("settings_get returns the default for a missing path", {
  config <- list(webr = list(version = "0.5.8"))
  expect_null(settings_get(config, "webr.nope"))
  expect_equal(
    settings_get(config, "packages.prebuilt", default = character()),
    character()
  )
  # Descending into a non-list also yields the default
  expect_equal(settings_get(config, "webr.version.sub", default = "d"), "d")
})

test_that("apply_config_defaults fills documented defaults into a minimal config", {
  filled <- apply_config_defaults(list(project = list(name = "mine")))

  expect_equal(filled$project$name, "mine")
  expect_equal(filled$project$description, "")
  expect_equal(filled$build$output_dir, "_site")
  expect_false(filled$build$offline)
  expect_true(filled$build$bundle_engine)
  expect_true(filled$build$clean)
  expect_equal(filled$webr$version, default_webr_version())
  expect_equal(filled$packages$prebuilt, list())
  expect_true(filled$packages$dependencies)
  expect_equal(filled$repl$share_links, "open")
  expect_true(filled$repl$panels$environment)
  expect_true("startup_script" %in% names(filled$repl))
  expect_null(filled$deploy)
})

test_that("apply_config_defaults handles NULL config", {
  filled <- apply_config_defaults(NULL)
  expect_equal(filled$build$output_dir, "_site")
  expect_equal(filled$webr$version, default_webr_version())
})

test_that("apply_config_defaults keeps a blank (NULL) key at its default", {
  # A blank `version:` in YAML parses to NULL; default must survive.
  filled <- apply_config_defaults(list(webr = list(version = NULL)))
  expect_equal(filled$webr$version, default_webr_version())
})

test_that("settings_set persists a nested value and collection_settings reflects it", {
  withr::with_tempdir({
    suppressMessages(catalog("."))

    suppressMessages(settings_set(".", "webr/version" = "0.4.2"))

    cfg <- suppressMessages(collection_settings("."))
    expect_equal(cfg$webr$version, "0.4.2")
  })
})

test_that("settings_set on a nested key does not wipe sibling defaults", {
  withr::with_tempdir({
    suppressMessages(catalog("."))

    suppressMessages(settings_set(".", "webr/version" = "0.9.9"))

    cfg <- suppressMessages(collection_settings("."))
    expect_equal(cfg$webr$version, "0.9.9")
    # Unrelated branch defaults still present
    expect_equal(cfg$build$output_dir, "_site")
    expect_equal(cfg$repl$share_links, "open")
  })
})

test_that("settings_set can update multiple keys at once", {
  withr::with_tempdir({
    suppressMessages(catalog("."))

    suppressMessages(
      settings_set(".", "project/name" = "renamed", "build/clean" = FALSE)
    )

    cfg <- suppressMessages(collection_settings("."))
    expect_equal(cfg$project$name, "renamed")
    expect_false(cfg$build$clean)
  })
})

# Tests for R/utils.R pure/offline helpers

test_that("write_yaml_file then read_yaml_file round-trips a nested list", {
  withr::with_tempdir({
    x <- list(
      name = "my-collection",
      packages = list(prebuilt = list("dplyr", "ggplot2")),
      nested = list(a = list(b = "deep"))
    )
    path <- "config.yml"
    ret <- write_yaml_file(x, path)

    expect_equal(ret, path)
    expect_true(fs::file_exists(path))

    back <- read_yaml_file(path)
    expect_equal(back$name, "my-collection")
    expect_equal(back$packages$prebuilt, c("dplyr", "ggplot2"))
    expect_equal(back$nested$a$b, "deep")
  })
})

test_that("write_yaml_file serializes logicals as true/false and reads back as logical", {
  withr::with_tempdir({
    x <- list(enabled = TRUE, disabled = FALSE)
    path <- "bool.yml"
    write_yaml_file(x, path)

    text <- readLines(path)
    joined <- paste(text, collapse = "\n")
    # Custom handler emits lowercase true/false (not yes/no)
    expect_match(joined, "enabled: true")
    expect_match(joined, "disabled: false")
    expect_false(grepl("yes", joined))
    expect_false(grepl("\\bno\\b", joined))

    back <- read_yaml_file(path)
    expect_identical(back$enabled, TRUE)
    expect_identical(back$disabled, FALSE)
  })
})

test_that("read_yaml_file returns NULL for a missing file", {
  withr::with_tempdir({
    expect_null(read_yaml_file("does-not-exist.yml"))
  })
})

test_that("ensure_dir creates a nested directory and is idempotent", {
  withr::with_tempdir({
    nested <- fs::path("a", "b", "c")
    ret <- ensure_dir(nested)

    expect_equal(ret, nested)
    expect_true(fs::dir_exists(nested))

    # Calling again is a no-op and must not error
    ret2 <- ensure_dir(nested)
    expect_equal(ret2, nested)
    expect_true(fs::dir_exists(nested))
  })
})

test_that("check_collection errors outside a collection and succeeds inside one", {
  withr::with_tempdir({
    dir <- withr::local_tempdir()
    expect_error(check_collection(dir), "Not a webrarian collection")

    suppressMessages(catalog(dir))
    expect_true(check_collection(dir))
  })
})

test_that("default_webr_version returns a semantic version string", {
  v <- default_webr_version()
  expect_type(v, "character")
  expect_length(v, 1)
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})

test_that("docker_available returns a length-1 logical", {
  res <- docker_available()
  expect_type(res, "logical")
  expect_length(res, 1)
  expect_false(is.na(res))
})

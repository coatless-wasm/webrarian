# Tests for pure/offline helpers in R/project.R

test_that("is_collection is FALSE before catalog and TRUE after", {
  withr::with_tempdir({
    expect_false(is_collection("."))

    suppressMessages(catalog(".", detect = FALSE))

    expect_true(is_collection("."))
    expect_true(fs::file_exists("_webrarian.yml"))
  })
})

test_that("collection_root resolves to the collection directory", {
  withr::with_tempdir({
    suppressMessages(catalog(".", detect = FALSE))

    root <- collection_root(".")
    expect_false(is.null(root))
    # Root should be the current temp dir (compare canonical paths)
    expect_equal(
      as.character(fs::path_real(root)),
      as.character(fs::path_real("."))
    )
  })
})

test_that("collection_root returns NULL when no collection exists", {
  tmp <- withr::local_tempdir()
  expect_null(collection_root(tmp))
})

test_that("collection_root finds ancestor collection from a subdirectory", {
  tmp <- withr::local_tempdir()
  suppressMessages(catalog(tmp, detect = FALSE))
  root_abs <- fs::path_real(tmp)

  deeper <- fs::path(tmp, "nested", "deeper")
  fs::dir_create(deeper)
  withr::local_dir(deeper)

  # is_collection is FALSE in the subdir, but collection_root walks up
  expect_false(is_collection("."))
  root <- collection_root(".")
  expect_false(is.null(root))
  expect_equal(as.character(fs::path_real(root)), as.character(root_abs))
})

test_that("detect_file_patterns detects R and data file patterns", {
  tmp <- withr::local_tempdir()
  writeLines("x <- 1", fs::path(tmp, "analysis.R"))
  writeLines("a,b\n1,2", fs::path(tmp, "data.csv"))

  patterns <- detect_file_patterns(tmp)

  expect_true("*.R" %in% patterns)
  expect_true("*.csv" %in% patterns)
  expect_false(any(duplicated(patterns)))
})

test_that("detect_file_patterns detects common directories", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "data"))
  fs::dir_create(fs::path(tmp, "scripts"))

  patterns <- detect_file_patterns(tmp)

  expect_true("data/" %in% patterns)
  expect_true("scripts/" %in% patterns)
})

test_that("detect_file_patterns returns empty for an empty directory", {
  tmp <- withr::local_tempdir()
  expect_identical(detect_file_patterns(tmp), character())
})

test_that("detect_package_dependencies finds library() calls and excludes base pkgs", {
  skip_if_not_installed("renv")

  tmp <- withr::local_tempdir()
  writeLines(
    c(
      "library(utils)",
      "library(stats)",
      "jsonlite::fromJSON('x')"
    ),
    fs::path(tmp, "script.R")
  )

  deps <- detect_package_dependencies(tmp)

  expect_type(deps, "character")
  # utils and stats are base packages and must be filtered out
  expect_false("utils" %in% deps)
  expect_false("stats" %in% deps)
  # non-base namespace usage is retained
  expect_true("jsonlite" %in% deps)
})

test_that("detect_package_dependencies returns empty character for no R files", {
  skip_if_not_installed("renv")
  tmp <- withr::local_tempdir()
  deps <- detect_package_dependencies(tmp)
  expect_type(deps, "character")
  expect_length(deps, 0)
})

test_that("create_initial_config builds expected structure for minimal template", {
  config <- create_initial_config("myproj", "minimal")

  expect_equal(config$project$name, "myproj")
  expect_null(config$project$version)
  expect_equal(config$webr$version, default_webr_version())
  expect_identical(config$packages$prebuilt, list())
  expect_true(config$packages$dependencies)
  expect_equal(config$build$output_dir, "_site")
  expect_null(config$deploy)
})

test_that("create_initial_config data-analysis template seeds packages and files", {
  config <- create_initial_config("proj", "data-analysis")

  expect_true("dplyr" %in% unlist(config$packages$prebuilt))
  expect_true("ggplot2" %in% unlist(config$packages$prebuilt))
  expect_true("data/" %in% unlist(config$files$include))
  expect_true("scripts/" %in% unlist(config$files$include))
})

test_that("create_initial_config package template configures local build", {
  config <- create_initial_config("proj", "package")

  expect_identical(config$packages$local, list("."))
  # A visitor reads and runs examples/, not the package's sources, and the
  # files panel lists them (pyodidarian's package template is the same).
  expect_identical(config$files$include, list("examples/"))
  expect_identical(config$repl$auto_open, list("examples/example.R"))
  expect_true(config$repl$panels$files)
  expect_null(config$repl$show_files)
})

test_that("is_current_dir compares canonical paths", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  expect_true(is_current_dir("."))
  expect_true(is_current_dir(getwd()))
  expect_true(is_current_dir(fs::path_abs(".")))

  sub <- fs::path(tmp, "sub")
  fs::dir_create(sub)
  expect_false(is_current_dir(sub))
  expect_false(is_current_dir(fs::path_dir(fs::path_real(tmp))))
})

test_that("is_current_dir sees through symlinked parents", {
  skip_on_os("windows")

  tmp <- withr::local_tempdir()
  real <- fs::path(tmp, "real")
  fs::dir_create(real)
  link <- fs::path(tmp, "link")
  fs::link_create(real, link)

  withr::local_dir(link)
  # Both spellings name the same directory, so neither counts as a subdirectory
  expect_true(is_current_dir(link))
  expect_true(is_current_dir(real))
})

test_that("relative_display_path prefers a path relative to the working directory", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  fs::dir_create(fs::path(tmp, "my-app"))

  expect_equal(relative_display_path(fs::path_abs("my-app")), "my-app")
})

test_that("relative_display_path falls back to the absolute path elsewhere", {
  other <- withr::local_tempdir()
  withr::local_dir(withr::local_tempdir())

  shown <- relative_display_path(fs::path_abs(other))

  expect_true(fs::is_absolute_path(shown))
  expect_equal(
    as.character(fs::path_real(shown)),
    as.character(fs::path_real(other))
  )
})

test_that("catalog_next_steps only adds the move-in step for a subdirectory", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  here <- catalog_next_steps(fs::path_abs("."))
  expect_length(here, 4)
  expect_false(any(grepl("setwd", here, fixed = TRUE)))

  sub <- fs::path(tmp, "my-app")
  fs::dir_create(sub)
  there <- catalog_next_steps(sub)
  expect_length(there, 6)
  expect_true(grepl("setwd(\"my-app\")", there[[1]], fixed = TRUE))
  expect_equal(names(there)[1], " ")
  expect_equal(names(there)[6], "i")
})

test_that("escape_cli_braces doubles braces so cli prints them literally", {
  expect_equal(escape_cli_braces("odd{name}"), "odd{{name}}")
  expect_equal(escape_cli_braces("plain"), "plain")
})

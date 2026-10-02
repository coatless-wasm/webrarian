# Tests for package management helpers in R/packages.R
# Pure/offline behavior only: no Docker, no network.

test_that("acquire_package adds a prebuilt package reflected in settings", {
  # acquire_package() looks prebuilt names up: keep the index cache in a
  # temporary directory, and give it a local port that refuses connections,
  # so the lookup is silent and nothing leaves the machine.
  local_isolated_cache()
  local_mocked_bindings(default_repo_url = function() "http://127.0.0.1:9")
  withr::with_tempdir({
    suppressMessages(catalog())

    suppressMessages(acquire_package("dplyr"))

    config <- collection_settings()
    expect_true("dplyr" %in% unlist(config$packages$prebuilt))
  })
})

test_that("acquire_package returns updated settings invisibly", {
  # acquire_package() looks prebuilt names up: keep the index cache in a
  # temporary directory, and give it a local port that refuses connections,
  # so the lookup is silent and nothing leaves the machine.
  local_isolated_cache()
  local_mocked_bindings(default_repo_url = function() "http://127.0.0.1:9")
  withr::with_tempdir({
    suppressMessages(catalog())

    res <- suppressMessages(acquire_package("dplyr"))
    expect_s3_class(res, "webrarian_config")
    expect_true("dplyr" %in% unlist(res$packages$prebuilt))
  })
})

test_that("acquire_package with source = github adds to the github list", {
  withr::with_tempdir({
    suppressMessages(catalog())

    suppressMessages(acquire_package("user/repo", source = "github"))

    config <- collection_settings()
    expect_true("user/repo" %in% unlist(config$packages$github))
    # Should not leak into the prebuilt list
    expect_false("user/repo" %in% unlist(config$packages$prebuilt))
  })
})

test_that("acquire_package with source = local adds to the local list", {
  withr::with_tempdir({
    suppressMessages(catalog())
    fs::dir_create("mypkg")
    writeLines(c("Package: mypkg", "Version: 0.1.0"), "mypkg/DESCRIPTION")

    suppressMessages(acquire_package("./mypkg", source = "local"))

    config <- collection_settings()
    expect_true("./mypkg" %in% unlist(config$packages$local))
  })
})

test_that("acquire_package does not create duplicate entries", {
  # acquire_package() looks prebuilt names up: keep the index cache in a
  # temporary directory, and give it a local port that refuses connections,
  # so the lookup is silent and nothing leaves the machine.
  local_isolated_cache()
  local_mocked_bindings(default_repo_url = function() "http://127.0.0.1:9")
  withr::with_tempdir({
    suppressMessages(catalog())

    suppressMessages(acquire_package("dplyr"))
    suppressMessages(acquire_package("dplyr"))

    config <- collection_settings()
    expect_equal(sum(unlist(config$packages$prebuilt) == "dplyr"), 1L)
  })
})

test_that("acquire_package() takes the shared arguments and leaves packages.dependencies alone", {
  expect_identical(
    formals_text(acquire_package),
    c(packages = "", path = "\".\"", source = "c(\"prebuilt\", \"local\", \"github\")")
  )
  # acquire_package() looks prebuilt names up; no repository is
  # contacted here, and nothing is cached in the developer's cache.
  local_mocked_bindings(fetch_webr_packages_index = function(...) NULL)
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.dependencies" = FALSE))
  suppressMessages(acquire_package("glue", root))
  cfg <- collection_settings(root)
  expect_true("glue" %in% unlist(cfg$packages$prebuilt))
  expect_false(cfg$packages$dependencies)
})

# acquire_package() looks prebuilt names up; its help page has
# to say so.
test_that("?acquire_package says prebuilt names are looked up", {
  rd <- test_path("..", "..", "man", "acquire_package.Rd")
  skip_if_not(file.exists(rd), "man/ is not in this tree")
  text <- paste(readLines(rd, warn = FALSE), collapse = " ")
  expect_match(text, "added with a warning", fixed = TRUE)
  expect_match(text, "not looked up", fixed = TRUE)
  expect_no_match(text, "included in the VFS", fixed = TRUE)
})

test_that("acquire_package validates names, GitHub specs and local paths", {
  withr::with_tempdir({
    suppressMessages(catalog(detect = FALSE))
    expect_error(acquire_package("not a pkg"), "not a valid R package name")
    expect_error(acquire_package("just-a-name", source = "github"), "owner/repo")
    expect_error(acquire_package("./nope", source = "local"), "does not exist")
    expect_no_error(suppressMessages(acquire_package("r-lib/cli@v3.6.0", source = "github")))
    expect_no_error(suppressMessages(acquire_package(
      "tidyverse/ggplot2/sub@main",
      source = "github"
    )))
  })
})

# A syntactically valid typo is still a typo. With build.bundle-engine: false
# bind() never looks at the repositories, so acquire time is the only build-
# side moment to say so.
test_that("acquire_package warns about a prebuilt package no repository has, and adds it anyway", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  repo <- local_fixture_repo(list(dplyr = c(Version = "1.1.4")))
  local_mocked_bindings(default_repo_url = function() repo$url)
  root <- local_collection()

  expect_warning(
    suppressMessages(acquire_package("fakepkgxyz123", path = root)),
    "fakepkgxyz123"
  )
  expect_no_warning(suppressMessages(acquire_package("dplyr", path = root)))
  expect_true(all(
    c("fakepkgxyz123", "dplyr") %in% unlist(collection_settings(root)$packages$prebuilt)
  ))
})

test_that("acquire_package says nothing about availability when no repository answers", {
  local_isolated_cache()
  local_mocked_bindings(default_repo_url = function() "http://127.0.0.1:9")
  root <- local_collection()
  expect_no_warning(suppressMessages(acquire_package("fakepkgxyz123", path = root)))
  expect_true("fakepkgxyz123" %in% unlist(collection_settings(root)$packages$prebuilt))
})

test_that("acquire_package says nothing about a name when a repository that may have it does not answer", {
  skip_if_not_installed("httpuv")
  local_isolated_cache()
  main <- local_fixture_repo(list(dplyr = c(Version = "1.1.4")))
  extra <- local_fixture_repo(list(paintr = c(Version = "0.0.1")))
  local_mocked_bindings(default_repo_url = function() main$url)
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.repos" = list(extra$url)))
  index_file <- fs::path(extra$contrib, "PACKAGES.rds")
  away_file <- fs::path(extra$contrib, "PACKAGES.rds.away")
  fs::file_move(index_file, away_file)

  expect_no_warning(suppressMessages(acquire_package("paintr", path = root)))
  expect_true("paintr" %in% unlist(collection_settings(root)$packages$prebuilt))

  fs::file_move(away_file, index_file)
  expect_warning(suppressMessages(acquire_package("fakepkgxyz123", path = root)), "fakepkgxyz123")
})

test_that("withdraw_package removes a package and keeps the rest", {
  withr::with_tempdir({
    suppressMessages(catalog(packages = c("dplyr", "ggplot2")))

    suppressMessages(withdraw_package("dplyr"))

    config <- collection_settings()
    expect_false("dplyr" %in% unlist(config$packages$prebuilt))
    expect_true("ggplot2" %in% unlist(config$packages$prebuilt))
  })
})

test_that("withdraw_package removes github-sourced packages too", {
  withr::with_tempdir({
    suppressMessages(catalog())
    suppressMessages(acquire_package("user/repo", source = "github"))

    suppressMessages(withdraw_package("user/repo"))

    config <- collection_settings()
    expect_false("user/repo" %in% unlist(config$packages$github))
  })
})

test_that("withdraw_package on a missing package warns and leaves config intact", {
  withr::with_tempdir({
    suppressMessages(catalog(packages = "dplyr"))

    # Warning is a cli alert (not an R warning condition); assert the config
    # is left untouched and settings are returned.
    res <- suppressMessages(withdraw_package("not-installed"))
    expect_s3_class(res, "webrarian_config")

    config <- collection_settings()
    expect_true("dplyr" %in% unlist(config$packages$prebuilt))
  })
})

test_that("collection_packages returns a webrarian_packages data frame", {
  withr::with_tempdir({
    suppressMessages(catalog(packages = c("dplyr", "ggplot2")))
    suppressMessages(acquire_package("user/repo", source = "github"))

    pkgs <- suppressMessages(collection_packages())

    expect_s3_class(pkgs, "webrarian_packages")
    expect_s3_class(pkgs, "data.frame")
    expect_named(pkgs, c("package", "source"))
    expect_equal(nrow(pkgs), 3L)
    expect_true(all(c("dplyr", "ggplot2", "user/repo") %in% pkgs$package))
    expect_equal(
      pkgs$source[pkgs$package == "user/repo"],
      "github"
    )
  })
})

test_that("collection_packages returns an empty frame when nothing configured", {
  withr::with_tempdir({
    suppressMessages(catalog())

    pkgs <- suppressMessages(collection_packages())
    expect_equal(nrow(pkgs), 0L)
    expect_true(all(c("package", "source") %in% names(pkgs)))
  })
})

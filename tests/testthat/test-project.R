test_that("catalog creates valid project structure", {
  withr::with_tempdir({
    suppressMessages(catalog("test-project"))

    expect_true(fs::dir_exists("test-project"))
    expect_true(fs::file_exists("test-project/_webrarian.yml"))
    expect_true(is_collection("test-project"))
  })
})

test_that("catalog with packages adds them to config", {
  withr::with_tempdir({
    suppressMessages(catalog(packages = c("dplyr", "ggplot2")))

    config <- collection_settings()
    expect_equal(unlist(config$packages$prebuilt), c("dplyr", "ggplot2"))
  })
})

test_that("catalog in current directory works", {
  withr::with_tempdir({
    suppressMessages(catalog())

    expect_true(fs::file_exists("_webrarian.yml"))
    expect_true(is_collection())
  })
})

test_that("catalog fails on existing project", {
  withr::with_tempdir({
    suppressMessages(catalog())

    expect_error(
      catalog(),
      "already a webrarian collection"
    )
  })
})

test_that("is_collection returns FALSE for non-projects", {
  withr::with_tempdir({
    expect_false(is_collection())
  })
})

test_that("collection_root finds project root from subdirectory", {
  withr::with_tempdir({
    suppressMessages(catalog("myproject"))
    fs::dir_create("myproject/subdir/nested")

    root <- collection_root("myproject/subdir/nested")
    expect_equal(fs::path_file(root), "myproject")
  })
})

test_that("collection_root returns NULL when no project", {
  withr::with_tempdir({
    expect_null(collection_root())
  })
})

test_that("data-analysis template includes packages and directories", {
  withr::with_tempdir({
    suppressMessages(catalog(template = "data-analysis"))

    config <- collection_settings()
    expect_true("dplyr" %in% unlist(config$packages$prebuilt))
    expect_true("ggplot2" %in% unlist(config$packages$prebuilt))
    expect_true(fs::dir_exists("data"))
    expect_true(fs::dir_exists("scripts"))
  })
})

# --- Package detection tests ---

test_that("catalog detects dependencies from existing R files", {
  skip_if_not_installed("renv")

  withr::with_tempdir({
    # Create directory with R files
    fs::dir_create("existing-project")
    writeLines(
      c("library(dplyr)", "library(ggplot2)", "stringr::str_detect('a', 'a')"),
      "existing-project/analysis.R"
    )

    suppressMessages(catalog("existing-project"))

    config <- collection_settings("existing-project")
    pkgs <- unlist(config$packages$prebuilt)

    expect_true("dplyr" %in% pkgs)
    expect_true("ggplot2" %in% pkgs)
    expect_true("stringr" %in% pkgs)
  })
})

test_that("catalog works on current directory with R files", {
  skip_if_not_installed("renv")

  withr::with_tempdir({
    writeLines("library(jsonlite)", "script.R")

    suppressMessages(catalog())

    config <- collection_settings()
    expect_true("jsonlite" %in% unlist(config$packages$prebuilt))
  })
})

test_that("catalog merges detected and explicit packages", {
  skip_if_not_installed("renv")

  withr::with_tempdir({
    writeLines("library(dplyr)", "script.R")

    suppressMessages(catalog(packages = c("ggplot2")))

    config <- collection_settings()
    pkgs <- unlist(config$packages$prebuilt)
    expect_true("dplyr" %in% pkgs)
    expect_true("ggplot2" %in% pkgs)
  })
})

# --- File pattern detection tests ---

test_that("catalog detects file patterns from existing directory", {
  withr::with_tempdir({
    fs::dir_create("project/data")
    fs::dir_create("project/scripts")
    writeLines("x <- 1", "project/analysis.R")
    writeLines("a,b\n1,2", "project/data.csv")

    suppressMessages(catalog("project"))

    config <- collection_settings("project")
    includes <- unlist(config$files$include)

    expect_true("*.R" %in% includes)
    expect_true("data/" %in% includes)
    expect_true("scripts/" %in% includes)
  })
})

test_that("catalog detects Quarto and Rmd files", {
  withr::with_tempdir({
    writeLines("---\ntitle: test\n---", "report.qmd")
    writeLines("---\ntitle: test\n---", "analysis.Rmd")

    suppressMessages(catalog())

    config <- collection_settings()
    includes <- unlist(config$files$include)

    expect_true("*.qmd" %in% includes)
    expect_true("*.Rmd" %in% includes)
  })
})

# --- detect parameter tests ---

test_that("catalog with detect=FALSE skips auto-detection", {
  skip_if_not_installed("renv")

  withr::with_tempdir({
    writeLines("library(dplyr)", "script.R")
    fs::dir_create("data")

    suppressMessages(catalog(detect = FALSE))

    config <- collection_settings()
    expect_equal(length(config$packages$prebuilt), 0)
    expect_equal(length(config$files$include), 0)
  })
})

test_that("catalog on new directory does not detect (nothing to detect)", {
  withr::with_tempdir({
    suppressMessages(catalog("brand-new-project"))

    config <- collection_settings("brand-new-project")
    expect_equal(length(config$packages$prebuilt), 0)
    expect_equal(length(config$files$include), 0)
  })
})

# --- "Next steps" guidance ---

# catalog() never calls setwd(), so the printed steps have to stay correct for
# the directory the user is actually sitting in.
catalog_output <- function(...) {
  withr::local_options(
    cli.hyperlink = FALSE,
    cli.hyperlink_run = FALSE,
    cli.num_colors = 1,
    cli.width = 500
  )
  paste(testthat::capture_messages(catalog(...)), collapse = "")
}

test_that("catalog next steps stay bare when cataloging the current directory", {
  withr::with_tempdir({
    out <- catalog_output()

    expect_true(grepl("Next steps:", out, fixed = TRUE))
    expect_true(grepl("webrarian::bind()", out, fixed = TRUE))
    # The working directory already is the collection: nothing to move into
    expect_false(grepl("setwd(", out, fixed = TRUE))
    expect_false(grepl("Move into the collection", out, fixed = TRUE))
    expect_false(grepl("path = ", out, fixed = TRUE))
  })
})

test_that("catalog next steps tell the user to move into a new subdirectory", {
  withr::with_tempdir({
    out <- catalog_output("my-app")

    expect_true(grepl("Move into the collection", out, fixed = TRUE))
    expect_true(grepl("setwd(\"my-app\")", out, fixed = TRUE))
    # The move-in step must come before the calls that depend on it
    expect_lt(
      regexpr("setwd(", out, fixed = TRUE),
      regexpr("webrarian::acquire_package(", out, fixed = TRUE)
    )
    # ... and the `path` alternative is spelled out for people who stay put
    expect_true(grepl("webrarian::bind(path = \"my-app\")", out, fixed = TRUE))
  })
})

test_that("catalog next steps show an absolute path outside the working directory", {
  target <- fs::path(withr::local_tempdir(), "elsewhere")
  withr::local_dir(withr::local_tempdir())

  out <- catalog_output(target)

  expect_true(grepl("Move into the collection", out, fixed = TRUE))

  shown <- regmatches(out, regexpr("setwd\\(\"[^\"]*\"\\)", out))
  expect_length(shown, 1)
  shown <- sub("\"\\)$", "", sub("^setwd\\(\"", "", shown))
  expect_true(fs::is_absolute_path(shown))
  expect_equal(
    as.character(fs::path_real(shown)),
    as.character(fs::path_real(target))
  )
})

test_that("catalog() does not add tooling packages it detects", {
  skip_if_not_installed("renv")
  withr::with_tempdir({
    writeLines(c("library(dplyr)", "webrarian::bind()", "rmarkdown::render('x.Rmd')"), "deploy.R")
    suppressMessages(catalog())
    pkgs <- unlist(collection_settings()$packages$prebuilt)
    expect_true("dplyr" %in% pkgs)
    expect_false(any(c("webrarian", "rmarkdown") %in% pkgs))
  })
})

# --- Templates that do what they say ---

test_that("the data-analysis template runs its init script at startup, and bind() accepts it", {
  withr::with_tempdir({
    suppressMessages(catalog(template = "data-analysis", detect = FALSE))
    cfg <- collection_settings()
    expect_equal(cfg$repl$startup_script, "scripts/init.R")
    expect_match(readLines("scripts/init.R")[[1]], "runs once when the site opens", fixed = TRUE)

    suppressMessages(settings_set(".", "build.bundle-engine" = FALSE))
    expect_no_warning(suppressMessages(bind()))
    expect_equal(read_site_config("_site")[["startup-script"]], "/home/web_user/scripts/init.R")
  })
})

test_that("packages given to catalog() add to the template's instead of replacing them", {
  withr::with_tempdir({
    suppressMessages(catalog(packages = "janitor", template = "data-analysis", detect = FALSE))
    expect_setequal(
      unlist(collection_settings()$packages$prebuilt),
      c("dplyr", "ggplot2", "tidyr", "readr", "janitor")
    )
  })
})

test_that("the package template bundles and opens an example, not the package's own sources", {
  withr::with_tempdir({
    fs::dir_create(c("R", "src", "data"))
    writeLines("f <- function() 1", "R/f.R")
    writeLines("x <- 1", "data/raw.R")
    writeLines(c("Package: mypkg", "Version: 0.1.0"), "DESCRIPTION")
    writeLines("^.*\\.Rproj$", ".Rbuildignore")
    suppressMessages(catalog(template = "package"))
    cfg <- collection_settings()
    expect_equal(unlist(cfg$packages$local), ".")
    # detect_file_patterns() would find R/, src/ and data/ here; the package
    # template bundles none of them.
    expect_equal(unlist(cfg$files$include), "examples/")
    expect_equal(unlist(cfg$repl$auto_open), "examples/example.R")
    expect_true("library(mypkg)" %in% readLines("examples/example.R"))
    expect_true("^examples$" %in% readLines(".Rbuildignore"))
    expect_equal(
      as.character(resolve_file_patterns(
        cfg$files$include,
        cfg$files$exclude,
        ".",
        output_dir = "_site"
      )),
      "examples/example.R"
    )
    # The page opens on the example: resolve_repl_files() accepts the entry.
    repl <- resolve_repl_files(cfg, "examples/example.R")
    expect_equal(repl$auto_open, "examples/example.R")
  })
})

test_that("the package template detects only what its examples use, never the package itself", {
  skip_if_not_installed("renv")
  withr::with_tempdir({
    fs::dir_create(c("R", "examples"))
    writeLines("f <- function() cli::cli_text('x')", "R/f.R")
    writeLines(c("Package: mypkg", "Version: 0.1.0", "Imports: cli"), "DESCRIPTION")
    writeLines(c("library(mypkg)", "library(glue)"), "examples/demo.R")
    suppressMessages(catalog(template = "package"))
    cfg <- collection_settings()
    expect_equal(unlist(cfg$packages$prebuilt), "glue")
    expect_setequal(
      as.character(resolve_file_patterns(
        cfg$files$include,
        cfg$files$exclude,
        ".",
        output_dir = "_site"
      )),
      c("examples/demo.R", "examples/example.R")
    )
  })
})

test_that("the package name for the example falls back to the directory, then to mypackage", {
  base <- withr::local_tempdir()
  named <- fs::path(base, "tidyish")
  fs::dir_create(named)
  expect_equal(local_package_name(named), "tidyish")
  dashed <- fs::path(base, "my-lib")
  fs::dir_create(dashed)
  expect_equal(local_package_name(dashed), "mypackage")
  writeLines(c("Package: realname", "Version: 1.0"), fs::path(dashed, "DESCRIPTION"))
  expect_equal(local_package_name(dashed), "realname")
})

test_that("catalog() takes the shared arguments and names the project after its directory", {
  expect_identical(
    formals_text(catalog),
    c(
      path = "\".\"",
      template = "c(\"minimal\", \"data-analysis\", \"package\")",
      packages = "NULL",
      detect = "TRUE"
    )
  )
  base <- withr::local_tempdir()
  suppressMessages(catalog(fs::path(base, "field-notes"), detect = FALSE))
  expect_equal(collection_settings(fs::path(base, "field-notes"))$project$name, "field-notes")
  expect_false(exists("rstudio_available", envir = asNamespace("webrarian"), inherits = FALSE))
})

test_that("catalog() informs, and does not warn, when the optional renv is absent", {
  local_mocked_bindings(renv_available = function() FALSE)
  dir <- withr::local_tempdir()
  writeLines("library(dplyr)", fs::path(dir, "analysis.R"))
  expect_no_warning(msgs <- testthat::capture_messages(catalog(dir)))
  expect_true(any(grepl("renv", msgs, fixed = TRUE)))
})

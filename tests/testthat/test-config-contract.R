# The wire config webrarian emits must satisfy the schema exlibris ships,
# vendored at inst/viewer-config.schema.json (draft 2020-12,
# additionalProperties: false). These validate real output, including a real
# bind(), not a hand-written fake.

schema_path <- function() {
  system.file("viewer-config.schema.json", package = "webrarian")
}

expect_valid_wire <- function(wire, label = "") {
  json <- viewer_config_json(wire)
  result <- jsonvalidate::json_validate(json, schema_path(), engine = "ajv", verbose = TRUE)
  expect_true(
    as.vector(result),
    info = paste(
      label,
      paste(utils::capture.output(print(attr(result, "errors"))), collapse = "\n")
    )
  )
}

contract_config <- function(repl = NULL) {
  apply_config_defaults(list(
    project = list(name = "demo"),
    webr = list(version = "0.6.0"),
    repl = repl
  ))
}

contract_packages <- list(
  install = c("dplyr", "ggplot2"),
  repos = "https://custom-wasm-repo.org",
  repo_url = "./repo"
)

test_that("the vendored schema ships with the package", {
  path <- schema_path()
  expect_true(nzchar(path))
  expect_true(file.exists(path))
})

test_that("build_viewer_config() output validates in every share-links mode", {
  skip_if_not_installed("jsonvalidate")
  for (mode in c("open", "fixed", "off")) {
    wire <- build_viewer_config(
      contract_config(list(share_links = mode, auto_run = list("a.R"), startup_script = "a.R")),
      c("a.R", "data/x y.csv"),
      "./",
      contract_packages
    )
    expect_valid_wire(wire)
  }
})

test_that("an empty collection's config validates", {
  skip_if_not_installed("jsonvalidate")
  wire <- build_viewer_config(
    contract_config(list(auto_open = list())),
    character(),
    "https://webr.r-wasm.org/v0.6.0/",
    list(install = character(), repos = character(), repo_url = "https://repo.r-wasm.org")
  )
  expect_valid_wire(wire)
})

test_that("the config a real bind() writes validates", {
  skip_if_not_installed("jsonvalidate")
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "files.include" = list("precious.R"),
    "repl.share-links" = "fixed"
  ))
  suppressMessages(bind(root))
  expect_valid_wire(read_site_config(fs::path(root, "_site")))
})

test_that("a config with the wrong schema-version is rejected by the vendored schema", {
  skip_if_not_installed("jsonvalidate")
  wire <- build_viewer_config(contract_config(), "a.R", "./", contract_packages)
  wire[["schema-version"]] <- 2L
  result <- jsonvalidate::json_validate(viewer_config_json(wire), schema_path(), engine = "ajv")
  expect_false(result)
})

test_that("an unrecognized top-level field is rejected (additionalProperties: false)", {
  skip_if_not_installed("jsonvalidate")
  wire <- build_viewer_config(contract_config(), "a.R", "./", contract_packages)
  wire[["unexpected-field"]] <- "surprise"
  result <- jsonvalidate::json_validate(viewer_config_json(wire), schema_path(), engine = "ajv")
  expect_false(result)
})

test_that("an unrecognized files[] field is rejected (nested additionalProperties: false)", {
  skip_if_not_installed("jsonvalidate")
  wire <- build_viewer_config(contract_config(), "a.R", "./", contract_packages)
  wire$files[[1]][["is-r-file"]] <- TRUE
  result <- jsonvalidate::json_validate(viewer_config_json(wire), schema_path(), engine = "ajv")
  expect_false(result)
})

test_that("an offline config validates, with or without a package source", {
  skip_if_not_installed("jsonvalidate")
  bundled <- build_viewer_config(
    contract_config(),
    "a.R",
    "./",
    list(install = "dplyr", repos = character(), repo_url = "./repo"),
    offline = TRUE
  )
  expect_true(bundled$offline)
  expect_valid_wire(bundled)

  bare <- build_viewer_config(
    contract_config(),
    "a.R",
    "./",
    list(install = character(), repos = character(), repo_url = NULL),
    offline = TRUE
  )
  expect_false("repo-url" %in% names(bare$packages))
  expect_valid_wire(bare)

  online <- build_viewer_config(contract_config(), "a.R", "./", contract_packages)
  expect_false("offline" %in% names(online))
})

test_that("the config every example builds satisfies the vendored schema", {
  skip_if_not_installed("jsonvalidate")
  for (name in c("basic", "branded", "data-analysis")) {
    path <- suppressMessages(collection_example(name))
    # The engine from the webR CDN and packages installed by the page: no download.
    suppressMessages(settings_set(path, "build.bundle-engine" = FALSE))
    suppressMessages(bind(path))
    expect_valid_wire(read_site_config(fs::path(path, "_site")), name)
  }
})

test_that("the local-package example's config satisfies the vendored schema", {
  skip_if_not_installed("jsonvalidate")
  local_fake_docker("ok")
  path <- suppressMessages(collection_example("local-package"))
  suppressMessages(settings_set(path, "build.bundle-engine" = FALSE))
  suppressMessages(bind(path))
  expect_valid_wire(read_site_config(fs::path(path, "_site")), "local-package")
})

test_that("an offline site's config satisfies the vendored schema", {
  skip_if_not_installed("jsonvalidate")
  local_fake_engine()
  root <- local_collection()
  suppressMessages(bind(root, offline = TRUE))
  wire <- read_site_config(fs::path(root, "_site"))
  expect_true(wire$offline)
  expect_valid_wire(wire, "offline")
})

test_that("the mirror page's config satisfies the vendored schema", {
  skip_if_not_installed("jsonvalidate")
  dir <- withr::local_tempdir()
  suppressMessages(generate_mirror_html(
    dir,
    list(webr_version = "0.6.0", page_title = "M", share_links = "off")
  ))
  html <- paste(readLines(fs::path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_valid_wire(read_inline_viewer_config(html), "mirror")
})

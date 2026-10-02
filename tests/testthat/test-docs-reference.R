# The key reference and the annotated template are generated from
# config_spec() by tools/config-reference.R, so they list exactly the keys
# webrarian reads.

# Callers skip first when the source tree is absent (R CMD check); in the
# source tree the generator must exist.
reference_tool <- function() {
  path <- repo_path("tools", "config-reference.R")
  if (!file.exists(path)) {
    stop("tools/config-reference.R is missing")
  }
  tool <- new.env()
  sys.source(path, envir = tool)
  tool
}

test_that("vignettes/config-reference.qmd is what the generator writes", {
  skip_if_no_source_tree()
  tool <- reference_tool()
  expected <- tool$reference_article_lines(config_spec())
  actual <- readLines(
    repo_path("vignettes", "config-reference.qmd"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  expect_identical(actual, expected, info = "Run Rscript tools/config-reference.R")
})

test_that("inst/templates/_webrarian.yml is what the generator writes", {
  skip_if_no_source_tree()
  tool <- reference_tool()
  actual <- readLines(
    repo_path("inst", "templates", "_webrarian.yml"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  expect_identical(
    actual,
    tool$reference_template_lines(config_spec()),
    info = "Run Rscript tools/config-reference.R"
  )
})

test_that("the reference names every key", {
  skip_if_no_source_tree()
  text <- paste(
    readLines(repo_path("vignettes", "config-reference.qmd"), warn = FALSE),
    collapse = "\n"
  )
  keys <- vapply(config_spec(), `[[`, character(1), "key")
  for (key in keys) {
    expect_match(
      text,
      paste0("`", gsub("_", "-", key, fixed = TRUE), "`"),
      fixed = TRUE,
      info = key
    )
  }
})

test_that("the annotated template is a complete, valid _webrarian.yml", {
  path <- system.file("templates", "_webrarian.yml", package = "webrarian")
  raw <- config_keys_to_snake(yaml::read_yaml(path))
  expect_no_error(validate_config(raw, strict = TRUE))
  for (entry in config_spec()) {
    keys <- strsplit(entry$key, ".", fixed = TRUE)[[1]]
    parent <- if (length(keys) > 1L) raw[[keys[-length(keys)]]] else raw
    expect_true(keys[[length(keys)]] %in% names(parent), info = entry$key)
  }
})

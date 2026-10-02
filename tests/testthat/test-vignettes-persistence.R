# The user guide covers the persistence and library-image features and the Shiny answer.

vignette_text <- function(name) {
  path <- test_path("..", "..", "vignettes", name)
  skip_if_not(file.exists(path), "vignettes/ is not in this tree")
  gsub("\\s+", " ", paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
}

test_that("the packages guide covers library images, the drift report and Shiny", {
  text <- vignette_text("packages.qmd")
  for (s in c(
    "## Faster page loads: the package library image",
    "library-image: false",
    "`build.offline: false`",
    "## Checking bundled versions against CRAN",
    "packages.json",
    "webrarian.cran_repo",
    "## Shiny apps",
    "shinylive"
  )) {
    expect_true(grepl(s, text, fixed = TRUE), info = s)
  }
})

test_that("the customization guide covers persistence and embedding", {
  text <- vignette_text("customization.qmd")
  for (s in c(
    "## Keeping visitors' edits",
    "persist-edits: false",
    "any code that runs there can read them",
    "## Embedding a site in another page",
    "exlibris-embed/1",
    "<iframe"
  )) {
    expect_true(grepl(s, text, fixed = TRUE), info = s)
  }
})

test_that("the settings the guides show are valid _webrarian.yml", {
  for (snippet in c("repl:\n  persist-edits: false", "build:\n  library-image: false")) {
    expect_no_warning(validate_config(config_keys_to_snake(yaml::yaml.load(snippet))))
  }
})

# The pkgdown site indexes every article and groups the settings functions
# together.

pkgdown_config <- function() {
  path <- repo_path("_pkgdown.yml")
  skip_if_not(file.exists(path), "_pkgdown.yml is not in this tree")
  yaml::read_yaml(path)
}

test_that("the settings functions share one reference group", {
  skip_if_no_source_tree()
  groups <- pkgdown_config()$reference
  settings <- Filter(function(g) identical(g$title, "Settings"), groups)
  expect_length(settings, 1L)
  expect_identical(
    unlist(settings[[1]]$contents),
    c("collection_settings", "settings_get", "settings_set")
  )
})

test_that("the Articles menu links every vignette and nothing else", {
  skip_if_no_source_tree()
  menu <- pkgdown_config()$navbar$components$articles$menu
  hrefs <- unlist(lapply(menu, `[[`, "href"))
  linked <- sub("^articles/(.*)\\.html$", "\\1", hrefs)
  vignettes <- sub("\\.qmd$", "", list.files(repo_path("vignettes"), pattern = "\\.qmd$"))
  expect_setequal(linked, vignettes)
})

test_that("the navbar and the home page link the live demo", {
  skip_if_no_source_tree()
  config <- pkgdown_config()
  expect_true("demo" %in% unlist(config$navbar$structure$left))
  expect_identical(config$navbar$components$demo$href, "demo/")
  for (file in c("README.qmd", "README.md")) {
    text <- readLines(repo_path(file), warn = FALSE)
    expect_true(
      any(grepl("https://coatless-wasm.github.io/webrarian/demo/", text, fixed = TRUE)),
      info = file
    )
  }
})

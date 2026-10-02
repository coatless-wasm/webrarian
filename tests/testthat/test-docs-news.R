# NEWS.md describes the release DESCRIPTION declares, names its main
# functions and settings, and points to the articles for details.

news_text <- function() {
  path <- repo_path("NEWS.md")
  skip_if_not(file.exists(path), "NEWS.md is not in this tree")
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

test_that("NEWS.md opens with the version in DESCRIPTION, once", {
  skip_if_no_source_tree()
  news <- news_text()
  version <- read.dcf(repo_path("DESCRIPTION"), fields = "Version")[1, 1]
  expect_identical(news[[1]], paste("# webrarian", version))
  expect_identical(sum(startsWith(news, "# webrarian ")), 1L)
})

test_that("NEWS.md names the main functions and settings, and none of the removed ones", {
  skip_if_no_source_tree()
  text <- paste(news_text(), collapse = "\n")
  main <- c(
    "catalog",
    "bind",
    "acquire_package",
    "acquire_file",
    "collection_example",
    "collection_mirror",
    "circulate_via_github",
    "circulate_via_netlify",
    "check_inventory",
    "diagnose_collection"
  )
  for (fn in main) {
    expect_true(fn %in% namespace_exports(), info = fn)
    expect_match(text, paste0("`", fn, "()`"), fixed = TRUE, info = fn)
  }
  expect_match(text, "`reading_room(watch = TRUE)`", fixed = TRUE)
  removed <- c(
    "catalog_files",
    "catalog_packages",
    "watch_circulation",
    "circulation_config",
    "check_requirements",
    "read_brand_yml",
    "webr_cache_path",
    "webr_assets_",
    "bundle_webr"
  )
  for (old in removed) {
    expect_false(grepl(old, text, fixed = TRUE), info = old)
  }
  topics <- c(
    "_webrarian.yml",
    "lowercase, hyphenated keys",
    "packages.repos",
    "Docker",
    "repl.share-links",
    "brand.yml",
    "build.offline",
    "LICENSES/",
    "experimental"
  )
  for (topic in topics) {
    expect_match(text, topic, fixed = TRUE, info = topic)
  }
})

# NEWS points to the articles for details; every article it names exists.
test_that("the articles NEWS.md points to exist", {
  skip_if_no_source_tree()
  text <- paste(news_text(), collapse = "\n")
  named <- regmatches(text, gregexpr('vignette\\("[a-z-]+"\\)', text))[[1]]
  expect_gt(length(named), 0L)
  for (name in sub('^vignette\\("([a-z-]+)"\\)$', "\\1", named)) {
    expect_true(file.exists(repo_path("vignettes", paste0(name, ".qmd"))), info = name)
  }
})

# One maturity statement for the joint first release: experimental before
# 1.0, as the README's badge says and as pyodidarian's "Development Status ::
# 3 - Alpha" and its README say.
test_that("NEWS.md says the API is experimental, as the README's badge does", {
  skip_if_no_source_tree()
  text <- gsub("\\s+", " ", paste(news_text(), collapse = " "))
  expect_match(text, "The API is experimental before 1.0", fixed = TRUE)
  expect_match(text, "deprecation warnings where practical", fixed = TRUE)
  expect_false(grepl("API is stable", text, ignore.case = TRUE))
  readme <- paste(
    readLines(repo_path("README.qmd"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  expect_match(readme, "lifecycle-experimental", fixed = TRUE)
  expect_false(grepl("lifecycle-stable", readme, fixed = TRUE))
})

# Community files. Source tree only: .github/ is not in the built
# package.

community_file <- function(name) {
  dir <- test_path("..", "..", ".github")
  skip_if_not(dir.exists(dir), ".github/ is not in this tree")
  path <- file.path(dir, name)
  expect_true(file.exists(path), info = name)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

maintainer_email <- function() {
  # R CMD check runs the tests from webrarian.Rcheck/tests/testthat, where
  # ../../DESCRIPTION does not exist: skip there, as community_file() does.
  description <- test_path("..", "..", "DESCRIPTION")
  skip_if_not(file.exists(description), "DESCRIPTION is not in this tree")
  authors <- read.dcf(description, fields = "Authors@R")[[1]]
  people <- unclass(eval(parse(text = authors)))
  Filter(function(p) "cre" %in% p$role, people)[[1]]$email
}

test_that("the code of conduct and the security policy name the maintainer", {
  # First, so the test skips outside the source tree before anything reads a file.
  conduct <- community_file("CODE_OF_CONDUCT.md")
  # One identity in every project (shared across the projects).
  expect_identical(maintainer_email(), "james.balamuta@gmail.com")
  expect_match(conduct, "Contributor Covenant", fixed = TRUE)
  expect_match(conduct, "version 2.1", fixed = TRUE)
  expect_match(conduct, maintainer_email(), fixed = TRUE)
  security <- community_file("SECURITY.md")
  expect_match(
    security,
    "https://github.com/coatless-wasm/webrarian/security/advisories/new",
    fixed = TRUE
  )
  expect_match(security, maintainer_email(), fixed = TRUE)
})

test_that("the contribution guide describes every R file and the maintenance tools", {
  guide <- community_file("CONTRIBUTING.md")
  r_files <- paste0("R/", list.files(test_path("..", "..", "R"), pattern = "\\.R$"))
  for (f in r_files) {
    expect_match(guide, paste0("`", f, "`"), fixed = TRUE, info = f)
  }
  for (tool in c(
    "tools/vendor-exlibris.sh",
    "tools/config-reference.R",
    "tools/measure-sizes.R",
    "tools/ci-test.R"
  )) {
    expect_match(guide, tool, fixed = TRUE, info = tool)
  }
  expect_match(guide, "NOT_CRAN=true Rscript -e 'devtools::test()'", fixed = TRUE)
  expect_no_match(guide, "docs/superpowers", fixed = TRUE)
  expect_no_match(guide, "CLAUDE", fixed = TRUE)
  # pkgdown renders this file at the site's root, where a link relative to
  # .github/ (SECURITY.md, ../NEWS.md) would break: every link is absolute.
  expect_no_match(guide, "\\]\\((?!https?://|#)", perl = TRUE)
})

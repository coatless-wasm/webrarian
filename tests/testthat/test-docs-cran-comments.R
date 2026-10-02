# cran-comments.md states nothing the package contradicts. The
# check results are regenerated at submission; these are the claims that
# must stay true in between.

test_that("cran-comments.md matches the package", {
  path <- repo_path("cran-comments.md")
  skip_if_not(file.exists(path), "cran-comments.md is not in this tree")
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  version <- read.dcf(repo_path("DESCRIPTION"), fields = "Version")[1, 1]
  expect_match(text, paste("new submission of webrarian", version), fixed = TRUE)

  stale <- c(
    "No external service is contacted",
    "macOS 15,",
    "41 npm packages",
    "is not vendored",
    "not required for any of the package's R code paths",
    "only when a user explicitly",
    "licence"
  )
  for (claim in stale) {
    expect_false(grepl(claim, text, fixed = TRUE), info = claim)
  }
  expect_match(text, "\\donttest{}", fixed = TRUE)
  expect_match(text, "path = \".\"", fixed = TRUE)
  expect_match(text, "SystemRequirements", fixed = TRUE)
  # check_inventory() also asks CRAN, and the tests switch that off.
  expect_match(text, "webrarian.cran_repo = FALSE", fixed = TRUE)
  expect_match(text, "build.bundle-engine", fixed = TRUE)

  # The NOTE quotes the maintainer DESCRIPTION names, who is the one the
  # 2026-09-25 identity decision names.
  authors <- eval(parse(text = read.dcf(repo_path("DESCRIPTION"), fields = "Authors@R")[1, 1]))
  cre <- Filter(function(i) "cre" %in% authors[i]$role, seq_along(authors))
  expect_length(cre, 1L)
  maintainer <- format(authors[cre[[1]]], include = c("given", "family", "email"))
  expect_identical(maintainer, "James Balamuta <james.balamuta@gmail.com>")
  # R CMD check quotes the maintainer with sQuote(): curly quotes in a UTF-8
  # session, straight ones under LC_ALL=C. The file copies the log as printed,
  # so fold the curly quotes before matching.
  text <- chartr("‘’", "''", text)
  expect_match(text, sprintf("Maintainer: '%s'", maintainer), fixed = TRUE)
})

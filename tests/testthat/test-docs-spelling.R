# US spelling in the documentation, with technical words in inst/WORDLIST. spelling reads DESCRIPTION, man/, the vignettes, README and
# NEWS.

test_that("the documentation has no spelling errors", {
  skip_if_no_source_tree()
  skip_if_not_installed("spelling")
  found <- spelling::spell_check_package(repo_path())
  expect_identical(found$word, character(), info = paste(found$word, collapse = ", "))
})

test_that("the word list holds no British spellings", {
  skip_if_no_source_tree()
  words <- tolower(readLines(repo_path("inst", "WORDLIST"), warn = FALSE, encoding = "UTF-8"))
  british <- c(
    "analyse",
    "artefact",
    "artefacts",
    "behaviour",
    "behaviours",
    "catalogue",
    "centre",
    "colour",
    "colours",
    "customisation",
    "customise",
    "favour",
    "honour",
    "initialise",
    "licence",
    "licences",
    "normalise",
    "normalised",
    "optimise",
    "organisation",
    "recognise",
    "recognised",
    "serialise",
    "summarise"
  )
  expect_identical(intersect(words, british), character())
})

# American spelling in everything the package tracks, not only the documentation
# the spelling package reads: R code and its messages, roxygen, comments,
# templates, tools, tests, vignettes, man/ and the notices (maintainer decision
# of 2026-09-28). Quoted third-party text keeps its own spelling.
british_spelling_pattern <- paste0(
  "(?i)(licenc|colour|behaviour|favour|honour|flavour|labell(ed|ing)|cancell(ed|ing)",
  "|centred|centre|artefact|catalogue|judgement|whilst|amongst|\\bgrey",
  "|modell(ed|ing)|travell(ed|ing)|misspelt|analys(e|ed|es|ing)(?![a-z])",
  "|(summar|initial|recogn|serial|normal|neutral|custom|standard|organ|optim|canonical",
  "|visual|special|capital|minim|maxim|prioriti|author|categor|character|util|synchron",
  "|sanit|emphas|general|random|local|final|memor|parameter|token|stabil|vector|harmon",
  "|trivial|central|synthes|real)is(e|es|ed|ing|ation|ations)(?![a-z]))"
)

# Upstream strings kept as they are: the ARIA attribute, the file name webR's
# package.json names, and the heading of webR's MIT text, quoted verbatim.
british_spelling_exempt_text <- c("aria-labelledby", "LICENCE.md", "## The MIT Licence")

british_spelling_exempt_file <- function(path) {
  # Verbatim third-party notices and the minified bundle (webR's and jszip's
  # own strings), and this file, which holds the pattern and the word list.
  grepl("^inst/viewer/THIRD-PARTY[^/]*\\.md$", path) ||
    path %in% c("inst/viewer/exlibris-r.js", "tests/testthat/test-docs-spelling.R")
}

test_that("the package's tracked text uses American spelling", {
  skip_if_no_source_tree()
  root <- normalizePath(repo_path(), mustWork = TRUE)
  skip_if(!nzchar(Sys.which("git")), "git is not installed")
  files <- suppressWarnings(system2(
    "git",
    c(
      "-C",
      shQuote(root),
      "ls-files",
      "--",
      "R",
      "inst",
      "tools",
      "tests",
      "vignettes",
      "man",
      "README.qmd",
      "NEWS.md",
      "LICENSE.note"
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  skip_if(!is.null(attr(files, "status")) || length(files) == 0L, "not a git checkout")
  found <- character()
  for (path in files) {
    if (british_spelling_exempt_file(path)) {
      next
    }
    full <- file.path(root, path)
    if (!file.exists(full)) {
      next
    }
    bytes <- readBin(full, "raw", file.info(full)$size)
    if (any(bytes == as.raw(0L))) {
      next
    }
    lines <- strsplit(rawToChar(bytes), "\r?\n", useBytes = TRUE)[[1]]
    for (text in british_spelling_exempt_text) {
      lines <- gsub(text, "", lines, fixed = TRUE, useBytes = TRUE)
    }
    hits <- which(grepl(british_spelling_pattern, lines, perl = TRUE, useBytes = TRUE))
    # cran-comments' stale-claim guard lists "licence" on purpose.
    if (identical(path, "tests/testthat/test-docs-cran-comments.R")) {
      hits <- hits[trimws(lines[hits]) != '"licence"']
    }
    found <- c(found, sprintf("%s:%d: %s", path, hits, trimws(lines[hits])))
  }
  expect(length(found) == 0L, paste0("British spellings found:\n", paste(found, collapse = "\n")))
})

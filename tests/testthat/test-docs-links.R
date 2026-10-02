# Links between the articles, and from the README to them, lead to an article
# and a section that exist.

# The section ids pandoc gives the headings among `lines`: the explicit
# {#id} where a heading has one, otherwise pandoc's auto_identifiers (drop the
# attribute block, drop what is not a letter, digit, space, _, - or .,
# lower-case, spaces to hyphens, and drop everything before the first letter).
heading_ids_from_lines <- function(lines) {
  heads <- grep("^#{1,6} ", lines, value = TRUE)
  text <- sub("\\s*\\{[^}]*\\}\\s*$", "", sub("^#+\\s+", "", heads))
  ids <- gsub("[^a-z0-9 _.-]", "", tolower(text))
  ids <- sub("^[^a-z]+", "", gsub(" +", "-", trimws(ids)))
  explicit <- "^.*\\{#([A-Za-z0-9_-]+)[^}]*\\}\\s*$"
  has_id <- grepl(explicit, heads)
  ids[has_id] <- sub(explicit, "\\1", heads[has_id])
  ids
}

heading_ids <- function(file) heading_ids_from_lines(prose_lines(file))

# The ids pandoc 3.10 (Quarto 1.10.18) gave these headings on 2026-09-25; the two
# link tests below are only as good as this function.
test_that("heading ids are the ones pandoc gives", {
  lines <- c(
    "## Faster page loads: the package library image",
    "Prose, not a heading.",
    "## Keeping visitors' edits",
    "### X {#custom}",
    "## What is in `_site/`",
    "## Panels {.unnumbered}"
  )
  expect_identical(
    heading_ids_from_lines(lines),
    c(
      "faster-page-loads-the-package-library-image",
      "keeping-visitors-edits",
      "custom",
      "what-is-in-_site",
      "panels"
    )
  )
  expect_identical(heading_ids_from_lines("No heading here"), character())
})

test_that("links to articles lead to an article and a section that exist", {
  skip_if_no_source_tree()
  pattern <- "\\]\\((https://coatless-wasm\\.github\\.io/webrarian/articles/)?[a-z-]+\\.html(#[A-Za-z0-9_-]+)?\\)"
  for (file in doc_sources()) {
    text <- paste(prose_lines(file), collapse = "\n")
    for (link in regmatches(text, gregexpr(pattern, text))[[1]]) {
      target <- sub("^.*?([a-z-]+)\\.html.*$", "\\1", link)
      vignette <- file.path("vignettes", paste0(target, ".qmd"))
      expect_true(file.exists(repo_path(vignette)), info = paste(file, link))
      if (grepl("#", link, fixed = TRUE) && file.exists(repo_path(vignette))) {
        anchor <- sub("^.*#([A-Za-z0-9_-]+)\\)$", "\\1", link)
        expect_true(anchor %in% heading_ids(vignette), info = paste(file, link))
      }
    }
  }
})

# [Without Docker](#without-docker) and the like: a link to a section of the
# page it is on breaks just the same when that heading is renamed.
test_that("links to a section of the same page lead to a section that exists", {
  skip_if_no_source_tree()
  for (file in doc_sources()) {
    text <- paste(prose_lines(file), collapse = "\n")
    ids <- heading_ids(file)
    for (link in regmatches(text, gregexpr("\\]\\(#[A-Za-z0-9_-]+\\)", text))[[1]]) {
      anchor <- sub("^\\]\\(#([A-Za-z0-9_-]+)\\)$", "\\1", link)
      expect_true(anchor %in% ids, info = paste(file, link))
    }
  }
})

# urlchecker (run on the vignettes' code) rejects any CRAN mirror URL, cloud.r-project.org
# included, as "CRAN URL not in canonical form": a vignette names the CRAN host instead.
test_that("a vignette's R code names no CRAN mirror", {
  skip_if_no_source_tree()
  for (file in grep("^vignettes/", doc_sources(), value = TRUE)) {
    for (block in fenced_blocks(file)) {
      if (block$lang != "r") {
        next
      }
      expect_false(
        any(grepl("cloud\\.r-project\\.org", block$lines)),
        info = paste(file, block$line)
      )
    }
  }
})

# Include patterns skip dot names unless the pattern spells the dot (the same
# rule as pyodidarian), and the
# Bundling Files article says so, so a reader knows why `.` leaves out
# .Renviron and how to bundle a dot file on purpose.

test_that("broad includes skip dot files, and a pattern that spells the dot selects one", {
  root <- withr::local_tempdir()
  for (f in c(".Rprofile", "data/.env", "data/a.csv")) {
    dir.create(dirname(file.path(root, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", file.path(root, f))
  }
  for (pattern in c(".", "data/")) {
    got <- as.character(resolve_file_patterns(pattern, NULL, root))
    expect_false(any(c(".Rprofile", "data/.env") %in% got), info = pattern)
    expect_true("data/a.csv" %in% got, info = pattern)
  }
  expect_identical(as.character(resolve_file_patterns(".Rprofile", NULL, root)), ".Rprofile")
})

test_that("the Bundling Files article states the dot-name rule", {
  skip_if_no_source_tree()
  lines <- readLines(repo_path("vignettes", "files.qmd"), warn = FALSE, encoding = "UTF-8")
  text <- gsub("\\s+", " ", paste(lines, collapse = " "))
  expect_true(
    grepl("selected only by a pattern that spells the dot", text, fixed = TRUE),
    info = "vignettes/files.qmd does not say that only a pattern spelling the dot selects a dot file"
  )
  expect_false(any(grepl("| `.` | every file in the collection |", lines, fixed = TRUE)))
})

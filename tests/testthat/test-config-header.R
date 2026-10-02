# Every _webrarian.yml webrarian writes starts with a pointer to the key
# reference and a warning that rewriting drops comments.

test_that("a new collection's _webrarian.yml starts with the header", {
  root <- local_collection()
  lines <- readLines(fs::path(root, "_webrarian.yml"), warn = FALSE)
  header <- config_file_header()
  expect_identical(lines[seq_along(header)], header)
  expect_match(
    paste(header, collapse = " "),
    'vignette("config-reference", package = "webrarian")',
    fixed = TRUE
  )
  expect_match(paste(header, collapse = " "), "comments", fixed = TRUE)
})

test_that("rewriting the file keeps one header and drops other comments", {
  root <- local_collection()
  path <- fs::path(root, "_webrarian.yml")
  writeLines(c(readLines(path, warn = FALSE), "# a note of my own"), path)

  suppressMessages(settings_set(root, "project.name" = "renamed"))

  lines <- readLines(path, warn = FALSE)
  expect_identical(sum(lines == config_file_header()[[1]]), 1L)
  expect_false("# a note of my own" %in% lines)
  expect_identical(collection_settings(root)$project$name, "renamed")
})

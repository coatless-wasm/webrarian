# ?bind and the customization article document share links, and the CSS
# hooks the article names exist.

test_that("?bind documents what share links may do", {
  rd <- repo_path("man", "bind.Rd")
  skip_if_not(file.exists(rd), "man/ is not in this tree")
  text <- paste(readLines(rd, warn = FALSE), collapse = "\n")
  expect_match(text, "\\section{Share links}", fixed = TRUE)
  expect_match(text, "repl.share-links", fixed = TRUE)
  for (mode in c("\"open\"", "\"fixed\"", "\"off\"")) {
    expect_match(text, mode, fixed = TRUE)
  }
  # The startup script is always the site's own (a shared rule with pyodidarian).
  expect_match(
    gsub("\\s+", " ", text),
    "never removes the site's packages or replaces its startup script",
    fixed = TRUE
  )
})

test_that("the CSS variables the customization article sets are the ones branding writes", {
  skip_if_no_source_tree()
  article <- readLines(repo_path("vignettes", "customization.qmd"), warn = FALSE)
  css <- unlist(lapply(
    Filter(function(b) identical(b$lang, "css"), fenced_blocks("vignettes/customization.qmd")),
    `[[`,
    "lines"
  ))
  documented <- unique(sub(
    "^\\s*(--[a-z-]+):.*$",
    "\\1",
    grep("^\\s*--[a-z-]+:", css, value = TRUE)
  ))
  expect_true(length(documented) >= 6L)
  written <- paste(
    unlist(brand_css_variables(finish_brand(list(
      color = list(primary = "#18BC9C", background = "#1a1a2e", foreground = "#eaeaea"),
      typography = list(base = "Inter", monospace = "JetBrains Mono")
    )))),
    collapse = "\n"
  )
  for (v in documented) {
    expect_match(written, paste0(v, ":"), fixed = TRUE, info = v)
  }
  for (hook in c("#webrarian-loading", ".webrarian-spinner")) {
    expect_true(any(grepl(hook, article, fixed = TRUE)), info = hook)
    expect_match(
      paste(readLines(repo_path("R", "html.R"), warn = FALSE), collapse = "\n"),
      sub("^[#.]", "", hook),
      fixed = TRUE
    )
  }
})

# Both tools theme a brand the same way (a rule shared with pyodidarian): the
# surface and text colors of a light palette apply in light mode only, and a dark palette applies in both. The article says so, and
# brand_css_variables() writes it.
test_that("the article's dark-mode rule for brand palettes is the one branding writes", {
  skip_if_no_source_tree()
  lines <- readLines(repo_path("vignettes", "customization.qmd"), warn = FALSE, encoding = "UTF-8")
  text <- gsub("\\s+", " ", paste(lines, collapse = " "))
  expect_match(text, "apply in light mode only", fixed = TRUE)
  expect_match(text, "the whole palette applies in both modes", fixed = TRUE)
  css <- function(color) {
    paste(unlist(brand_css_variables(finish_brand(list(color = color)))), collapse = "\n")
  }
  light <- css(list(primary = "#18BC9C", background = "#FFFFFF", foreground = "#2C3E50"))
  dark <- css(list(primary = "#18BC9C", background = "#1a1a2e", foreground = "#eaeaea"))
  expect_match(light, "prefers-color-scheme: light", fixed = TRUE)
  expect_false(grepl("prefers-color-scheme", dark, fixed = TRUE))
})

# ui.loading.custom-html replaces the splash's contents, not the splash: the
# container and its status line stay, as in pyodidarian. The article says it in the words of
# config_spec(), which the key reference prints, and pyodidarian uses the same.
test_that("the article describes ui.loading.custom-html in config_spec()'s words", {
  skip_if_no_source_tree()
  lines <- readLines(repo_path("vignettes", "customization.qmd"), warn = FALSE, encoding = "UTF-8")
  text <- gsub("\\s+", " ", paste(lines, collapse = " "))
  entry <- Filter(function(e) identical(e$key, "ui.loading.custom_html"), config_spec())
  expect_length(entry, 1L)
  doc <- entry[[1]]$doc
  expect_identical(
    doc,
    paste(
      "HTML that replaces the loading screen's contents. The screen and its status line,",
      "where a load failure and the Reload button appear, stay."
    )
  )
  expect_match(text, doc, fixed = TRUE)
  expect_false(grepl("replace the whole loading screen", text, fixed = TRUE))
})

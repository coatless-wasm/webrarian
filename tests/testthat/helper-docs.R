# Helpers for the documentation tests (test-docs-*.R). They read the source
# tree (README.qmd, vignettes/, tools/, .github/), which R CMD check does not
# put beside the tests, so every documentation test skips there and runs under
# devtools::test() and in CI.

# A path in the package's source tree.
repo_path <- function(...) test_path("..", "..", ...)

skip_if_no_source_tree <- function() {
  skip_if_not(dir.exists(repo_path("vignettes")), "the source tree is not beside the tests")
}

# Documentation sources, relative to the package root: the README source,
# every vignette and every example's README.
doc_sources <- function() {
  vignettes <- sort(list.files(repo_path("vignettes"), pattern = "\\.qmd$"))
  examples <- sort(list.files(repo_path("inst", "examples")))
  files <- c(
    "README.qmd",
    file.path("vignettes", vignettes),
    file.path("inst", "examples", examples, "README.md")
  )
  files[file.exists(repo_path(files))]
}

# The fenced code blocks of a Markdown or Quarto file.
#
# Returns a list of list(lang, classes, info, line, lines): `lang` is the first
# word of the info string without braces or a leading dot ("r", "yaml",
# "text"), `classes` every word that started with a dot (```{.text .message}
# gives "text" and "message"), `info` the info string as written (chunk options
# such as ```{r eval=TRUE} live there), `line` the line number of the opening
# fence and `lines` the block's content.
fenced_blocks <- function(file) {
  text <- readLines(repo_path(file), warn = FALSE, encoding = "UTF-8")
  blocks <- list()
  open <- NULL
  for (i in seq_along(text)) {
    line <- text[[i]]
    if (is.null(open)) {
      m <- regmatches(line, regexec("^\\s*(`{3,})\\s*(.*)$", line))[[1]]
      if (length(m) == 0L) {
        next
      }
      info <- trimws(gsub("[{}]", " ", m[[3]]))
      words <- strsplit(info, "[[:space:],]+")[[1]]
      words <- words[nzchar(words)]
      open <- list(
        fence = m[[2]],
        lang = if (length(words) > 0L) tolower(sub("^\\.", "", words[[1]])) else "",
        classes = sub("^\\.", "", words[startsWith(words, ".")]),
        info = m[[3]],
        line = i,
        lines = character()
      )
    } else if (grepl(paste0("^\\s*", open$fence, "`*\\s*$"), line)) {
      blocks[[length(blocks) + 1L]] <- open[c("lang", "classes", "info", "line", "lines")]
      open <- NULL
    } else {
      open$lines <- c(open$lines, line)
    }
  }
  blocks
}

# A function from webrarian's namespace.
webrarian_function <- function(name) get(name, envir = asNamespace("webrarian"))

# The lines of a documentation file outside its fenced code blocks.
prose_lines <- function(file) {
  lines <- readLines(repo_path(file), warn = FALSE, encoding = "UTF-8")
  in_code <- logical(length(lines))
  for (block in fenced_blocks(file)) {
    in_code[block$line + seq_len(length(block$lines) + 2L) - 1L] <- TRUE
  }
  lines[!in_code]
}

# NULL when `key` names a setting webrarian reads, else what is wrong with it.
setting_key_problem <- function(key) {
  if (!is.character(key) || length(key) != 1L) {
    return(NULL)
  }
  tryCatch(
    {
      segments <- normalize_setting_key(key)
      if (!identical(segments[[1]], "brand")) {
        validate_config(set_nested_value(list(), segments, NULL), strict = TRUE)
      }
      NULL
    },
    error = function(e) sprintf("setting %s: %s", key, conditionMessage(e))
  )
}

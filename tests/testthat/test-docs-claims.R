# Facts the documentation states are read from the package, not typed in
# (every doc named an exlibris tag that was no longer vendored).

test_that("the pin chunk falls back to the source tree when the package is not installed", {
  skip_if_no_source_tree()
  chunk <- Filter(
    function(b) identical(b$lang, "r") && any(grepl("^#\\| label: pin$", b$lines)),
    fenced_blocks("vignettes/ecosystem.qmd")
  )[[1]]$lines
  code <- paste(chunk, collapse = "\n")
  expect_match(
    code,
    'if (!nzchar(prov)) prov <- file.path("..", "inst", "viewer", "PROVENANCE.json")',
    fixed = TRUE
  )
  expect_match(code, "jsonlite::read_json(prov)", fixed = TRUE)
})

test_that("the ecosystem article reads the exlibris pin from PROVENANCE.json", {
  skip_if_no_source_tree()
  chunks <- Filter(
    function(b) identical(b$lang, "r") && any(grepl("^#\\| label: pin$", b$lines)),
    fenced_blocks("vignettes/ecosystem.qmd")
  )
  expect_length(chunks, 1L)
  env <- new.env()
  eval(parse(text = chunks[[1]]$lines), envir = env)
  provenance <- jsonlite::read_json(system.file("viewer", "PROVENANCE.json", package = "webrarian"))
  expect_identical(env$pin$exlibrisCommit, provenance$exlibrisCommit)
  expect_true(nzchar(env$pin$webrClientVersion))

  text <- paste(readLines(repo_path("vignettes", "ecosystem.qmd"), warn = FALSE), collapse = "\n")
  for (field in c("pin$exlibrisRef", "pin_commit", "pin$webrClientVersion")) {
    expect_match(text, paste0("`r ", field, "`"), fixed = TRUE)
  }
})

test_that("the ecosystem article presents pyodidarian as the Python counterpart, released separately", {
  skip_if_no_source_tree()
  text <- gsub(
    "\\s+",
    " ",
    paste(readLines(repo_path("vignettes", "ecosystem.qmd"), warn = FALSE), collapse = "\n")
  )
  expect_match(
    text,
    "**pyodidarian**, released separately, does for Python and Pyodide",
    fixed = TRUE
  )
  expect_false(grepl("Node command line", text, fixed = TRUE))
})

# exlibris and pyodidarian are not public yet: the documentation names them
# but links neither their repositories nor a PyPI page.
test_that("no documentation links to the unreleased exlibris or pyodidarian", {
  skip_if_no_source_tree()
  for (file in c(doc_sources(), "README.md")) {
    text <- paste(readLines(repo_path(file), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_false(grepl("github\\.com/coatless-wasm/(exlibris|pyodidarian)", text), info = file)
    expect_false(grepl("pypi.org", text, fixed = TRUE), info = file)
    expect_false(grepl("pip install pyodidarian", text, fixed = TRUE), info = file)
  }
  readme <- gsub("\\s+", " ", paste(prose_lines("README.qmd"), collapse = " "))
  expect_match(readme, "pyodidarian, the Python counterpart, is released separately", fixed = TRUE)
})

# pyodidarian's public API accepts a few R-only differences, which
# webrarian's docs must note. There are six: open_browser, the data
# frame, acquire_package()'s warning, webr_cache_clear()'s question,
# collection_mirror()'s return value and diagnose_collection()'s areas, each an
# [R] note in the API table.
test_that("the ecosystem article lists where webrarian's API differs from pyodidarian's", {
  skip_if_no_source_tree()
  text <- gsub(
    "\\s+",
    " ",
    paste(readLines(repo_path("vignettes", "ecosystem.qmd"), warn = FALSE), collapse = "\n")
  )
  differences <- c(
    "`upgrade`",
    "`favicon`",
    "`block = TRUE`",
    "`open_browser`",
    "data frame",
    "`acquire_package()` adds a prebuilt package that no repository has",
    "`webr_cache_clear()` asks before clearing everything",
    "`collection_mirror()` returns the mirror's directory, invisibly",
    "`diagnose_collection()` files each problem under an `area`"
  )
  for (name in differences) {
    expect_match(text, name, fixed = TRUE, info = name)
  }
})

# The four differences the code decides are read from the functions, so a
# later change to either the code or the article shows here.
test_that("the ecosystem article's API differences match the functions", {
  skip_if_no_source_tree()
  text <- gsub(
    "\\s+",
    " ",
    paste(readLines(repo_path("vignettes", "ecosystem.qmd"), warn = FALSE), collapse = "\n")
  )

  # diagnose_collection() files problems through its local report(level, area,
  # message); the areas are the literal second arguments of those calls.
  areas <- character()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible())
    }
    if (identical(e[[1]], quote(report)) && length(e) >= 3L && is.character(e[[3]])) {
      areas <<- c(areas, e[[3]])
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
  }
  walk(body(diagnose_collection))
  expect_true(length(areas) > 0L)
  bullet <- regmatches(text, regexpr("- `diagnose_collection\\(\\)` [^.]*\\.", text))
  expect_length(bullet, 1L)
  named <- character()
  if (length(bullet) == 1L) {
    named <- gsub('`"|"`', "", regmatches(bullet, gregexpr('`"[a-z]+"`', bullet))[[1]])
  }
  expect_setequal(named, unique(areas))

  # collection_mirror() ends by returning its dest, invisibly.
  last <- utils::tail(as.list(body(collection_mirror)), 1L)[[1]]
  expect_identical(last, quote(invisible(dest)))

  # webr_cache_clear() asks through ask_yes_no(); acquire_package() looks
  # prebuilt names up through warn_unavailable_prebuilt().
  expect_true("ask_yes_no" %in% all.names(body(webr_cache_clear)))
  expect_true("warn_unavailable_prebuilt" %in% all.names(body(acquire_package)))
})

test_that("no documentation names an exlibris tag or commit by hand", {
  skip_if_no_source_tree()
  for (file in c(doc_sources(), "NEWS.md", "cran-comments.md")) {
    if (!file.exists(repo_path(file))) {
      next
    }
    text <- paste(readLines(repo_path(file), warn = FALSE), collapse = "\n")
    expect_false(grepl("vendored-20[0-9]{2}-", text), info = file)
    expect_false(grepl("\\b[0-9a-f]{40}\\b", text), info = file)
  }
})

# Vignettes run under R CMD check on CRAN. None may build a site, download or
# write: every chunk is shown, not run, except the chunk above that reads
# PROVENANCE.json, and inline R appears only where it prints that pin.

# A chunk option that turns evaluation on, in a chunk header (```{r eval=TRUE},
# ```{r, eval = T}), a `#|` line (eval: true, eval: TRUE, eval: yes,
# eval: !expr TRUE) or the front matter (execute: eval: true), in any case.
eval_on_pattern <- "(?i)\\beval\\s*[:=]\\s*(!expr\\s+)?(true|t|yes|on)\\b"

# The knitr::opts_chunk$set() calls in `lines` whose eval is anything but a
# literal FALSE, counted; a later opts_chunk$set(eval = TRUE) would turn every
# chunk after it on.
opts_eval_not_false <- function(lines) {
  exprs <- tryCatch(parse(text = lines, keep.source = FALSE), error = function(e) expression())
  found <- 0L
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible())
    }
    if (grepl("opts_chunk\\$set$", paste(deparse(e[[1]]), collapse = ""))) {
      value <- as.list(e)[["eval"]]
      if (!is.null(value) && !identical(value, FALSE) && !identical(value, quote(F))) {
        found <<- found + 1L
      }
    }
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
  }
  for (e in exprs) {
    walk(e)
  }
  found
}

# The two detectors above catch what they must and nothing else, so the test
# below cannot pass by missing a chunk that runs.
test_that("the evaluated-code check recognizes every way to turn evaluation on", {
  on <- c(
    "{r eval=TRUE}",
    "r, eval = T",
    "#| eval: true",
    "#| eval: !expr TRUE",
    "  eval: yes",
    "#| EVAL: True"
  )
  expect_true(all(grepl(eval_on_pattern, on, perl = TRUE)))
  off <- c(
    "{r eval=FALSE}",
    "#| eval: false",
    "#| eval: !expr FALSE",
    "#| label: evaluation",
    "  eval: no"
  )
  expect_false(any(grepl(eval_on_pattern, off, perl = TRUE)))
  expect_identical(opts_eval_not_false("knitr::opts_chunk$set(eval = TRUE)"), 1L)
  expect_identical(opts_eval_not_false("opts_chunk$set(eval = !interactive())"), 1L)
  expect_identical(opts_eval_not_false("knitr::opts_chunk$set(eval = FALSE)"), 0L)
  expect_identical(
    opts_eval_not_false(c("knitr::opts_chunk$set(", "  collapse = TRUE,", "  eval = F", ")")),
    0L
  )
})

test_that("vignettes evaluate nothing but the ecosystem article's provenance chunk", {
  skip_if_no_source_tree()
  for (f in list.files(repo_path("vignettes"), pattern = "\\.qmd$")) {
    file <- file.path("vignettes", f)
    text <- readLines(repo_path(file), warn = FALSE, encoding = "UTF-8")
    end <- match("---", text[-1L])
    if (identical(text[[1]], "---") && !is.na(end)) {
      front <- text[seq_len(end + 1L)]
      expect_false(any(grepl(eval_on_pattern, front, perl = TRUE)), info = paste(f, "front matter"))
    }
    r_blocks <- Filter(function(b) identical(b$lang, "r"), fenced_blocks(file))
    if (length(r_blocks) > 0L) {
      expect_true(any(grepl("eval = FALSE", text, fixed = TRUE)), info = f)
    }
    for (block in r_blocks) {
      where <- sprintf("%s:%d", f, block$line)
      expect_identical(
        opts_eval_not_false(block$lines),
        0L,
        info = paste(where, "sets eval to something other than FALSE")
      )
      options <- c(block$info, grep("^#\\|", block$lines, value = TRUE))
      if (!any(grepl(eval_on_pattern, options, perl = TRUE))) {
        next
      }
      expect_identical(file, "vignettes/ecosystem.qmd", info = where)
      expect_true(any(grepl("^#\\| label: pin$", block$lines)), info = where)
      expect_false(
        any(grepl(
          "webrarian::|\\b(bind|catalog|acquire_[a-z]+|settings_set|writeLines|download\\.file)\\(",
          block$lines
        )),
        info = where
      )
    }
    inline <- unlist(regmatches(text, gregexpr("`r [^`]+`", text)))
    if (!identical(f, "ecosystem.qmd")) {
      expect_identical(inline, character(), info = f)
    }
  }
})

test_that("the when-to-use article compares webrarian with the other webR tools", {
  skip_if_no_source_tree()
  path <- repo_path("vignettes", "when-to-use.qmd")
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (url in c(
    "https://posit-dev.github.io/r-shinylive/",
    "https://r-wasm.github.io/quarto-live/",
    "https://webr.r-wasm.org/latest/",
    "https://jupyterlite.readthedocs.io/"
  )) {
    expect_match(text, url, fixed = TRUE)
  }
  expect_match(text, "collection_mirror()", fixed = TRUE)
})

# Claims known to be false.

stale_phrases <- c(
  "bundle_webr",
  "__VIEWER_ALLOW_URL_OVERRIDE__",
  "R.bin.data",
  "tar.gz archive",
  "100 MB",
  "~30 MB",
  "30MB",
  "1.4 MB in total",
  "catalog_files",
  "catalog_packages",
  "watch_circulation",
  "circulation_config",
  "check_requirements",
  "read_brand_yml",
  "webr_cache_path",
  "webr_assets_",
  "startup_code",
  "startup-code",
  "evalRVoid",
  "Chrome is most reliable",
  "localhost, not",
  "rocker/r-ver:4.3",
  "R >= 4.1",
  "workflows/deploy.yml",
  "open = FALSE",
  "cname:",
  "custom_css",
  "tar archive",
  # The offline default the 2026-09-25 decisions reversed.
  "the default, `build.offline: true`",
  "`build.offline: true`, the default",
  "default `build.offline: true`",
  "offline (default)",
  "Offline (the default)",
  "searches only `repo/`"
)

# Key names from before 0.1.0, which no page should mention.
old_keys <- c(
  "allow-url-override",
  "auto-open-r-files",
  "auto-run-files",
  "show-editor",
  "show-terminal",
  "show-files",
  "show-plot",
  "clear-untitled",
  "base-path"
)

test_that("no documentation repeats a claim known to be false", {
  skip_if_no_source_tree()
  for (file in doc_sources()) {
    text <- paste(readLines(repo_path(file), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (phrase in stale_phrases) {
      expect_false(grepl(phrase, text, fixed = TRUE), info = paste(file, phrase))
    }
    for (key in old_keys) {
      expect_false(grepl(key, text, fixed = TRUE), info = paste(file, key))
    }
  }
})

test_that("the documentation names only webR versions this webrarian offers", {
  skip_if_no_source_tree()
  offered <- vapply(webr_versions_table()$versions, `[[`, character(1), "version")
  pattern <- "(webR |webr:v|webr/v|webr_version = \"|version: \")[0-9]+\\.[0-9]+\\.[0-9]+"
  for (file in doc_sources()) {
    text <- paste(readLines(repo_path(file), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    found <- regmatches(text, gregexpr(pattern, text))[[1]]
    versions <- sub("^.*?([0-9]+\\.[0-9]+\\.[0-9]+)$", "\\1", found)
    expect_identical(setdiff(versions, offered), character(), info = file)
  }
})

# Files named as inst/viewer/<file>, or read with system.file("viewer", <file>).
viewer_files_named <- function(text) {
  paths <- regmatches(text, gregexpr("inst/viewer/[A-Za-z0-9._-]*[A-Za-z0-9_-]", text))[[1]]
  calls <- regmatches(text, gregexpr('system\\.file\\("viewer", "[^"]+"', text))[[1]]
  unique(c(
    sub("^inst/viewer/", "", paths),
    sub('^system\\.file\\("viewer", "([^"]+)"$', "\\1", calls)
  ))
}

test_that("files the documentation names in inst/viewer exist", {
  skip_if_no_source_tree()
  for (file in c(doc_sources(), "cran-comments.md")) {
    if (!file.exists(repo_path(file))) {
      next
    }
    text <- paste(readLines(repo_path(file), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (name in viewer_files_named(text)) {
      expect_true(
        file.exists(repo_path("inst", "viewer", name)),
        info = paste(file, "names inst/viewer/", name)
      )
    }
  }
})

test_that("every image in vignettes/images is shown by a vignette", {
  skip_if_no_source_tree()
  vignettes <- list.files(repo_path("vignettes"), pattern = "\\.qmd$", full.names = TRUE)
  text <- paste(unlist(lapply(vignettes, readLines, warn = FALSE)), collapse = "\n")
  for (image in list.files(repo_path("vignettes", "images"))) {
    expect_match(text, paste0("images/", image), fixed = TRUE, info = image)
  }
})

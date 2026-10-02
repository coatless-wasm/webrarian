# The README states the install command, the R version DESCRIPTION requires,
# the demo, reference and license facts, and README.md is rendered from
# README.qmd.

# The README's prose, on one line.
readme_prose <- function(file = "README.qmd") {
  gsub("\\s+", " ", paste(prose_lines(file), collapse = " "))
}

# The package is on GitHub only until it reaches r-universe or CRAN.
test_that("the README installs from GitHub", {
  skip_if_no_source_tree()
  blocks <- Filter(function(b) identical(b$lang, "r"), fenced_blocks("README.qmd"))
  code <- unlist(lapply(blocks, `[[`, "lines"))
  expect_true(any(grepl('pak::pak("coatless-wasm/webrarian")', code, fixed = TRUE)))
  expect_false(any(grepl("install.packages(", code, fixed = TRUE)))
  text <- readme_prose()
  expect_match(text, 'remotes::install_github("coatless-wasm/webrarian")', fixed = TRUE)
  expect_false(grepl("r-universe.dev", text, fixed = TRUE))
})

test_that("the README links the reference and states the license terms", {
  skip_if_no_source_tree()
  for (file in c("README.qmd", "README.md")) {
    text <- gsub(
      "\\s+",
      " ",
      paste(readLines(repo_path(file), warn = FALSE, encoding = "UTF-8"), collapse = " ")
    )
    expect_match(
      text,
      "https://coatless-wasm.github.io/webrarian/reference/",
      fixed = TRUE,
      info = file
    )
    expect_match(text, "AGPL-3", fixed = TRUE, info = file)
    expect_match(text, "/blob/main/LICENSE.note)", fixed = TRUE, info = file)
    expect_match(text, "/blob/main/inst/viewer/THIRD-PARTY.md)", fixed = TRUE, info = file)
  }
  expect_match(
    readme_prose(),
    "the sites and mirrors webrarian generates are yours to license",
    fixed = TRUE
  )
})

test_that("the README states the R version DESCRIPTION requires", {
  skip_if_no_source_tree()
  depends <- read.dcf(repo_path("DESCRIPTION"), fields = "Depends")[1, 1]
  floor <- sub("^.*R \\(>= ([0-9.]+)\\).*$", "\\1", gsub("\\s+", " ", depends))
  text <- paste(prose_lines("README.qmd"), collapse = "\n")
  expect_match(text, paste0("R >= ", floor), fixed = TRUE)
  others <- regmatches(text, gregexpr("R >= [0-9]+(\\.[0-9]+)*", text))[[1]]
  expect_identical(unique(others), paste0("R >= ", floor))
})

# The words of a README outside its YAML front matter and its code blocks, in
# order. quarto render re-wraps lines, escapes characters (R \>= 4.4) and
# turns ' into a curly apostrophe, none of which changes a word made of
# letters, digits and dots, so README.md and README.qmd give the same vector
# exactly when README.md was rendered from the current README.qmd.
readme_words <- function(file) {
  lines <- prose_lines(file)
  if (length(lines) > 0L && identical(lines[[1]], "---")) {
    end <- match("---", lines[-1L])
    if (!is.na(end)) lines <- lines[-seq_len(end + 1L)]
  }
  text <- gsub("…", "...", paste(lines, collapse = "\n"), fixed = TRUE)
  regmatches(text, gregexpr("[A-Za-z0-9.]+", text))[[1]]
}

test_that("README.md is rendered from README.qmd", {
  skip_if_no_source_tree()
  rendered <- readme_words("README.md")
  source <- readme_words("README.qmd")
  first <- match(TRUE, c(rendered, "") != c(source, "")[seq_len(length(rendered) + 1L)])
  expect_identical(
    rendered,
    source,
    info = sprintf(
      "README.md differs from README.qmd from word %d (\"%s\" against \"%s\"): run quarto render README.qmd",
      first,
      rendered[first],
      source[first]
    )
  )
})

# One maturity statement for the joint first release: webrarian's API is
# experimental before 1.0, as NEWS.md says, and pyodidarian, released with it,
# declares "Development Status :: 3 - Alpha" and says so in its README.
test_that("the README's lifecycle badge says the API is experimental, as NEWS does", {
  skip_if_no_source_tree()
  text <- paste(
    readLines(repo_path("README.qmd"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  expect_match(text, "lifecycle-experimental", fixed = TRUE)
  expect_false(grepl("lifecycle-stable", text, fixed = TRUE))
})

# The quick start is the five lines from nothing to a preview, then a
# sentence each for files and deployment.
test_that("the README's quick start catalogs, adds a package, binds and previews", {
  skip_if_no_source_tree()
  blocks <- Filter(function(b) identical(b$lang, "r"), fenced_blocks("README.qmd"))
  quick <- Filter(function(b) any(b$lines == "#| label: quick-start"), blocks)
  expect_length(quick, 1L)
  code <- setdiff(quick[[1]]$lines, "#| label: quick-start")
  expect_identical(
    code,
    c(
      "library(webrarian)",
      'catalog("my-site")',
      'acquire_package("dplyr", path = "my-site")',
      'bind("my-site")',
      'reading_room("my-site")'
    )
  )
  text <- readme_prose()
  expect_match(text, "`acquire_file()`", fixed = TRUE)
  expect_match(text, "`circulate_via_github()`", fixed = TRUE)
  code <- unlist(lapply(blocks, `[[`, "lines"))
  expect_true(any(grepl("collection_example(", code, fixed = TRUE)))
})

# The example names are shared with pyodidarian (parent spec 2026-09-25,
# "Shared API alignment"), and collection_example() takes the directory name.
test_that("the README names the shipped examples", {
  skip_if_no_source_tree()
  text <- gsub("\\s+", " ", paste(prose_lines("README.qmd"), collapse = " "))
  sentence <- regmatches(text, regexpr("The examples are [^.]*\\.", text))
  expect_length(sentence, 1L)
  named <- gsub("[`\"]", "", unlist(regmatches(sentence, gregexpr("`\"[a-z-]+\"`", sentence))))
  expect_setequal(named, list.files(repo_path("inst", "examples")))
})

# The alt text of the README's hero image (the <img class="wr-hero"> tag).
readme_hero_alt <- function() {
  text <- paste(
    readLines(repo_path("README.qmd"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  m <- regmatches(text, regexec('<img class="wr-hero"[^>]*? alt="([^"]*)"', text, perl = TRUE))[[1]]
  if (length(m) == 0L) NA_character_ else m[[2]]
}

test_that("the hero images say what the README's alt text says and name the hashed bundle", {
  skip_if_no_source_tree()
  alt <- readme_hero_alt()
  expect_false(is.na(alt))
  svgs <- lapply(c("hero-light.svg", "hero-dark.svg"), function(f) {
    readLines(repo_path("man", "figures", f), warn = FALSE, encoding = "UTF-8")
  })
  for (i in seq_along(svgs)) {
    text <- paste(svgs[[i]], collapse = "\n")
    where <- c("hero-light.svg", "hero-dark.svg")[[i]]
    desc <- regmatches(text, regexec('<desc id="d">([^<]*)</desc>', text))[[1]]
    expect_identical(desc[2], alt, info = where)
    for (name in c(">exlibris-r.&lt;hash&gt;.js<", ">exlibris-r.&lt;hash&gt;.css<")) {
      expect_true(grepl(name, text, fixed = TRUE), info = paste(where, "lacks", name))
    }
    for (name in c(">exlibris-r.js<", ">exlibris-r.css<")) {
      expect_false(grepl(name, text, fixed = TRUE), info = paste(where, "still lists", name))
    }
  }
  # The light and dark files differ only in their color rules.
  without_style <- function(svg) svg[!grepl("^\\s*<style>", svg)]
  expect_identical(without_style(svgs[[1]]), without_style(svgs[[2]]))
})

# The animated hex logo leads the README (and so the site's home page), light
# by default and dark under a dark color scheme; extra.css swaps the site's
# header logo with the site's own theme and keeps pkgdown from repeating it in
# the header of every other page.
test_that("the README heading carries the animated hex logo, and only the home page shows it", {
  skip_if_no_source_tree()
  for (file in c("README.qmd", "README.md")) {
    lines <- readLines(repo_path(file), warn = FALSE, encoding = "UTF-8")
    heading <- grep("^# webrarian", lines, value = TRUE)
    expect_length(heading, 1L)
    expect_match(
      heading,
      'href="https://coatless-wasm.github.io/webrarian/"',
      fixed = TRUE,
      info = file
    )
    expect_match(heading, 'src="man/figures/logo.svg"', fixed = TRUE, info = file)
    expect_match(
      heading,
      '<source media="(prefers-color-scheme: dark)" srcset="man/figures/webrarian-logo-dark-animated.svg">',
      fixed = TRUE,
      info = file
    )
  }
  figure <- function(name) readLines(repo_path("man", "figures", name), warn = FALSE)
  for (name in c("logo.svg", "webrarian-logo-dark-animated.svg")) {
    expect_true(any(grepl("@keyframes", figure(name), fixed = TRUE)), info = name)
  }
  css <- paste(readLines(repo_path("pkgdown", "extra.css"), warn = FALSE), collapse = "\n")
  expect_match(
    css,
    '[data-bs-theme="dark"] img.logo {\n  content: url("reference/figures/webrarian-logo-dark-animated.svg");',
    fixed = TRUE
  )
  expect_match(css, ".container:not(.template-home) .page-header > img.logo", fixed = TRUE)
  # pkgdown strips only a bare h1 > a > img logo, not one inside <picture>.
  expect_match(css, ".page-header > h1 > a > picture {\n  display: none;", fixed = TRUE)
})

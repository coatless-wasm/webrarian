# No dead code, unused imports or unused templates, and tests
# that stay out of the developer's own cache. The source-tree
# checks skip in R CMD check, where R/ is not beside the tests.

source_path <- function(...) test_path("..", "..", ...)

r_code_lines <- function() {
  files <- list.files(source_path("R"), pattern = "\\.R$", full.names = TRUE)
  code <- unlist(lapply(files, readLines, warn = FALSE))
  code[!grepl("^\\s*#", code)]
}

test_that("every function in R/ is exported, an S3 method, or called from R/", {
  skip_if_not(dir.exists(source_path("R")), "R/ is not in this tree")
  code <- r_code_lines()
  defs <- sub(
    "^([A-Za-z0-9._]+) <- function.*$",
    "\\1",
    grep("^[A-Za-z0-9._]+ <- function", code, value = TRUE)
  )
  candidates <- setdiff(defs, c(namespace_exports(), ".onLoad", ".onAttach", ".onUnload"))
  candidates <- candidates[!grepl("^(print|format|summary)\\.", candidates)]
  dead <- character()
  for (fn in candidates) {
    escaped <- gsub(".", "\\.", fn, fixed = TRUE)
    uses <- grep(sprintf("(^|[^A-Za-z0-9._])%s([^A-Za-z0-9._]|$)", escaped), code)
    definition <- grep(sprintf("^%s <- function", escaped), code)
    if (length(setdiff(uses, definition)) == 0L) dead <- c(dead, fn)
  }
  expect_identical(dead, character())
})

test_that("R/ defines no load hooks with nothing in them", {
  skip_if_not(dir.exists(source_path("R")), "R/ is not in this tree")
  expect_false(any(grepl("^\\.on(Load|Attach) <- function", r_code_lines())))
})

# The plan's Finish check ("Leftover names") greps for these and expects
# nothing under R/, comments included: a comment that names a removed
# function sends a maintainer looking for it. Each pattern brackets its first
# character, as in `ps | grep "[p]attern"`, so it does not match its own
# line: that grep scans tests/ too and expects only the tests that check a
# removed name is gone.
test_that("R/ names no function or argument the API freeze removed, not even in a comment", {
  skip_if_not(dir.exists(source_path("R")), "R/ is not in this tree")
  removed <- c(
    "[c]atalog_files",
    "[c]atalog_packages",
    "[w]atch_circulation",
    "[c]irculation_config",
    "[w]ebr_assets_install",
    "[o]pen = FALSE",
    "[b]undle_webr",
    "[b]rowse = ",
    "[p]rebuilt-packages",
    "[c]ustom-package",
    "[T]HIRD-PARTY-nested",
    "[b]alamut2",
    "[b]ind\\(output_dir"
  )
  hits <- character()
  for (file in list.files(source_path("R"), pattern = "\\.R$", full.names = TRUE)) {
    lines <- readLines(file, warn = FALSE)
    for (pattern in removed) {
      at <- grep(pattern, lines)
      hits <- c(hits, sprintf("R/%s:%d matches %s", basename(file), at, rep(pattern, length(at))))
    }
  }
  expect_identical(hits, character())
})

test_that("every package in Imports is used", {
  desc <- source_path("DESCRIPTION")
  skip_if_not(dir.exists(source_path("R")), "R/ is not in this tree")
  imports <- trimws(sub("\\(.*$", "", strsplit(read.dcf(desc, fields = "Imports")[1, 1], ",")[[1]]))
  code <- paste(r_code_lines(), collapse = "\n")
  ns <- paste(readLines(source_path("NAMESPACE"), warn = FALSE), collapse = "\n")
  for (pkg in imports) {
    used <- grepl(paste0(pkg, "::"), code, fixed = TRUE) ||
      grepl(sprintf("importFrom(%s,", pkg), ns, fixed = TRUE)
    expect_true(used, info = pkg)
  }
})

# Only an actual template read counts: "index.html" alone appears all over R/
# (fs::path(output_dir, "index.html")), so a bare name match would pass an
# unused template. Every read goes through template_path() (R/build.R) or render_template() (R/deploy.R); a direct
# system.file("templates", "<name>") also counts.
test_that("every template in inst/templates is read by R/", {
  skip_if_not(dir.exists(source_path("R")), "R/ is not in this tree")
  code <- paste(r_code_lines(), collapse = "\n")
  templates <- setdiff(list.files(source_path("inst", "templates")), "_webrarian.yml")
  for (t in templates) {
    name <- gsub(".", "\\.", t, fixed = TRUE)
    read <- sprintf(
      '(template_path|render_template)\\("%s"|system\\.file\\(\\s*"templates",\\s*"%s"',
      name,
      name
    )
    expect_true(grepl(read, code), info = t)
  }
})

test_that("a branded site carries no unused brand logo copies", {
  path <- suppressMessages(collection_example("branded"))
  suppressMessages(settings_set(path, "build.bundle-engine" = FALSE)) # downloads nothing
  suppressMessages(bind(path))
  site <- fs::path(path, "_site")
  expect_false(fs::dir_exists(fs::path(site, "assets", "brand")))
  expect_true(fs::dir_exists(fs::path(site, "assets", "custom")))
})

build_ignored <- function(path) {
  rules <- trimws(readLines(source_path(".Rbuildignore"), warn = FALSE))
  rules <- rules[nzchar(rules) & !startsWith(rules, "#")]
  # R CMD build matches each rule as a Perl regex, ignoring case, against every
  # file and directory path; an ignored directory takes its contents with it.
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  prefixes <- vapply(
    seq_along(parts),
    function(i) paste(parts[seq_len(i)], collapse = "/"),
    character(1)
  )
  any(vapply(
    rules,
    function(rule) any(grepl(rule, prefixes, perl = TRUE, ignore.case = TRUE)),
    logical(1)
  ))
}

test_that("files git ignores or nothing references stay out of the tarball", {
  skip_if_not(file.exists(source_path(".Rbuildignore")), ".Rbuildignore is not in this tree")
  for (path in c(
    "vignettes/_quarto.yaml",
    "vignettes/_quarto.yml",
    "man/figures/logo.png",
    "man/figures/webrarian-logo-light-animated.svg",
    "man/figures/webrarian-logo-light-static.svg",
    "man/figures/webrarian-logo-dark-static.svg",
    "air.toml",
    "tools/vendor-exlibris.sh"
  )) {
    expect_true(build_ignored(path), info = path)
  }
  for (path in c(
    "man/figures/logo.svg",
    "man/figures/webrarian-logo-dark-animated.svg",
    "man/figures/hero-light.svg",
    "man/figures/hero-dark.svg",
    "vignettes/getting-started.qmd",
    "inst/COPYRIGHTS",
    "LICENSE.note",
    "inst/OUTPUT-EXCEPTION.md",
    "inst/viewer/THIRD-PARTY.md",
    "inst/viewer/EXCEPTION.md"
  )) {
    expect_false(build_ignored(path), info = path)
  }
})

test_that("tests never write into the developer's own cache", {
  expect_true(nzchar(Sys.getenv("R_USER_CACHE_DIR")))
})

test_that("the repository's own community files stay out of the tarball", {
  skip_if_not(file.exists(source_path(".Rbuildignore")), ".Rbuildignore is not in this tree")
  for (path in c(".github/CONTRIBUTING.md", ".github/CODE_OF_CONDUCT.md", ".github/SECURITY.md")) {
    expect_true(build_ignored(path), info = path)
  }
})

# Local tool directories are excluded in .git/info/exclude on the maintainer's
# machine; the shared ignore files do not name them.
test_that("the ignore files do not list local tool directories", {
  for (name in c(".gitignore", ".Rbuildignore")) {
    path <- source_path(name)
    skip_if_not(file.exists(path), paste(name, "is not in this tree"))
    lines <- readLines(path, warn = FALSE)
    expect_false(any(grepl("claude|superpowers", lines, ignore.case = TRUE)), info = name)
  }
})

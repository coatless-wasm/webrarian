# Every YAML block in the documentation names the file it belongs to, and
# every block that is a _webrarian.yml or a _brand.yml is one webrarian reads
# without an error or a warning (snippets that aborted on
# underscores or used keys that never existed).

# The top-level sections of _webrarian.yml.
config_top_keys <- c("project", "webr", "packages", "files", "repl", "ui", "build", "brand")

# TRUE when a block is made only of _webrarian.yml sections but is not
# labeled as one. A block labeled with another YAML file's name, such as
# `# .gitlab-ci.yml` with a `build:` job, is that file and is left alone.
looks_like_unlabeled_settings <- function(label, top) {
  settings_file <- label %in% c("_webrarian.yml", "_brand.yml")
  other_yaml_file <- !settings_file && grepl("\\.ya?ml$", label)
  !settings_file && !other_yaml_file && length(top) > 0L && all(top %in% config_top_keys)
}

test_that("only blocks made of _webrarian.yml sections need the _webrarian.yml label", {
  expect_false(looks_like_unlabeled_settings(".gitlab-ci.yml", c("stages", "build", "pages")))
  expect_false(looks_like_unlabeled_settings(
    ".github/workflows/deploy.yml",
    c("name", "on", "jobs")
  ))
  expect_false(looks_like_unlabeled_settings("_webrarian.yml", c("project", "build")))
  expect_false(looks_like_unlabeled_settings(NA_character_, c("stages", "build")))
  expect_true(looks_like_unlabeled_settings(NA_character_, "repl"))
  expect_true(looks_like_unlabeled_settings("Example", c("ui", "brand")))
})

yaml_block_label <- function(block) {
  first <- if (length(block$lines) > 0L) block$lines[[1]] else ""
  m <- regmatches(first, regexec("^#\\s*([^\\s:]+)", first, perl = TRUE))[[1]]
  if (length(m) == 0L) NA_character_ else m[[2]]
}

yaml_problem <- function(code) {
  tryCatch(
    {
      force(code)
      NULL
    },
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
}

check_brand <- function(brand, root) {
  css <- brand_css_variables(load_brand(list(brand = brand), root))
  invisible(css)
}

# Read, validate strictly and apply the defaults, but do not bind(): a block
# names files and packages a scratch directory lacks, so a build would fail
# for reasons unrelated to its keys. test-bind-effects.R builds a site
# for every config_spec() key.
check_webrarian_yaml <- function(text, where) {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "_webrarian.yml")
  writeLines(text, path)
  problem <- yaml_problem({
    raw <- read_config_file(path)
    validate_config(raw, strict = TRUE)
    apply_config_defaults(raw %||% list(), dir)
    if (is.list(raw$brand)) check_brand(raw$brand, dir)
  })
  expect_null(problem, info = where)
}

check_brand_yaml <- function(text, where) {
  dir <- withr::local_tempdir()
  writeLines(text, file.path(dir, "_brand.yml"))
  problem <- yaml_problem(check_brand(NULL, dir))
  expect_null(problem, info = where)
}

for (file in doc_sources()) {
  if (!any(vapply(fenced_blocks(file), function(b) identical(b$lang, "yaml"), logical(1)))) {
    next
  }
  test_that(paste("every YAML block in", file, "names its file and is valid"), {
    skip_if_no_source_tree()
    for (block in fenced_blocks(file)) {
      if (!identical(block$lang, "yaml")) {
        next
      }
      where <- sprintf("%s:%d", file, block$line)
      label <- yaml_block_label(block)
      expect_false(
        is.na(label),
        info = paste(where, "does not start with a '# <file name>' comment")
      )
      text <- paste(block$lines, collapse = "\n")
      parsed <- tryCatch(yaml::yaml.load(text), error = function(e) e)
      expect_false(inherits(parsed, "error"), info = paste(where, "is not valid YAML"))
      if (inherits(parsed, "error")) {
        next
      }
      expect_false(
        looks_like_unlabeled_settings(label, names(parsed)),
        info = paste(where, "holds only _webrarian.yml keys but is not labeled '# _webrarian.yml'")
      )
      if (identical(label, "_webrarian.yml")) {
        check_webrarian_yaml(text, where)
      }
      if (identical(label, "_brand.yml")) check_brand_yaml(text, where)
    }
  })
}

# Setting names in the prose, such as `repl.share-links`, are real keys in the
# file's hyphenated spelling.
for (file in doc_sources()) {
  test_that(paste("setting names in", file, "are real keys, spelled as in the file"), {
    skip_if_no_source_tree()
    text <- paste(prose_lines(file), collapse = "\n")
    keys <- regmatches(
      text,
      gregexpr("`(project|webr|packages|files|repl|ui|build)\\.[A-Za-z0-9_.-]+`", text)
    )[[1]]
    keys <- unique(gsub("`", "", keys, fixed = TRUE))
    # A file name such as `packages.json` (the drift manifest) is not a setting.
    keys <- keys[!grepl("\\.(json|ya?ml|html?|js|css|tgz|md|qmd|R)$", keys)]
    for (key in keys) {
      expect_false(grepl("_", key, fixed = TRUE), info = paste(file, key))
      expect_null(setting_key_problem(key), info = paste(file, key))
    }
    succeed()
  })
}

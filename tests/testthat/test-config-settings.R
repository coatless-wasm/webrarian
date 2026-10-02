# settings_set()/settings_get() accept the file's own hyphenated spelling, the
# snake_case one, and dotted or slashed paths.

set_one <- function(dir, key, value) {
  suppressMessages(do.call(settings_set, c(list(dir), stats::setNames(list(value), key))))
}

test_that("settings_set accepts the file's hyphenated spelling without duplicating the key", {
  dir <- local_collection()
  set_one(dir, "build/output-dir", "docs")

  lines <- readLines(fs::path(dir, "_webrarian.yml"))
  expect_equal(sum(grepl("^\\s*output-dir:", lines)), 1L)
  expect_equal(collection_settings(dir)$build$output_dir, "docs")
})

test_that("dotted, slashed, hyphenated and snake spellings address the same key", {
  dir <- local_collection()
  keys <- c("build.output-dir", "build/output_dir", "build.output_dir", "build/output-dir")
  for (i in seq_along(keys)) {
    key <- keys[[i]]
    # A distinct value per spelling, so a spelling that silently writes nothing
    # cannot pass on the value an earlier iteration left behind.
    value <- paste0("out", i)
    set_one(dir, key, value)
    cfg <- collection_settings(dir)
    expect_equal(cfg$build$output_dir, value, info = key)
    expect_equal(settings_get(cfg, key), value, info = key)
  }
})

test_that("settings_get takes one key, in either spelling", {
  expect_identical(formals_text(settings_get), c(config = "", key = "", default = "NULL"))
  dir <- local_collection()
  cfg <- collection_settings(dir)
  expect_equal(settings_get(cfg, "build.output-dir"), "_site")
  expect_equal(settings_get(cfg, "build/output_dir"), "_site")
  expect_equal(settings_get(cfg, "files/mount-point"), "/home/web_user")
  expect_equal(settings_get(cfg, "build.nope", default = "d"), "d")
  expect_error(settings_get(cfg), "`key` is missing", fixed = TRUE)
})

test_that("the old separate-levels form warns with the key to write instead", {
  cfg <- collection_settings(local_collection())
  # The pre-0.1.0 call settings_get(cfg, "build", "output-dir"), built with
  # do.call() so Step 4's grep for leftover separate-level calls skips it.
  old_call <- list(cfg, "build", "output-dir")
  expect_warning(do.call(settings_get, old_call), "build.output-dir", fixed = TRUE)
})

# brand.yml's own keys contain hyphens (line-height, palette names); they are
# written verbatim, never snake-cased.
test_that("brand keys keep brand.yml's own spelling", {
  dir <- local_collection()
  suppressMessages(settings_set(
    dir,
    "brand.color.palette.acme-blue" = "#0d6efd",
    "brand/typography/base/line-height" = 1.6,
    "brand.color.primary" = "acme-blue"
  ))

  raw <- yaml::read_yaml(fs::path(dir, "_webrarian.yml"))
  expect_equal(raw$brand$color$palette[["acme-blue"]], "#0d6efd")
  expect_null(raw$brand$color$palette[["acme_blue"]])
  expect_equal(raw$brand$typography$base[["line-height"]], 1.6)
  expect_equal(
    settings_get(collection_settings(dir), "brand.typography.base.line-height"),
    1.6
  )
})

test_that("an explicit NULL is written as null and the default applies", {
  dir <- local_collection()
  set_one(dir, "build.output-dir", "docs")
  set_one(dir, "build.output-dir", NULL)
  expect_equal(collection_settings(dir)$build$output_dir, "_site")
})

test_that("unnamed settings are an error, not a silent no-op", {
  dir <- local_collection()
  expect_error(settings_set(dir, "webr.version", "0.6.0"), "must be named")
  expect_error(settings_set(dir), "Nothing to set")
})

test_that("an empty key segment is rejected", {
  expect_error(normalize_setting_key("build..output-dir"), "empty segment")
  expect_error(normalize_setting_key("build."), "empty segment")
  expect_error(normalize_setting_key("build/"), "empty segment")
  expect_equal(normalize_setting_key("repl.panels.plot"), c("repl", "panels", "plot"))
})

test_that("a write that would not parse never replaces _webrarian.yml", {
  dir <- local_collection()
  path <- fs::path(dir, "_webrarian.yml")
  before <- readLines(path)

  expect_error(
    write_config_file(list(build = list(output_dir = "a", `output-dir` = "b")), path),
    "would not parse"
  )
  expect_identical(readLines(path), before)
  expect_length(fs::dir_ls(dir, all = TRUE, regexp = "tmp-"), 0L)
})

# `_webrarian.yml` uses lowercase hyphenated keys; the settings object uses snake_case. The
# translation happens where the file is read and written, so that R code can keep saying
# `config$files$mount_point` instead of `config$files$`mount-point`` several hundred times.
#
# These tests pin the two things that make that split safe: the translation round-trips, and a
# pre-hyphen config is rejected rather than quietly working. The second matters because the
# internal spelling IS the old file spelling - without the check, an old `_webrarian.yml` would
# flow straight through and keep working, which is exactly the back-compatibility this rename
# was chosen not to have.

test_that("hyphenated file keys become snake_case settings", {
  x <- config_keys_to_snake(list(
    files = list(`mount-point` = "/home/web_user", include = list("a.R")),
    repl = list(`auto-run-files` = list("a.R"), `startup-script` = NULL),
    build = list(`output-dir` = "_site")
  ))
  expect_equal(x$files$mount_point, "/home/web_user")
  expect_equal(x$repl$auto_run_files, list("a.R"))
  expect_equal(x$build$output_dir, "_site")
  expect_equal(x$files$include, list("a.R")) # single-word keys are untouched
})

test_that("an explicit null survives the translation as a present, NULL entry", {
  # `x[[i]] <- NULL` deletes rather than assigns, so a naive loop drops every `key: null` and
  # the defaults silently win.
  x <- config_keys_to_snake(list(repl = list(`startup-script` = NULL, `show-editor` = TRUE)))
  expect_true("startup_script" %in% names(x$repl))
  expect_null(x$repl$startup_script)
  expect_true(x$repl$show_editor)
})

test_that("snake_case settings become hyphenated file keys", {
  y <- config_keys_to_kebab(list(
    files = list(mount_point = "/home/web_user"),
    repl = list(auto_run_files = list("a.R"), startup_script = NULL)
  ))
  expect_equal(names(y$files), "mount-point")
  expect_setequal(names(y$repl), c("auto-run-files", "startup-script"))
})

test_that("the two directions round-trip", {
  original <- list(
    project = list(name = "demo"),
    files = list(mount_point = "/home/web_user", exclude = list("**/.DS_Store")),
    repl = list(auto_run_files = list("a.R"), startup_script = NULL, allow_url_override = TRUE),
    ui = list(meta = list(og_image = NULL, twitter_card = "summary")),
    build = list(output_dir = "_site", service_worker = FALSE)
  )
  expect_equal(config_keys_to_snake(config_keys_to_kebab(original)), original)
})

test_that("a pre-hyphen config is rejected, naming the key and its new spelling", {
  expect_error(
    config_keys_to_snake(list(files = list(mount_point = "/home/web_user"))),
    "files.mount_point"
  )
  expect_error(
    config_keys_to_snake(list(files = list(mount_point = "/home/web_user"))),
    "mount-point"
  )
})

test_that("collection_settings rejects an underscored key and gives the hyphenated spelling", {
  dir <- withr::local_tempdir()
  writeLines(
    c("project:", "  name: demo", "repl:", "  auto_run_files: [a.R]"),
    file.path(dir, "_webrarian.yml")
  )
  expect_error(collection_settings(dir), "auto-run-files")
})

test_that("the brand subtree is passed through untouched in both directions", {
  # brand holds a brand.yml document; its key names are Posit's standard, not webrarian's,
  # so rewriting them would corrupt a valid brand config.
  brand <- list(color = list(palette = list(blue = "#4A90D9")), meta = list(name = "My App"))
  x <- config_keys_to_snake(list(brand = brand, build = list(`output-dir` = "_site")))
  expect_equal(x$brand, brand)
  expect_equal(config_keys_to_kebab(list(brand = brand))$brand, brand)
})

test_that("a brand key containing an underscore does not trip the underscore check", {
  expect_equal(
    config_keys_to_snake(list(brand = list(some_vendor_key = 1)))$brand$some_vendor_key,
    1
  )
})

test_that("every key the shipped template declares is hyphenated", {
  template <- readLines(system.file("templates", "_webrarian.yml", package = "webrarian"))
  # Only the key name, never the value: paths like "/home/web_user" and "_site" are full of
  # underscores and say nothing about the key spelling.
  key_lines <- grep("^\\s*#?\\s*[a-z][a-z0-9_-]*\\s*:", template, value = TRUE)
  keys <- sub("^\\s*#?\\s*([a-z][a-z0-9_-]*)\\s*:.*$", "\\1", key_lines)
  expect_equal(grep("_", keys, value = TRUE), character())
})

# One key table (R/config-spec.R) drives defaults, validation and, later, the
# generated key reference.

write_config <- function(lines, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(lines, fs::path(dir, "_webrarian.yml"))
  dir
}

test_that("an unknown key warns and names the nearest real key", {
  dir <- write_config(c("repl:", "  autorun: [a.R]"))
  expect_warning(collection_settings(dir), "repl.auto-run")
  dir <- write_config(c("webr:", "  verison: '0.6.0'"))
  expect_warning(collection_settings(dir), "webr.version")
  dir <- write_config(c("reple:", "  auto-run: []"))
  expect_warning(collection_settings(dir), "`repl`")
  dir <- write_config(c("repl:", "  panels:", "    plots: false"))
  expect_warning(collection_settings(dir), "repl.panels.plot")
})

test_that("a key with no near match still warns", {
  dir <- write_config(c("zzzzzz: 1"))
  expect_warning(collection_settings(dir), "not a webrarian setting")
})

test_that("a key that looks like a cli expression is reported, not evaluated", {
  dir <- write_config(c("\"reple{x}\": 1"))
  expect_warning(collection_settings(dir), "`reple{x}` is not a webrarian setting", fixed = TRUE)
  dir <- local_collection()
  expect_error(settings_set(dir, "ui{x}" = 1), "`ui{x}` is not a webrarian setting", fixed = TRUE)
})

test_that("a key from before 0.1.0 gets the ordinary unknown-key warning", {
  for (lines in list(
    c("repl:", "  show-plot: false"),
    c("repl:", "  allow-url-override: false"),
    c("build:", "  compress: true")
  )) {
    key <- paste0(sub(":$", "", lines[[1]]), ".", sub(":.*$", "", trimws(lines[[2]])))
    expect_warning(
      collection_settings(write_config(lines)),
      sprintf("`%s` is not a webrarian setting and is ignored.", key),
      fixed = TRUE
    )
  }
})

test_that("a value of the wrong type is an error naming the key and the type", {
  expect_error(collection_settings(write_config(c("webr:", "  version: 0.6"))), "quoted version")
  expect_error(collection_settings(write_config(c("repl:", "  share-links: yes"))), "one of")
  # The same two values as pyodidarian's enum.
  expect_error(
    collection_settings(write_config(c("ui:", "  meta:", "    twitter-card: large"))),
    "twitter-card"
  )
  expect_error(
    collection_settings(write_config(c("build:", "  clean: sometimes"))),
    "true or false"
  )
  expect_error(
    collection_settings(write_config(c("files:", "  include: {a: 1}"))),
    "list of strings"
  )
  # YAML 1.1 reads a bare yes/no/on/off as a boolean wherever it appears, so
  # every key that takes text says to quote it, not only share-links (the
  # same rule as pyodidarian's bare-yaml-words-quoted-elsewhere).
  expect_error(collection_settings(write_config(c("project:", "  name: yes"))), "quote")
  expect_error(collection_settings(write_config(c("files:", "  include: [a.R, on]"))), "quote")
  flag_error <- tryCatch(
    collection_settings(write_config(c("build:", "  clean: sometimes"))),
    error = conditionMessage
  )
  expect_false(grepl("quote", flag_error))
})

# The spec documents `share-links: open|fixed|off`. The yaml package reads
# YAML 1.1, where a bare off/no/false is a boolean, so the documented value
# arrives as FALSE. For this key FALSE can only mean "off".
test_that("an unquoted share-links: off (which YAML reads as false) means off", {
  expect_equal(
    collection_settings(write_config(c("repl:", "  share-links: off")))$repl$share_links,
    "off"
  )
  expect_equal(
    collection_settings(write_config(c("repl:", "  share-links: no")))$repl$share_links,
    "off"
  )
  expect_equal(
    collection_settings(write_config(c("repl:", "  share-links: \"off\"")))$repl$share_links,
    "off"
  )
})

test_that("rewriting the file keeps share-links as the string off", {
  dir <- write_config(c("project:", "  name: demo", "repl:", "  share-links: off"))
  suppressMessages(settings_set(dir, "project.name" = "renamed"))
  expect_identical(yaml::read_yaml(fs::path(dir, "_webrarian.yml"))$repl[["share-links"]], "off")

  suppressMessages(settings_set(dir, "repl.share-links" = "open"))
  suppressMessages(settings_set(dir, "repl.share-links" = FALSE))
  expect_equal(collection_settings(dir)$repl$share_links, "off")
})

test_that("an unquoted yes or on for share-links is an error that says to quote it", {
  expect_error(collection_settings(write_config(c("repl:", "  share-links: on"))), "quote")
})

test_that("brand.yml keys are never checked", {
  dir <- write_config(c(
    "brand:",
    "  some_vendor_key: 1",
    "  color:",
    "    palette:",
    "      acme-blue: '#123456'"
  ))
  expect_no_warning(collection_settings(dir))
})

test_that("an empty file is a valid collection", {
  dir <- write_config(character())
  cfg <- collection_settings(dir)
  expect_equal(cfg$project$name, as.character(fs::path_file(dir)))
  expect_equal(cfg$repl$share_links, "open")
})

test_that("settings_set refuses unknown keys and wrong types before writing", {
  dir <- local_collection()
  before <- readLines(fs::path(dir, "_webrarian.yml"))
  expect_error(settings_set(dir, "webr.verison" = "0.6.0"), "webr.version")
  expect_error(settings_set(dir, "repl.share-links" = "sometimes"), "one of")
  expect_error(settings_set(dir, "build.compress" = FALSE), "not a webrarian setting")
  expect_identical(readLines(fs::path(dir, "_webrarian.yml")), before)
  # A refused key is not "ignored": only the warning says that.
  refused <- tryCatch(settings_set(dir, "webr.verison" = "0.6.0"), error = conditionMessage)
  expect_match(refused, "`webr.verison` is not a webrarian setting.", fixed = TRUE)
  expect_false(grepl("ignored", refused, fixed = TRUE))
})

test_that("catalog() writes no dead keys and the result validates silently", {
  dir <- withr::local_tempdir()
  expect_no_warning(suppressMessages(catalog(dir, detect = FALSE)))
  raw <- yaml::read_yaml(fs::path(dir, "_webrarian.yml"))
  expect_null(raw$deploy)
  expect_null(raw$project$version)
  expect_null(raw$repl[["clear-untitled"]])
  expect_no_warning(collection_settings(dir))
})

test_that("every shipped example and the template validate without warnings", {
  for (name in c("basic", "branded", "local-package", "data-analysis")) {
    expect_no_warning(collection_settings(collection_example(name)))
  }
  template <- yaml::read_yaml(system.file("templates", "_webrarian.yml", package = "webrarian"))
  expect_no_warning(validate_config(config_keys_to_snake(template)))
})

test_that("the defaults cover every key in the table", {
  d <- config_defaults()
  for (entry in config_spec()) {
    keys <- strsplit(entry$key, ".", fixed = TRUE)[[1]]
    parent <- if (length(keys) > 1L) d[[keys[-length(keys)]]] else d
    expect_true(keys[[length(keys)]] %in% names(parent), info = entry$key)
    expect_true(nzchar(entry$doc), info = entry$key)
  }
})

test_that("the print method runs on a validated config", {
  expect_no_error(utils::capture.output(
    print(collection_settings(collection_example("basic"))),
    type = "message"
  ))
})

test_that("the print method shows a brand given as a path", {
  dir <- write_config(c("brand: branding/brand.yml"))
  out <- utils::capture.output(print(collection_settings(dir)), type = "message")
  expect_true(any(grepl("branding/brand.yml", out, fixed = TRUE)))
})

test_that("repl.persist-edits is a flag that defaults to TRUE", {
  expect_true(collection_settings(write_config(character()))$repl$persist_edits)
  expect_false(
    collection_settings(write_config(c("repl:", "  persist-edits: false")))$repl$persist_edits
  )
  expect_error(
    collection_settings(write_config(c("repl:", "  persist-edits: sometimes"))),
    "persist-edits"
  )
})

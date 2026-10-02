# The public API is the shared table in the pyodidarian design spec, for R.
# This is the one place that states it.

api_table <- list(
  acquire_file = c(paths = "", path = "\".\""),
  acquire_package = c(
    packages = "",
    path = "\".\"",
    source = "c(\"prebuilt\", \"local\", \"github\")"
  ),
  bind = c(path = "\".\"", offline = "NULL", clean = "NULL"),
  catalog = c(
    path = "\".\"",
    template = "c(\"minimal\", \"data-analysis\", \"package\")",
    packages = "NULL",
    detect = "TRUE"
  ),
  check_inventory = c(packages = "NULL", path = "\".\""),
  circulate_via_github = c(path = "\".\"", overwrite = "FALSE"),
  circulate_via_netlify = c(path = "\".\"", overwrite = "FALSE"),
  clean_shelves = c(path = "\".\""),
  collection_example = c(name = "", dest = "NULL"),
  collection_files = c(path = "\".\""),
  collection_mirror = c(
    dest = "",
    packages = "NULL",
    mode = "c(\"packages\", \"full\")",
    share_links = "\"off\"",
    webr_version = "NULL",
    repos = "NULL",
    favicon = "NULL"
  ),
  collection_packages = c(path = "\".\""),
  collection_root = c(path = "\".\""),
  collection_settings = c(path = "\".\""),
  diagnose_collection = c(path = "\".\""),
  is_collection = c(path = "\".\""),
  reading_room = c(
    path = "\".\"",
    port = "NULL",
    watch = "FALSE",
    block = "TRUE",
    open_browser = "rlang::is_interactive()"
  ),
  reading_room_close = c(room = "NULL"),
  settings_get = c(config = "", key = "", default = "NULL"),
  settings_set = c(path = "\".\"", ... = ""),
  webr_cache_clear = c(versions = "NULL"),
  webr_cache_info = character(),
  withdraw_file = c(paths = "", path = "\".\""),
  withdraw_package = c(packages = "", path = "\".\"")
)

test_that("the exports are exactly the API table", {
  expect_setequal(namespace_exports(), names(api_table))
  expect_length(namespace_exports(), 24L)
})

test_that("every export has the table's arguments, order and defaults", {
  for (name in names(api_table)) {
    f <- get(name, envir = asNamespace("webrarian"))
    expect_identical(
      names(formals_text(f)) %||% character(),
      names(api_table[[name]]) %||% character(),
      info = name
    )
    expect_identical(unname(formals_text(f)), unname(api_table[[name]]), info = name)
  }
})

test_that("the pkgdown reference index lists every export and nothing else", {
  pkgdown <- test_path("..", "..", "_pkgdown.yml")
  skip_if_not(file.exists(pkgdown), "_pkgdown.yml is not in this tree")
  listed <- unlist(lapply(yaml::read_yaml(pkgdown)$reference, `[[`, "contents"))
  listed <- listed[!grepl("(", listed, fixed = TRUE)]
  expect_setequal(listed, names(api_table))
})

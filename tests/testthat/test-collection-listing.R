# collection_files() and collection_packages() list what a collection holds,
# always in the same shape.

test_that("collection_files() returns every element, visibly, even when nothing is configured", {
  root <- local_collection()
  out <- withVisible(collection_files(root))
  expect_true(out$visible)
  expect_s3_class(out$value, "webrarian_files")
  expect_named(out$value, c("include", "exclude", "mount_point", "files"))
  expect_identical(out$value$include, character())
  expect_identical(out$value$files, character())
  expect_equal(out$value$mount_point, "/home/web_user")
})

test_that("collection_files() lists exactly the files bind() bundles", {
  root <- local_collection()
  fs::dir_create(fs::path(root, "data"))
  writeLines("a", fs::path(root, "data", "public.csv"))
  writeLines("b", fs::path(root, "data", "private.csv"))
  suppressMessages(settings_set(
    root,
    "files.include" = list("data/"),
    "files.exclude" = list("data/private.csv")
  ))
  files <- collection_files(root)
  expect_identical(files$files, "data/public.csv")
  expect_identical(files$include, "data/")
  expect_true("data/private.csv" %in% files$exclude)
  expect_message(print(files), "public.csv", fixed = TRUE)
})

# File names and include patterns are data to the printer, never cli
# templates: brace globs are accepted, and a file may be called data/{x}.csv.
test_that("a files listing prints names and patterns that contain braces", {
  files <- structure(
    list(
      include = c("data/{a,b}.csv", "R/"),
      exclude = character(),
      mount_point = "/home/web_user",
      files = c("data/{x}.csv", "data/a.csv")
    ),
    class = "webrarian_files"
  )
  expect_no_error(suppressMessages(print(files)))
  expect_message(print(files), "data/{x}.csv", fixed = TRUE)
  expect_message(print(files), "data/{a,b}.csv", fixed = TRUE)
  expect_invisible(suppressMessages(print(files)))
})

test_that("collection_packages() returns one class, visibly, with zero rows when empty", {
  root <- local_collection()
  out <- withVisible(collection_packages(root))
  expect_true(out$visible)
  expect_s3_class(out$value, "webrarian_packages")
  expect_s3_class(out$value, "data.frame")
  expect_named(out$value, c("package", "source"))
  expect_equal(nrow(out$value), 0L)

  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue", "cli")))
  pkgs <- collection_packages(root)
  expect_s3_class(pkgs, "webrarian_packages")
  expect_equal(pkgs$package, c("glue", "cli"))
  expect_equal(pkgs$source, c("prebuilt", "prebuilt"))
})

# print.webrarian_packages() (kept from 0.0.1) now prints collection_packages()'s
# data frame; no test called it before.
test_that("a packages listing prints each package, or says there are none", {
  root <- local_collection()
  expect_message(print(collection_packages(root)), "No packages configured")
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  pkgs <- collection_packages(root)
  expect_message(print(pkgs), "glue", fixed = TRUE)
  expect_invisible(suppressMessages(print(pkgs)))
})

test_that("the catalog_* names are gone", {
  for (old in c("catalog_files", "catalog_packages", "resolve_package_dependencies")) {
    expect_false(exists(old, envir = asNamespace("webrarian"), inherits = FALSE), info = old)
  }
  expect_false(any(c("catalog_files", "catalog_packages") %in% namespace_exports()))
})

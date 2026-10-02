# bind() and clean_shelves() delete their output directory. These tests pin the
# guard that decides what they may delete. The directory comes
# from build.output-dir alone (the shared API has no output_dir argument).

expect_collection_intact <- function(root) {
  expect_true(fs::file_exists(fs::path(root, "precious.R")))
  expect_true(fs::file_exists(fs::path(root, "_webrarian.yml")))
}

test_that("'.', '', '..', a sibling or an outside path is refused before anything is deleted", {
  root <- local_collection()
  sibling <- fs::path(fs::path_dir(root), "sibling")
  fs::dir_create(sibling)
  writeLines("keep", fs::path(sibling, "keep.txt"))

  for (bad in c(
    ".",
    "./",
    "",
    "..",
    "../sibling",
    as.character(fs::path(fs::path_dir(root), "elsewhere"))
  )) {
    set_output_dir(root, bad)
    expect_error(suppressMessages(bind(root)), "output", info = bad)
  }
  expect_collection_intact(root)
  expect_true(fs::file_exists(fs::path(sibling, "keep.txt")))
})

test_that("an output-dir read from _webrarian.yml is guarded in bind() and clean_shelves()", {
  root <- local_collection()
  suppressMessages(settings_set(root, "build/output_dir" = ".."))

  expect_error(suppressMessages(bind(root)), "not inside")
  expect_error(suppressMessages(clean_shelves(root)), "not inside")
  expect_collection_intact(root)
})

test_that("an absolute output dir inside the collection is used as given, not re-rooted", {
  root <- local_collection()
  out <- fs::path(fs::path_real(root), "public")
  set_output_dir(root, as.character(out))

  suppressMessages(bind(root))

  expect_true(fs::file_exists(fs::path(out, "index.html")))
  first_segment <- strsplit(as.character(out), "/", fixed = TRUE)[[1]][[2]]
  expect_false(fs::dir_exists(fs::path(root, first_segment)))
})

test_that("a directory webrarian did not create is never deleted", {
  root <- local_collection()
  docs <- fs::path(root, "docs")
  fs::dir_create(docs)
  writeLines("hand written", fs::path(docs, "notes.md"))
  set_output_dir(root, "docs")

  expect_error(suppressMessages(bind(root)), "not created by")
  expect_error(suppressMessages(clean_shelves(root)), "not created by")
  expect_identical(readLines(fs::path(docs, "notes.md")), "hand written")
})

test_that("an empty existing directory may be used as the output", {
  root <- local_collection()
  fs::dir_create(fs::path(root, "docs"))
  set_output_dir(root, "docs")
  suppressMessages(bind(root))
  expect_true(fs::file_exists(fs::path(root, "docs", "index.html")))
})

test_that("output inside .git or .webrarian is refused", {
  root <- local_collection()
  fs::dir_create(fs::path(root, ".git"))
  set_output_dir(root, ".git")
  expect_error(suppressMessages(bind(root)), "belongs to")
  set_output_dir(root, ".webrarian/site")
  expect_error(suppressMessages(bind(root)), "belongs to")
})

test_that("bind() marks its output, and a marked output can be rebuilt and cleaned", {
  root <- local_collection()
  suppressMessages(bind(root))

  marker <- fs::path(root, "_site", ".webrarian-build")
  expect_true(fs::file_exists(marker))
  info <- jsonlite::read_json(marker)
  expect_identical(info$tool, "webrarian")
  expect_match(info$build_id, "^[0-9]{14}-[0-9a-f]{8}$")

  expect_no_error(suppressMessages(bind(root)))
  suppressMessages(clean_shelves(root))
  expect_false(fs::dir_exists(fs::path(root, "_site")))
  expect_collection_intact(root)
})

test_that("a site built before the marker existed is still recognized", {
  dir <- withr::local_tempdir()
  writeLines("<html></html>", fs::path(dir, "index.html"))
  writeLines("//", fs::path(dir, "exlibris-r.js"))
  expect_true(is_webrarian_output(dir))
})

test_that("a collection reached through a symlink accepts an absolute output dir spelled either way", {
  skip_on_os("windows")
  root <- local_collection()
  link <- fs::path(fs::path_dir(root), "link")
  fs::link_create(root, link)

  real_out <- fs::path(fs::path_real(root), "site-a")
  set_output_dir(link, as.character(real_out))
  suppressMessages(bind(link))
  expect_true(fs::file_exists(fs::path(real_out, "index.html")))

  link_out <- fs::path(link, "site-b")
  set_output_dir(fs::path_real(root), as.character(link_out))
  suppressMessages(bind(fs::path_real(root)))
  expect_true(fs::file_exists(fs::path(root, "site-b", "index.html")))

  outside <- fs::path(fs::path_dir(root), "outside")
  fs::dir_create(outside)
  fs::link_create(outside, fs::path(root, "escape"))
  set_output_dir(root, "escape")
  expect_error(suppressMessages(bind(root)), "not inside")
  set_output_dir(root, "escape/sub")
  expect_error(suppressMessages(bind(root)), "not inside")
})

# bind() and clean_shelves() have the shared signatures: no
# argument that does nothing, and one source for the output directory.

test_that("bind() takes the collection, offline and clean, and nothing else", {
  expect_identical(formals_text(bind), c(path = "\".\"", offline = "NULL", clean = "NULL"))
  expect_identical(formals_text(clean_shelves), c(path = "\".\""))
})

test_that("bind() and clean_shelves() use build.output-dir", {
  root <- local_collection()
  set_output_dir(root, "public")
  suppressMessages(bind(root))
  expect_true(fs::file_exists(fs::path(root, "public", "index.html")))
  expect_false(fs::dir_exists(fs::path(root, "_site")))
  suppressMessages(clean_shelves(root))
  expect_false(fs::dir_exists(fs::path(root, "public")))
})

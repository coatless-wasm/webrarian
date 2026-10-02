# A cached webR asset directory is only reusable if the download completed. An
# interrupted download leaves a non-empty but partial directory that used to be
# served forever ("Using cached webR"). cache_is_complete() gates reuse on a
# sentinel written only after the copy fully succeeds.

test_that("cache_is_complete requires the completion sentinel", {
  d <- withr::local_tempdir()
  expect_false(cache_is_complete(d)) # empty dir

  writeLines("x", file.path(d, "R.wasm"))
  expect_false(cache_is_complete(d)) # files but no sentinel

  writeLines("0.5.8", file.path(d, ".webrarian-complete"))
  expect_true(cache_is_complete(d)) # sentinel present
})

test_that("cache_is_complete is FALSE for a missing directory", {
  expect_false(cache_is_complete(file.path(withr::local_tempdir(), "does-not-exist")))
})

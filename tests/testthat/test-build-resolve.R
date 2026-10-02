# Tests for pure, offline dependency-resolution helpers in R/build.R.
# No Docker, no network: these operate on in-memory PACKAGES-style matrices
# and config lists only.

# A small synthetic PACKAGES matrix mimicking repo.r-wasm.org's index.
# Columns match what the helpers read: Package, Version, Depends, Imports.
make_available <- function() {
  rbind(
    c(
      Package = "dplyr",
      Version = "1.1.4",
      Depends = "R (>= 4.1)",
      Imports = "rlang (>= 1.1.0), tibble, methods"
    ),
    c(Package = "rlang", Version = "1.1.2", Depends = "", Imports = ""),
    c(Package = "tibble", Version = "3.2.1", Depends = "", Imports = "rlang"),
    c(Package = "vctrs", Version = "0.6.5", Depends = "R (>= 3.5.0)", Imports = "rlang (>= 1.1.0)")
  )
}

# ---------------------------------------------------------------------------
# parse_deps()
# ---------------------------------------------------------------------------

test_that("parse_deps strips version constraints and returns clean names", {
  expect_equal(parse_deps("R (>= 4.1), methods"), c("R", "methods"))
})

test_that("parse_deps strips constraints from a single entry", {
  expect_equal(parse_deps("rlang (>= 1.1.0)"), "rlang")
})

test_that("parse_deps skips NA and empty strings", {
  expect_equal(parse_deps(NA_character_), character())
  expect_equal(parse_deps(""), character())
  expect_equal(parse_deps(c(NA, "")), character())
})

test_that("parse_deps de-duplicates package names", {
  expect_equal(parse_deps(c("dplyr", "dplyr, rlang")), c("dplyr", "rlang"))
})

test_that("parse_deps handles a multi-element input vector (Depends + Imports)", {
  out <- parse_deps(c("R (>= 4.1)", "rlang (>= 1.1.0), tibble, methods"))
  expect_equal(out, c("R", "rlang", "tibble", "methods"))
})

# ---------------------------------------------------------------------------
# resolve_package_deps()
# ---------------------------------------------------------------------------

test_that("resolve_package_deps returns packages unchanged when index is not a matrix", {
  expect_equal(resolve_package_deps(c("dplyr"), NULL), c("dplyr"))
  expect_equal(resolve_package_deps(c("dplyr"), "not-a-matrix"), c("dplyr"))
})

test_that("resolve_package_deps pulls in transitive Imports/Depends and drops base packages", {
  avail <- make_available()
  out <- resolve_package_deps("dplyr", avail)

  # dplyr requested, plus its available deps rlang + tibble (transitively).
  expect_true(all(c("dplyr", "rlang", "tibble") %in% out))
  # "methods" is a base package and must be dropped.
  expect_false("methods" %in% out)
  # "R" pseudo-dependency must be dropped.
  expect_false("R" %in% out)
  # vctrs was never requested nor a dependency of dplyr here.
  expect_false("vctrs" %in% out)
})

test_that("resolve_package_deps only keeps deps present in the index", {
  avail <- make_available()
  # tibble imports rlang (present) -> both returned, nothing spurious.
  out <- resolve_package_deps("tibble", avail)
  expect_setequal(out, c("tibble", "rlang"))
})

test_that("resolve_package_deps keeps a requested package even if absent from the index", {
  avail <- make_available()
  out <- resolve_package_deps("ghost", avail)
  # Requested packages are always retained; missing ones just contribute no deps.
  expect_true("ghost" %in% out)
})

test_that("resolve_package_deps returns each package once", {
  avail <- make_available()
  out <- resolve_package_deps(c("dplyr", "tibble", "rlang"), avail)
  expect_equal(anyDuplicated(out), 0L)
})

# ---------------------------------------------------------------------------
# get_package_filename()
# ---------------------------------------------------------------------------

test_that("get_package_filename returns <pkg>_<version>.tgz from the index", {
  avail <- make_available()
  expect_equal(get_package_filename("dplyr", avail), "dplyr_1.1.4.tgz")
  expect_equal(get_package_filename("rlang", avail), "rlang_1.1.2.tgz")
})

test_that("get_package_filename returns NULL for a package missing from the index", {
  avail <- make_available()
  expect_null(get_package_filename("nonexistent", avail))
})

test_that("get_package_filename falls back to <pkg>.tgz when index is not a matrix", {
  expect_equal(get_package_filename("dplyr", NULL), "dplyr.tgz")
  expect_equal(get_package_filename("dplyr", "not-a-matrix"), "dplyr.tgz")
})

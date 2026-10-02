# Every holder of bundled code is credited: each package in the
# viewer's notices has a line in inst/COPYRIGHTS, and each holder there is a
# "cph" in Authors@R, and the other way round.

# The bundled packages exlibris's metafile lists, and the nested code its
# generator appends as the file's last section.
third_party_names <- function() {
  lines <- readLines(
    system.file("viewer", "THIRD-PARTY.md", package = "webrarian"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  start <- grep("^# Third-party notices: exlibris-r", lines)[1]
  nested <- match("## Code nested inside bundled packages", lines)
  rows <- function(from, to) {
    x <- lines[from:to]
    sub("^\\| `([^`]+)` \\|.*$", "\\1", grep("^\\| `[^`]+` \\|", x, value = TRUE))
  }
  list(bundled = unique(rows(start, nested - 1L)), nested = unique(rows(nested, length(lines))))
}

copyrights_block <- function(name) {
  lines <- readLines(
    system.file("COPYRIGHTS", package = "webrarian"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  from <- match(paste("BEGIN", name), lines)
  to <- match(paste("END", name), lines)
  fields <- strsplit(lines[(from + 1L):(to - 1L)], " | ", fixed = TRUE)
  data.frame(
    package = vapply(fields, `[[`, character(1), 1L),
    holders = vapply(fields, `[[`, character(1), 2L),
    stringsAsFactors = FALSE
  )
}

cph_holders <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "webrarian"), fields = "Authors@R")
  people <- eval(parse(text = desc[1, "Authors@R"]))
  cph <- people[vapply(people, function(p) "cph" %in% p$role, logical(1))]
  vapply(cph, function(p) format(p, include = c("given", "family")), character(1))
}

test_that("inst/COPYRIGHTS has a line for every package in the viewer's notices", {
  names <- third_party_names()
  expect_setequal(copyrights_block("BUNDLED")$package, names$bundled)
  expect_setequal(copyrights_block("NESTED")$package, names$nested)
})

test_that("every holder in inst/COPYRIGHTS is a cph in Authors@R, and every cph is a holder", {
  holders <- unique(unlist(strsplit(
    c(copyrights_block("BUNDLED")$holders, copyrights_block("NESTED")$holders),
    "; ",
    fixed = TRUE
  )))
  expect_setequal(setdiff(cph_holders(), "James Balamuta"), holders)
})

test_that("DESCRIPTION points at inst/COPYRIGHTS", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "webrarian"), fields = "Copyright")
  expect_match(desc[1, "Copyright"], "inst/COPYRIGHTS", fixed = TRUE)
})

test_that("the maintainer is James Balamuta <james.balamuta@gmail.com>", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "webrarian"), fields = "Authors@R")
  people <- eval(parse(text = desc[1, "Authors@R"]))
  cre <- people[vapply(people, function(p) "cre" %in% p$role, logical(1))]
  expect_length(cre, 1L)
  expect_identical(
    format(cre[[1]], include = c("given", "family", "email")),
    "James Balamuta <james.balamuta@gmail.com>"
  )
  expect_true(all(c("aut", "cph") %in% cre[[1]]$role))
  expect_false(grepl("balamut2@illinois.edu", desc[1, "Authors@R"], fixed = TRUE))
})

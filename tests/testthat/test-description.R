# CRAN-facing metadata and examples.

desc_field <- function(field) {
  unname(read.dcf(system.file("DESCRIPTION", package = "webrarian"), fields = field)[1, field])
}

test_that("DESCRIPTION declares its system requirements", {
  req <- desc_field("SystemRequirements")
  expect_false(is.na(req))
  expect_match(req, "Docker", fixed = TRUE)
  expect_match(req, "Quarto", fixed = TRUE)
})

test_that("the version is a release version, 0.1.0 or later", {
  version <- desc_field("Version")
  expect_match(version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_true(package_version(version) >= "0.1.0")
})

test_that("the Description explains webR at its first mention and names the mirror", {
  text <- gsub("\\s+", " ", desc_field("Description"))
  first <- as.integer(regexpr("'webR'", text, fixed = TRUE))
  expect_equal(
    first,
    as.integer(regexpr("'webR' (<https://docs.r-wasm.org/webr/>)", text, fixed = TRUE))
  )
  expect_match(text, "collection_mirror()", fixed = TRUE)
})

rd_files <- function() {
  man <- test_path("..", "..", "man")
  skip_if_not(dir.exists(man), "man/ is not in this tree")
  list.files(man, pattern = "\\.Rd$", full.names = TRUE)
}

rd_tags <- function(x) {
  tags <- character()
  walk <- function(node) {
    tag <- attr(node, "Rd_tag")
    if (!is.null(tag)) {
      tags <<- c(tags, tag)
    }
    if (is.list(node)) {
      for (child in node) {
        walk(child)
      }
    }
  }
  walk(x)
  tags
}

test_that("\\dontrun is kept for Docker builds, large downloads and whole-repository mirrors", {
  with_dontrun <- character()
  for (f in rd_files()) {
    if ("\\dontrun" %in% rd_tags(tools::parse_Rd(f))) {
      with_dontrun <- c(with_dontrun, sub("\\.Rd$", "", basename(f)))
    }
  }
  expect_setequal(with_dontrun, c("bind", "collection_mirror"))
})

# build.bundle-engine (default TRUE) copies the ~40 MB webR engine into a site,
# so an example that builds, outside \dontrun and interactive(), turns it off.
test_that("examples that build a site download no engine in R CMD check", {
  for (f in rd_files()) {
    ex <- tempfile(fileext = ".R")
    tools::Rd2ex(f, ex, commentDontrun = TRUE)
    if (!file.exists(ex)) {
      next
    }
    code <- readLines(ex, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    builds <- any(grepl("(^|[^A-Za-z0-9._])bind\\(", code))
    if (!builds || any(grepl("interactive()", code, fixed = TRUE))) {
      next
    }
    expect_true(
      any(grepl("\"build.bundle-engine\" = FALSE", code, fixed = TRUE)),
      info = basename(f)
    )
  }
})

test_that("examples name only webR versions this webrarian offers", {
  listed <- vapply(webr_versions_table()$versions, `[[`, character(1), "version")
  for (f in rd_files()) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    examples <- sub("(?s)^.*?\\\\examples\\{", "", text, perl = TRUE)
    if (identical(examples, text)) {
      next
    }
    versions <- gsub(
      "\"",
      "",
      regmatches(examples, gregexpr("\"0\\.[0-9]+\\.[0-9]+\"", examples))[[1]]
    )
    expect_true(all(versions %in% listed), info = basename(f))
  }
})

test_that("every export has a help page with examples", {
  for (fn in namespace_exports()) {
    f <- test_path("..", "..", "man", paste0(fn, ".Rd"))
    skip_if_not(dir.exists(dirname(f)), "man/ is not in this tree")
    expect_true(file.exists(f), info = fn)
    expect_match(
      paste(readLines(f, warn = FALSE), collapse = "\n"),
      "\\examples{",
      fixed = TRUE,
      info = fn
    )
  }
})

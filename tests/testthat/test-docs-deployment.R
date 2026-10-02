# The Deployment article describes the files webrarian writes for hosts
# (the rule shared with pyodidarian: "_headers" is the only source of the isolation
# headers and the cache rules, and LICENSES/ uses names both tools share).

deployment_text <- function() {
  lines <- readLines(repo_path("vignettes", "deployment.qmd"), warn = FALSE, encoding = "UTF-8")
  gsub("\\s+", " ", paste(lines, collapse = " "))
}

test_that("the generated netlify.toml carries no headers, as the article says", {
  skip_if_no_source_tree()
  root <- local_collection()
  written <- suppressMessages(circulate_via_netlify(root))
  expect_setequal(fs::path_file(written), c("netlify.toml", "netlify-deploy.yml"))
  toml <- readLines(written[fs::path_file(written) == "netlify.toml"], warn = FALSE)
  expect_false(any(grepl("[[headers]]", toml, fixed = TRUE)))
  expect_true(any(grepl("^\\s*publish\\s*=", toml)))
  text <- deployment_text()
  expect_match(
    text,
    "`netlify.toml` tells Netlify to publish `_site/` and sets no headers",
    fixed = TRUE
  )
  expect_false(grepl("with caching and content-type rules", text, fixed = TRUE))
})

test_that("the article names the LICENSES/ files every site carries", {
  skip_if_no_source_tree()
  text <- deployment_text()
  for (f in c(
    "index.html",
    "exlibris.md",
    "THIRD-PARTY-r.md",
    "webR.md",
    "PACKAGES.md",
    "webrarian.md"
  )) {
    expect_match(text, paste0("| `", f, "` |"), fixed = TRUE, info = f)
  }
  expect_false(grepl("`THIRD-PARTY.md`", text, fixed = TRUE))
  expect_false(grepl("EXCEPTION.md", text, fixed = TRUE))
})

# Every path _headers marks no-cache (the page, the files, the repository,
# LICENSES/, sw.js and the packages.json manifest) is in the Caching table's
# "rechecked on every visit" row, and nothing else is.
test_that("the Caching table's rechecked row lists every no-cache path in _headers", {
  skip_if_no_source_tree()
  rules <- cache_rules()
  no_cache <- vapply(
    rules,
    function(rule) {
      identical(unname(rule$headers["Cache-Control"]), "no-cache")
    },
    logical(1)
  )
  paths <- vapply(rules[no_cache], function(rule) rule$path, character(1))
  expected <- sub("\\*$", "", sub("^/", "", paths))
  expected <- expected[nzchar(expected)]
  lines <- readLines(repo_path("vignettes", "deployment.qmd"), warn = FALSE, encoding = "UTF-8")
  row <- grep("| rechecked on every visit |", lines, fixed = TRUE, value = TRUE)
  expect_length(row, 1L)
  cell <- strsplit(row[[1]], "|", fixed = TRUE)[[1]][[2]]
  listed <- gsub("`", "", regmatches(cell, gregexpr("`[^`]+`", cell))[[1]], fixed = TRUE)
  expect_setequal(listed, expected)
})

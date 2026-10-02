# The packages article says where the page installs packages from, and that
# must match what bind() writes into the page (offline sites and share links).

# The article's wording for a list of package sources, in the order the page
# searches them. A site without a repo/ has no repository of its own, whatever
# URL the code leaves in repo-url.
describe_sources <- function(r, has_local_repo) {
  sources <- c(r$repo_url, unlist(r$repos))
  sources <- sources[!is.na(sources) & nzchar(sources)]
  if (!has_local_repo) {
    sources <- setdiff(sources, "./repo")
  }
  if (length(sources) == 0L) {
    return("none")
  }
  words <- vapply(
    sources,
    function(s) {
      switch(
        s,
        "./repo" = "the site's own `repo/`",
        "https://repo.r-wasm.org" = "repo.r-wasm.org",
        "https://x.r-universe.dev" = "`packages.repos`",
        s
      )
    },
    character(1)
  )
  paste(words, collapse = ", then ")
}

test_that("the article's table matches viewer_package_repos() in all four cases", {
  skip_if_no_source_tree()
  text <- paste(
    readLines(repo_path("vignettes", "packages.qmd"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  cases <- list(
    list(label = "With bundled packages", offline = FALSE, repo = TRUE),
    list(label = "Without bundled packages", offline = FALSE, repo = FALSE),
    list(label = "`build.offline: true`, with bundled packages", offline = TRUE, repo = TRUE),
    list(label = "`build.offline: true`, without bundled packages", offline = TRUE, repo = FALSE)
  )
  for (case in cases) {
    r <- viewer_package_repos(
      offline = case$offline,
      has_local_repo = case$repo,
      config_repos = "https://x.r-universe.dev"
    )
    row <- sprintf("| %s | %s |", case$label, describe_sources(r, case$repo))
    expect_match(text, row, fixed = TRUE, info = row)
  }
})

# acquire_package() looks prebuilt names up and warns ("No
# package repository has ...") about any that no repository has, adding it
# anyway.
test_that("the packages article says acquire_package() looks prebuilt names up", {
  skip_if_no_source_tree()
  text <- paste(
    readLines(repo_path("vignettes", "packages.qmd"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  text <- gsub("\\s+", " ", text)
  expect_false(grepl("does not contact any repository", text, fixed = TRUE))
  expect_match(text, "warns about any name no repository has", fixed = TRUE)
})

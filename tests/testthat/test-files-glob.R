# Regression tests for glob_to_regex() and the exclude matching in
# resolve_file_patterns(). The default excludes (**/*.Rhistory, **/.DS_Store,
# **/.git/**) must drop matching files at ANY depth, including the project root,
# or private files leak into the published bundle.

test_that("glob_to_regex: **/ matches zero or more leading directories", {
  re <- glob_to_regex("**/*.Rhistory")
  expect_match("x.Rhistory", re) # root level (the regression)
  expect_match("a/b.Rhistory", re) # one level deep
  expect_match("a/b/c.Rhistory", re) # deeply nested
  expect_false(grepl(re, "notes.txt")) # wrong extension
})

test_that("glob_to_regex: a single * does not cross directory separators", {
  re <- glob_to_regex("*.R")
  expect_match("foo.R", re)
  expect_false(grepl(re, "a/foo.R"))
})

test_that("glob_to_regex: ? matches exactly one non-separator character", {
  re <- glob_to_regex("file?.R")
  expect_match("file1.R", re)
  expect_false(grepl(re, "file.R")) # needs exactly one char
  expect_false(grepl(re, "file12.R")) # not two
  expect_false(grepl(re, "file/.R")) # must not match a slash
})

test_that("glob_to_regex: **/dir/** matches the directory at any depth", {
  re <- glob_to_regex("**/.git/**")
  expect_match(".git/config", re) # root-level .git
  expect_match("sub/.git/config", re) # nested
  expect_match("a/b/.git/hooks/pre-commit", re) # deeply nested
  expect_false(grepl(re, "src/main.R"))
})

test_that("resolve_file_patterns excludes matching files at every depth", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "sub", "cache"), recursive = TRUE)
  writeLines("x", file.path(root, "keep.R"))
  writeLines("x", file.path(root, "root.log")) # root-level, excluded
  writeLines("x", file.path(root, "sub", "keep2.R"))
  writeLines("x", file.path(root, "sub", "nested.log")) # nested, excluded
  writeLines("x", file.path(root, "sub", "cache", "data.bin")) # in cache/, excluded

  result <- resolve_file_patterns(
    include = ".", # a directory -> recurse into all files
    exclude = list("**/*.log", "**/cache/**"),
    root = root
  )

  expect_true("keep.R" %in% result)
  expect_true(file.path("sub", "keep2.R") %in% result)
  expect_false(any(grepl("\\.log$", result))) # both root and nested .log dropped
  expect_false(any(grepl("cache", result))) # cache/ contents dropped
})

# --- resolve_file_patterns(): recursive, gitignore-style -----------

local_tree <- function(files, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  for (f in files) {
    fs::dir_create(fs::path_dir(fs::path(root, f)))
    writeLines("x", fs::path(root, f))
  }
  root
}

tree_files <- c(
  "top.R",
  "R/a.R",
  "R/deep/b.R",
  "x.csv",
  "y.rds",
  "hash#1.csv",
  "data/a.csv",
  "data/sub/b.csv",
  "data/secret.csv",
  "data/raw/big.csv",
  "data/run.log",
  "notes.log",
  "data/.DS_Store",
  "data/.env",
  ".Renviron",
  ".env",
  ".RData",
  ".Rprofile",
  ".git/config",
  "_site/index.html",
  ".webrarian/staging-1-abc/R.wasm",
  "renv/library/x/DESCRIPTION",
  "renv/activate.R",
  ".Rproj.user/state",
  "_webrarian.yml"
)

test_that("** is recursive and includes the root level", {
  root <- local_tree(tree_files)
  expect_setequal(
    resolve_file_patterns("**/*.R", NULL, root),
    c("top.R", "R/a.R", "R/deep/b.R", "renv/activate.R")
  )
})

test_that("braces expand", {
  root <- local_tree(tree_files)
  expect_setequal(
    resolve_file_patterns("*.{csv,rds}", NULL, root),
    c("x.csv", "y.rds", "hash#1.csv")
  )
})

test_that("a glob that matches directories keeps only files", {
  root <- local_tree(tree_files)
  expect_setequal(
    resolve_file_patterns("data/*", NULL, root),
    c("data/a.csv", "data/secret.csv", "data/run.log")
  )
})

test_that("excludes written the way the files vignette writes them work", {
  root <- local_tree(tree_files)
  expect_setequal(
    resolve_file_patterns("data/", c("data/secret.csv", "*.log", "data/raw/", ".DS_Store"), root),
    c("data/a.csv", "data/sub/b.csv")
  )
})

test_that("a ** exclude is not matched against the collection's own parent directories", {
  root <- fs::path(local_tree(character()), "tmp")
  fs::dir_create(fs::path(root, "data"))
  writeLines("x", fs::path(root, "data", "a.csv"))
  expect_equal(as.character(resolve_file_patterns("data/", "**/tmp/**", root)), "data/a.csv")
})

test_that("include '.' in a real project skips git, renv, RStudio, webrarian and output directories", {
  root <- local_tree(tree_files)
  writeLines("GITHUB_PAT=secret", fs::path(root, ".Renviron"))
  got <- resolve_file_patterns(".", NULL, root, output_dir = "_site")
  expect_false(any(grepl("^(_site|\\.webrarian|\\.git|renv/library|\\.Rproj\\.user)/", got)))
  expect_false("_webrarian.yml" %in% got)
  expect_true(all(c("renv/activate.R", "top.R", "data/raw/big.csv") %in% got))

  # Secrets and saved workspaces are dot names. ".", "**", a directory include
  # and a wildcard never select one; a pattern that spells the dot does.
  secrets <- c(".Renviron", ".env", ".RData", ".Rprofile", "data/.env", "data/.DS_Store")
  expect_false(any(secrets %in% got))
  expect_false(any(grepl("(^|/)\\.", resolve_file_patterns("**", NULL, root))))
  expect_false(any(
    c("data/.env", "data/.DS_Store") %in% resolve_file_patterns("data/", NULL, root)
  ))
  rdata <- resolve_file_patterns("*.RData", NULL, root)
  expect_false(".RData" %in% rdata)
  expect_equal(attr(rdata, "unmatched"), "*.RData")
  expect_equal(as.character(resolve_file_patterns(".Renviron", NULL, root)), ".Renviron")
  expect_equal(as.character(resolve_file_patterns("data/.env", NULL, root)), "data/.env")
  expect_true(all(c(".Renviron", ".env", ".RData") %in% resolve_file_patterns(".*", NULL, root)))
})

test_that("the walk never enters renv's library, its staging area or the output directory", {
  root <- local_tree(c(tree_files, "renv/staging/y/DESCRIPTION", "_site/vfs-files/top.R"))
  got <- list_collection_files(root, prune_paths = c("renv/library", "renv/staging", "_site"))
  expect_false(any(grepl(
    "^(renv/library|renv/staging|_site|\\.git|\\.webrarian|\\.Rproj\\.user)/",
    got
  )))
  expect_true(all(c("renv/activate.R", "data/raw/big.csv", "top.R", "data/.DS_Store") %in% got))
  expect_equal(anyDuplicated(got), 0L)

  # resolve_file_patterns() prunes the output directory it is given.
  seen <- NULL
  local_mocked_bindings(list_collection_files = function(root, ...) {
    seen <<- list(...)$prune_paths
    character()
  })
  resolve_file_patterns(".", NULL, root, output_dir = "docs/site")
  expect_true(all(c("renv/library", "renv/staging", "docs/site") %in% seen))
})

# collection_files(), the watch manifest, diagnose_config() and acquire_file()
# pass build.output-dir as written, and an absolute one is allowed.
test_that("an absolute output dir inside the collection is left out like a relative one", {
  root <- local_tree(c(tree_files, "public/index.html", "public/vfs-files/top.R"))
  for (out in c(fs::path(root, "public"), fs::path(fs::path_real(root), "public"))) {
    got <- resolve_file_patterns(".", NULL, root, output_dir = as.character(out))
    expect_false(any(startsWith(got, "public/")), info = out)
    expect_true("top.R" %in% got, info = out)
  }
  # An output dir outside the collection holds none of its files: nothing to
  # leave out, and no error.
  got <- resolve_file_patterns(
    ".",
    NULL,
    root,
    output_dir = as.character(fs::path(fs::path_dir(root), "elsewhere"))
  )
  expect_true(all(c("top.R", "public/index.html") %in% got))
})

test_that("a pattern pointing outside the collection is an error", {
  root <- local_tree(tree_files)
  expect_error(resolve_file_patterns("../../README.md", NULL, root), "outside the collection")
  expect_error(resolve_file_patterns("/etc/passwd", NULL, root), "outside the collection")
})

test_that("include patterns that match nothing are reported", {
  root <- local_tree(tree_files)
  got <- resolve_file_patterns(c("data/", "nothing/*.R"), NULL, root)
  expect_equal(attr(got, "unmatched"), "nothing/*.R")
})

test_that("exclude: [] clears the default excludes", {
  merged <- merge_config(
    list(files = list(exclude = list("**/*.Rhistory", "**/.DS_Store"))),
    list(files = list(exclude = list()))
  )
  expect_identical(merged$files$exclude, list())
})

test_that("bind() bundles only selected files and warns about unmatched includes", {
  root <- local_collection()
  fs::dir_create(fs::path(root, "data", "raw"))
  writeLines("a,b", fs::path(root, "data", "keep.csv"))
  writeLines("a,b", fs::path(root, "data", "raw", "private.csv"))
  suppressMessages(settings_set(
    root,
    "files/include" = list(".", "missing/*.R"),
    "files/exclude" = list("data/raw/")
  ))

  expect_warning(suppressMessages(bind(root)), "missing/\\*\\.R")
  suppressWarnings(suppressMessages(bind(root)))

  bundled <- as.character(fs::path_rel(
    fs::dir_ls(fs::path(root, "_site", "vfs-files"), recurse = TRUE, type = "file"),
    fs::path(root, "_site", "vfs-files")
  ))
  # catalog()'s .gitignore is a dot name, which "." does not select.
  expect_setequal(bundled, c("precious.R", "data/keep.csv"))
})

test_that("a symlinked file that leads outside the collection is never bundled", {
  skip_on_os("windows")
  outside <- withr::local_tempdir()
  writeLines("GITHUB_PAT=secret", fs::path(outside, ".Renviron"))
  root <- local_collection()
  writeLines("x <- 1", fs::path(root, "inside.R"))
  fs::link_create(fs::path(outside, ".Renviron"), fs::path(root, "env.txt"))
  fs::link_create(fs::path(root, "inside.R"), fs::path(root, "alias.R"))
  fs::link_create(fs::path(outside, "gone.csv"), fs::path(root, "dangling.csv"))
  suppressMessages(settings_set(root, "files/include" = list(".")))

  expect_warning(suppressMessages(bind(root)), "env\\.txt")

  vfs <- fs::path(root, "_site", "vfs-files")
  bundled <- as.character(fs::path_rel(fs::dir_ls(vfs, recurse = TRUE, type = "file"), vfs))
  expect_false("env.txt" %in% bundled)
  expect_false("dangling.csv" %in% bundled)
  # A link that stays inside the collection is an ordinary file to bundle.
  expect_true(all(c("inside.R", "alias.R") %in% bundled))
  contents <- unlist(lapply(fs::path(vfs, bundled), readLines, warn = FALSE))
  expect_false(any(grepl("GITHUB_PAT", contents, fixed = TRUE)))
})

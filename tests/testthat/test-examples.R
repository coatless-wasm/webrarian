# The bundled example projects under inst/examples/ are the curated set users
# are pointed to, so they must stay accessible and valid.

test_that("collection_example() copies an example into dest and returns the copy", {
  dest <- fs::path(withr::local_tempdir(), "basic-copy")
  out <- suppressMessages(collection_example("basic", dest = dest))
  expect_identical(as.character(fs::path_real(out)), as.character(fs::path_real(dest)))
  expect_true(fs::file_exists(fs::path(dest, "_webrarian.yml")))
  expect_true(fs::file_exists(fs::path(dest, "data", "sample.csv")))
})

test_that("without dest, each call makes a fresh copy under tempdir()", {
  a <- suppressMessages(collection_example("basic"))
  b <- suppressMessages(collection_example("basic"))
  expect_false(identical(as.character(a), as.character(b)))
  expect_true(startsWith(as.character(fs::path_real(a)), as.character(fs::path_real(tempdir()))))
})

test_that("a dest that already holds files is refused and left untouched", {
  dest <- fs::path(withr::local_tempdir(), "mine")
  suppressMessages(collection_example("basic", dest = dest))
  writeLines("my edit", fs::path(dest, "analysis.R"))
  expect_error(collection_example("basic", dest = dest), "already holds files")
  expect_identical(readLines(fs::path(dest, "analysis.R")), "my edit")

  empty <- withr::local_tempdir()
  suppressMessages(collection_example("branded", dest = empty))
  expect_true(fs::file_exists(fs::path(empty, "_webrarian.yml")))
})

test_that("a missing or unknown name lists the examples, which have the names both tools share", {
  expect_error(collection_example(), "basic")
  expect_error(collection_example("does-not-exist"), "data-analysis")
  expect_setequal(
    example_names(),
    c("basic", "branded", "data-analysis", "local-package", "mirror")
  )
})

test_that("building a copy never writes into the installed package", {
  installed <- system.file("examples", "basic", package = "webrarian")
  before <- list.files(installed, recursive = TRUE, all.files = TRUE)
  path <- suppressMessages(collection_example("basic"))
  # The engine from the webR CDN: this build downloads nothing.
  suppressMessages(settings_set(path, "build.bundle-engine" = FALSE))
  suppressMessages(bind(path))
  expect_identical(list.files(installed, recursive = TRUE, all.files = TRUE), before)
})

test_that("every example's README builds a copy, never the installed example", {
  for (name in example_names()) {
    readme <- paste(
      readLines(system.file("examples", name, "README.md", package = "webrarian"), warn = FALSE),
      collapse = "\n"
    )
    if (name == "mirror") {
      # A script, not a collection: it builds into a directory the user names.
      expect_match(readme, "collection_mirror(", fixed = TRUE, info = name)
      next
    }
    expect_match(readme, sprintf('collection_example("%s"', name), fixed = TRUE, info = name)
    expect_false(grepl('system.file("examples', readme, fixed = TRUE), info = name)
  }
})

test_that("no example auto-runs a script another auto-run script sources", {
  for (name in setdiff(example_names(), "mirror")) {
    path <- suppressMessages(collection_example(name))
    auto_run <- unlist(collection_settings(path)$repl$auto_run)
    for (f in auto_run) {
      src <- readLines(fs::path(path, f), warn = FALSE)
      sourced <- sub(
        "^.*source\\([\"']([^\"']+)[\"'].*$",
        "\\1",
        grep("source\\(", src, value = TRUE)
      )
      expect_length(intersect(sourced, auto_run), 0L)
    }
  }
})

test_that("examples carry user-facing names, and the mirror script builds where it is told", {
  cfg <- collection_settings(suppressMessages(collection_example("basic")))
  expect_false(grepl("test", cfg$project$name, ignore.case = TRUE))
  expect_false(grepl("test", cfg$project$description, ignore.case = TRUE))

  script <- readLines(system.file("examples", "mirror", "build-mirror.R", package = "webrarian"))
  expect_false(any(grepl("tempdir()|Test script|test-mirror", script)))
  expect_true(any(grepl("commandArgs(trailingOnly = TRUE)", script, fixed = TRUE)))
  expect_true(any(grepl("if (interactive())", script, fixed = TRUE)))
})

test_that("every catalog-style example has a current, valid config", {
  # Guards against the examples going stale (old webR version, removed options,
  # YAML numeric coercion of an unquoted version).
  for (name in c("basic", "branded", "local-package", "data-analysis")) {
    cfg <- expect_no_warning(collection_settings(collection_example(name)))
    expect_identical(cfg$webr$version, "0.6.0", info = name)
    result <- suppressMessages(diagnose_config(collection_example(name)))
    expect_true(result$valid, info = name)
  }
})

test_that("the mirror example ships its build script and favicon", {
  p <- collection_example("mirror")
  expect_true(file.exists(file.path(p, "build-mirror.R")))
  expect_true(file.exists(file.path(p, "sample-favicon.svg")))
})

test_that("bind() builds the basic example with the CDN engine", {
  # Fast, no network, no Docker: verifies the core build pipeline (VFS + HTML)
  # actually runs and produces a bundle. Skipped on CRAN to keep check time low.
  skip_on_cran()
  proj <- withr::local_tempdir()
  file.copy(
    list.files(collection_example("basic"), full.names = TRUE),
    proj,
    recursive = TRUE
  )
  suppressMessages(settings_set(proj, "build.bundle-engine" = FALSE))
  suppressMessages(bind(proj))

  site <- file.path(proj, "_site")
  expect_true(file.exists(file.path(site, "index.html")))
  expect_true(dir.exists(file.path(site, "vfs-files")))

  # The vendored exlibris-r bundle (not the retired webrarian-repl.js) is emitted.
  expect_length(fs::dir_ls(site, regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"), 1L)

  # The config is inlined into index.html as window.__VIEWER_CONFIG__ - no
  # separate webrarian-config.json is written or fetched at runtime.
  expect_false(file.exists(file.path(site, "webrarian-config.json")))
  html <- paste(readLines(file.path(site, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "<script>window\\.__VIEWER_CONFIG__ = \\{", perl = TRUE)
})

test_that("bind() builds a bundled webR bundle end-to-end (downloads webR)", {
  # The full path: downloads and bundles the webR REPL locally. Gated on network
  # and CRAN; the webR download is cached after the first run.
  skip_on_cran()
  skip_if_offline()
  proj <- withr::local_tempdir()
  file.copy(
    list.files(collection_example("basic"), full.names = TRUE),
    proj,
    recursive = TRUE
  )
  suppressMessages(bind(proj))

  site <- file.path(proj, "_site")
  expect_true(file.exists(file.path(site, "index.html")))
  expect_true(file.exists(file.path(site, "webr", "v0.6.0", "webr-worker.js")))
})

test_that("the data-analysis example bundles its packages and records their sources (network)", {
  skip_on_cran()
  skip_if_offline()
  path <- suppressMessages(collection_example("data-analysis"))
  # The example's defaults: the engine and its packages are copied into the site.
  suppressMessages(bind(path))
  r_line <- resolve_webr_version(collection_settings(path)$webr$version)$r_version
  contrib <- fs::path(path, "_site", "repo", "bin", "emscripten", "contrib", r_line)
  expect_true(all(
    c("dplyr", "ggplot2", "scales") %in% read.dcf(fs::path(contrib, "PACKAGES"))[, "Package"]
  ))
  md <- paste(readLines(fs::path(path, "_site", "LICENSES", "PACKAGES.md")), collapse = "\n")
  expect_match(md, "| dplyr |", fixed = TRUE)
  expect_match(md, "https://repo.r-wasm.org/src/contrib/dplyr_", fixed = TRUE)
})

test_that("a mirror of cli builds from the real repository (network)", {
  skip_on_cran()
  skip_if_offline()
  out <- fs::path(withr::local_tempdir(), "mirror")
  suppressMessages(collection_mirror(out, packages = "cli"))
  r_line <- resolve_webr_version(webr_version_default())$r_version
  contrib <- fs::path(out, "repo", "bin", "emscripten", "contrib", r_line)
  expect_length(fs::dir_ls(contrib, regexp = "/cli_[^/]*\\.tgz$"), 1L)
  expect_true(fs::file_exists(fs::path(out, "LICENSES", "PACKAGES.md")))
})

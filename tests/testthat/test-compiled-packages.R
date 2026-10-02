# Compiled local and GitHub packages reach the page. Docker is
# the fake from tests/testthat/fixtures/fake-docker.sh; downloads come from a
# local fixture repository.

skip_if_not_installed("httpuv")

make_pkg <- function(dir, name, version = "0.1.0", imports = NULL) {
  fs::dir_create(fs::path(dir, "R"))
  writeLines(
    c(
      paste("Package:", name),
      paste("Version:", version),
      if (!is.null(imports)) paste("Imports:", imports)
    ),
    fs::path(dir, "DESCRIPTION")
  )
  writeLines("f <- function() 1", fs::path(dir, "R", "f.R"))
  dir
}

contrib_of <- function(root) fs::path(root, "_site", "repo", "bin", "emscripten", "contrib", "4.6")

test_that("a local package is compiled, installed at boot, and its dependencies are bundled", {
  local_fake_docker("ok")
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0")))
  local_mocked_bindings(default_repo_url = function() repo$url)

  root <- local_collection(bundle_engine = TRUE)
  make_pkg(fs::path(root, "pkgs", "demotools"), "demotools", imports = "stats, glue")
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))
  suppressMessages(bind(root))

  wire <- read_site_config(fs::path(root, "_site"))
  expect_true("demotools" %in% unlist(wire$packages$install))
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_true(fs::file_exists(fs::path(contrib_of(root), "demotools_0.1.0.tgz")))
  expect_true(fs::file_exists(fs::path(contrib_of(root), "glue_1.8.0.tgz")))
  index <- read.dcf(fs::path(contrib_of(root), "PACKAGES"))
  expect_equal(unname(index[index[, "Package"] == "demotools", "Imports"]), "stats, glue")
})

test_that("a local package's dependency that no repository has warns and the build goes on", {
  local_fake_docker("ok")
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0")))
  local_mocked_bindings(default_repo_url = function() repo$url)

  root <- local_collection(bundle_engine = TRUE)
  make_pkg(fs::path(root, "pkgs", "demotools"), "demotools", imports = "glue, notonwasm")
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))

  seen <- character()
  withCallingHandlers(
    suppressMessages(bind(root)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("notonwasm", seen, fixed = TRUE)))
  wire <- read_site_config(fs::path(root, "_site"))
  expect_true("demotools" %in% unlist(wire$packages$install))
  expect_true(fs::file_exists(fs::path(contrib_of(root), "glue_1.8.0.tgz")))
})

test_that("a configured prebuilt package that no repository has is still an error", {
  local_fake_docker("ok")
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0")))
  local_mocked_bindings(default_repo_url = function() repo$url)

  root <- local_collection(bundle_engine = TRUE)
  make_pkg(fs::path(root, "pkgs", "demotools"), "demotools")
  suppressMessages(settings_set(
    root,
    "packages.local" = list("pkgs/demotools"),
    "packages.prebuilt" = list("notonwasm")
  ))
  expect_error(suppressMessages(bind(root)), "notonwasm")
})

test_that("a GitHub package is installed at boot under its package name, and ./repo is reachable online", {
  # local_collection(): the engine comes from the CDN, and repo/ holds only
  # the compiled package.
  gh_src <- withr::local_tempdir()
  make_pkg(fs::path(gh_src, "praise"), "praise", version = "1.0.0.9000")
  local_fake_docker("ok", github = gh_src)

  root <- local_collection()
  suppressMessages(settings_set(root, "packages.github" = list("rladies/praise")))
  suppressMessages(bind(root))

  wire <- read_site_config(fs::path(root, "_site"))
  expect_true("praise" %in% unlist(wire$packages$install))
  expect_false("rladies/praise" %in% unlist(wire$packages$install))
  # repo/ first, then the public repository for anything else a visitor adds.
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_equal(unlist(wire$packages$repos)[[1]], "https://repo.r-wasm.org")
  # The CDN engine runs its worker from a blob: URL, where "./repo" means
  # nothing: the page must make it absolute before the viewer reads it.
  html <- paste(readLines(fs::path(root, "_site", "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "document.baseURI", fixed = TRUE)
})

test_that("an online build searches the compiled development version before repo.r-wasm.org", {
  gh_src <- withr::local_tempdir()
  make_pkg(fs::path(gh_src, "R6"), "R6", version = "2.6.1.9000")
  local_fake_docker("ok", github = gh_src)

  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("R6"),
    "packages.github" = list("r-lib/R6"),
    "packages.repos" = list("https://x.r-universe.dev")
  ))
  suppressMessages(bind(root))

  wire <- read_site_config(fs::path(root, "_site"))
  searched <- c(wire$packages[["repo-url"]], unlist(wire$packages$repos))
  # webr::install() takes a package from the first repository that has it.
  expect_lt(match("./repo", searched), match("https://repo.r-wasm.org", searched))
  expect_equal(searched, c("./repo", "https://repo.r-wasm.org", "https://x.r-universe.dev"))
  expect_equal(unlist(wire$packages$install), "R6")
  expect_equal(
    as.character(fs::path_file(fs::dir_ls(contrib_of(root), glob = "*.tgz"))),
    "R6_2.6.1.9000.tgz"
  )
})

test_that("a compiled development version replaces the prebuilt package of the same name", {
  gh_src <- withr::local_tempdir()
  make_pkg(fs::path(gh_src, "R6"), "R6", version = "2.6.1.9000")
  local_fake_docker("ok", github = gh_src)
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(R6 = c(Version = "2.6.1")))
  local_mocked_bindings(default_repo_url = function() repo$url)

  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("R6"),
    "packages.github" = list("r-lib/R6")
  ))
  suppressMessages(bind(root))

  expect_equal(
    as.character(fs::path_file(fs::dir_ls(contrib_of(root), glob = "*.tgz"))),
    "R6_2.6.1.9000.tgz"
  )
  expect_equal(unname(read.dcf(fs::path(contrib_of(root), "PACKAGES"))[, "Version"]), "2.6.1.9000")
  expect_equal(unlist(read_site_config(fs::path(root, "_site"))$packages$install), "R6")
})

test_that("an absolute local path is used as given; a missing one fails and leaves the old site", {
  local_fake_docker("ok")
  root <- local_collection()
  elsewhere <- make_pkg(fs::path(withr::local_tempdir(), "abspkg"), "abspkg")
  suppressMessages(settings_set(root, "packages.local" = list(as.character(elsewhere))))
  suppressMessages(bind(root))
  expect_true("abspkg" %in% unlist(read_site_config(fs::path(root, "_site"))$packages$install))

  before <- readLines(fs::path(root, "_site", "index.html"))
  suppressMessages(settings_set(root, "packages.local" = list("does/not/exist")))
  expect_error(suppressMessages(bind(root)), "does not exist")
  expect_identical(readLines(fs::path(root, "_site", "index.html")), before)
})

test_that("a collection that is itself the package does not copy its output or .git into the build", {
  log <- local_fake_docker("ok")
  root <- local_collection()
  make_pkg(root, "selfpkg")
  fs::dir_create(fs::path(root, ".git"))
  writeLines("ref", fs::path(root, ".git", "HEAD"))
  suppressMessages(settings_set(root, "packages.local" = list(".")))

  suppressMessages(bind(root))
  suppressMessages(bind(root)) # now an earlier _site exists
  # A second output directory, set in _webrarian.yml.
  set_output_dir(root, "docs")
  suppressMessages(bind(root))
  suppressMessages(bind(root)) # now docs/ exists

  # The fake logs each build's copied files as "./<pkg>/<path>" lines.
  copied <- grep("^\\./", readLines(log), value = TRUE)
  expect_false(any(grepl("_site/|docs/|\\.git/|\\.webrarian/", copied)))
  expect_true(any(grepl("selfpkg/R/f.R", copied, fixed = TRUE)))
})

test_that("a compiled package whose built DESCRIPTION has an invalid name fails the build", {
  gh_src <- withr::local_tempdir()
  make_pkg(fs::path(gh_src, "badpkg"), "bad_name")
  local_fake_docker("ok", github = gh_src)
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.github" = list("someone/badpkg")))
  expect_error(suppressMessages(bind(root)), "not a valid R package name")
})

test_that("a local package whose DESCRIPTION gives no valid package name fails before anything is copied", {
  local_fake_docker("ok")
  scratch <- withr::local_tempdir()
  local_mocked_bindings(docker_build_dir = function() fs::path(scratch, "build"))
  root <- local_collection()
  # Copied as <build>/packages/../../escape, that is <scratch>/escape.
  make_pkg(fs::path(root, "pkgs", "evil"), "../../escape")
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/evil")))

  expect_error(suppressMessages(bind(root)), "valid R package name")
  expect_false(fs::dir_exists(fs::path(scratch, "escape")))
  expect_false(fs::dir_exists(fs::path(scratch, "build")))

  # No Package field at all: the same error, not "subscript out of bounds".
  writeLines("Version: 0.1.0", fs::path(root, "pkgs", "evil", "DESCRIPTION"))
  expect_error(suppressMessages(bind(root)), "valid R package name")
})

test_that("the webR container compiles exactly the named packages, not rwasm's built-in forks", {
  # rwasm::add_pkg()'s default (remotes = NA) first resolves rwasm's list of
  # webR forks. Under the pkgdepends 0.9.1 that ghcr.io/r-wasm/webr:v0.6.0
  # ships, that resolution aborts ("`nrow(out)` must equal `1`",
  # r-lib/pkgdepends#462) before anything is compiled.
  script <- fs::path(withr::local_tempdir(), "build.R")
  local_fake_docker("ok", script = script)
  root <- local_collection()
  make_pkg(fs::path(root, "pkgs", "demo"), "demo")
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demo")))
  suppressMessages(bind(root))

  exprs <- as.list(parse(script, keep.source = FALSE))
  is_call_to <- function(e, name) is.call(e) && identical(e[[1]], as.name(name))
  calls <- Filter(function(e) is_call_to(e, "add_pkg"), exprs)
  expect_length(calls, 1L)
  # rwasm's own signature, so the arguments are matched as the container would.
  add_pkg <- function(
    packages,
    repo_dir = "./repo",
    remotes = NA,
    dependencies = FALSE,
    compress = TRUE
  ) {
    NULL
  }
  args <- as.list(match.call(add_pkg, calls[[1]]))[-1]
  expect_true("remotes" %in% names(args))
  expect_null(args[["remotes"]])
  expect_false(args[["dependencies"]])

  assigned <- Filter(
    function(e) is_call_to(e, "<-") && identical(e[[2]], as.name("packages")),
    exprs
  )
  expect_length(assigned, 1L)
  expect_equal(eval(assigned[[1]][[3]], baseenv()), "local::/build/packages/demo")
})

test_that("the package index reads rwasm's tarballs, which end in webR's filesystem index", {
  # rwasm's add_tar_index() (r-wasm/rwasm R/tar.R) appends a .vfs-index.json
  # entry, two zero blocks and a 16-byte "webR" hint inside the gzip stream of
  # every binary it builds. The fake docker writes plain tarballs, so without
  # this test only the real-Docker CI job would hand index_rows_from_tgz() one.
  dir <- withr::local_tempdir()
  tar_entries <- function(name, write) {
    # R's own tar, cut where rwasm cuts it: at the first all-zero block.
    src <- fs::path(dir, paste0("src-", name))
    fs::dir_create(src)
    write(src)
    archive <- fs::path(dir, paste0(name, ".tar"))
    withr::with_dir(src, utils::tar(archive, files = name, compression = "none", tar = "internal"))
    bytes <- readBin(archive, "raw", file.size(archive))
    zero <- Position(function(i) all(bytes[i + 1:512] == 0), seq(0, length(bytes) - 512, by = 512))
    bytes[seq_len((zero - 1) * 512)]
  }
  package <- tar_entries("demo", function(src) {
    fs::dir_create(fs::path(src, "demo"))
    writeLines(
      c("Package: demo", "Version: 0.1.0", "Imports: stats"),
      fs::path(src, "demo", "DESCRIPTION")
    )
  })
  json <- '{"files":[],"gzip":true}'
  index <- tar_entries(".vfs-index.json", function(src) {
    writeLines(json, fs::path(src, ".vfs-index.json"), sep = "")
  })
  hint <- c(
    charToRaw("webR"),
    raw(4),
    writeBin(length(package) %/% 512L + 1L, raw(), size = 4, endian = "big"),
    writeBin(nchar(json), raw(), size = 4, endian = "big")
  )
  tgz <- fs::path(dir, "demo_0.1.0.tgz")
  con <- gzfile(tgz, "wb", compression = 9)
  writeBin(c(package, index, raw(1024), hint), con)
  close(con)

  rows <- expect_no_warning(index_rows_from_tgz(tgz))
  expect_equal(unname(rows[1, c("Package", "Version", "Imports")]), c("demo", "0.1.0", "stats"))
})

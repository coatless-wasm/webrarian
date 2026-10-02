# bind() always builds into .webrarian/staging-* and swaps it in only after
# every step succeeded; clean = FALSE only changes what the new
# site starts from.

index_of <- function(root) readLines(fs::path(root, "_site", "index.html"))

# The build id in the site's marker, or NULL when there is no marker.
build_id_of <- function(root) {
  marker <- fs::path(root, "_site", ".webrarian-build")
  if (fs::file_exists(marker)) jsonlite::read_json(marker)$build_id
}

# The failure is injected in emit_cache_headers(), after emit_viewer() has
# written index.html. A CDN-engine page without a service worker is the same
# bytes on every build, so index.html alone cannot tell the old site from a
# half-rebuilt one: a bind() that deleted _site and rebuilt in place would
# still pass that check. A file only the old site has (sentinel.txt) and the
# old site's build id can: a delete-then-rebuild loses both.
test_that("a failing build leaves the previous site untouched and no staging behind", {
  root <- local_collection()
  suppressMessages(bind(root))
  before <- index_of(root)
  writeLines("old", fs::path(root, "_site", "sentinel.txt"))
  old_id <- build_id_of(root)
  expect_false(is.null(old_id))

  local_mocked_bindings(emit_cache_headers = function(...) stop("boom"))
  expect_error(suppressMessages(bind(root)), "boom")

  expect_true(fs::file_exists(fs::path(root, "_site", "sentinel.txt")))
  expect_identical(build_id_of(root), old_id)
  expect_identical(index_of(root), before)
  expect_length(staging_leftovers(root), 0L)
})

test_that("an interrupt mid-build leaves the previous site intact and no staging behind", {
  root <- local_collection()
  suppressMessages(bind(root))
  before <- index_of(root)
  writeLines("old", fs::path(root, "_site", "sentinel.txt"))
  old_id <- build_id_of(root)
  expect_false(is.null(old_id))

  local_mocked_bindings(emit_cache_headers = function(...) rlang::interrupt())
  result <- tryCatch(
    suppressMessages(bind(root)),
    interrupt = function(cnd) "interrupted"
  )

  expect_identical(result, "interrupted")
  expect_true(fs::file_exists(fs::path(root, "_site", "sentinel.txt")))
  expect_identical(build_id_of(root), old_id)
  expect_identical(index_of(root), before)
  expect_length(staging_leftovers(root), 0L)
})

test_that("a successful build replaces the site and leaves nothing in .webrarian/", {
  root <- local_collection()
  suppressMessages(bind(root))
  first <- jsonlite::read_json(fs::path(root, "_site", ".webrarian-build"))$build_id
  suppressMessages(bind(root))
  second <- jsonlite::read_json(fs::path(root, "_site", ".webrarian-build"))$build_id

  expect_false(identical(first, second))
  expect_length(staging_leftovers(root), 0L)
})

# build.clean: false means what it means in pyodidarian: the build is still staged, and a
# file of the previous site that this build did not write is carried over,
# unless it lies under a path the build owns. A copy there is stale by
# definition: the vfs-files/ copy of a file no longer included would stay
# publicly downloadable, and an old hashed bundle or engine is dead weight.
test_that("clean = FALSE keeps files the build does not write", {
  root <- local_collection()
  writeLines("x <- 1", fs::path(root, "a.R"))
  writeLines("y <- 2", fs::path(root, "b.R"))
  suppressMessages(settings_set(root, "files/include" = list("a.R", "b.R")))
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  expect_true(fs::file_exists(fs::path(site, "vfs-files", "b.R")))

  writeLines("example.org", fs::path(site, "CNAME"))
  fs::dir_create(fs::path(site, "notes"))
  writeLines("stays", fs::path(site, "notes", "extra.txt"))
  stale <- c(
    "exlibris-r.00000000.js",
    "webr/v0.0.1/R.js",
    "repo/bin/emscripten/contrib/4.6/old_0.1.tgz",
    "library/library-00000000.tgz",
    "LICENSES/OLD.md",
    "assets/brand/old.png",
    "assets/fonts/old.woff2",
    "assets/custom/logo.gif",
    "packages.json",
    "vfs-files.new-1/vfs-files/b.R"
  )
  for (rel in stale) {
    fs::dir_create(fs::path_dir(fs::path(site, rel)))
    writeLines("stale", fs::path(site, rel))
  }
  # b.R is no longer included.
  suppressMessages(settings_set(root, "files/include" = list("a.R")))

  suppressMessages(bind(root, clean = FALSE))

  expect_identical(readLines(fs::path(site, "CNAME")), "example.org")
  expect_true(fs::file_exists(fs::path(site, "notes", "extra.txt")))
  expect_true(fs::file_exists(fs::path(site, "vfs-files", "a.R")))
  expect_false(fs::file_exists(fs::path(site, "vfs-files", "b.R")))
  for (rel in stale) {
    expect_false(fs::file_exists(fs::path(site, rel)), info = rel)
  }
  expect_length(staging_leftovers(root), 0L)

  # Still staged: a failing clean = FALSE build leaves this site as it was.
  # The failure comes after the files step, so a build written into the live
  # site in place would already have replaced vfs-files/a.R.
  old_id <- build_id_of(root)
  writeLines("x <- 2", fs::path(root, "a.R"))
  expect_error(
    with_mocked_bindings(
      suppressMessages(bind(root, clean = FALSE)),
      emit_cache_headers = function(...) stop("boom")
    ),
    "boom"
  )
  expect_identical(readLines(fs::path(site, "vfs-files", "a.R")), "x <- 1")
  expect_identical(readLines(fs::path(site, "CNAME")), "example.org")
  expect_identical(build_id_of(root), old_id)
  expect_length(staging_leftovers(root), 0L)

  # The default replaces the whole site.
  suppressMessages(bind(root))
  expect_false(fs::file_exists(fs::path(site, "CNAME")))
  expect_false(fs::dir_exists(fs::path(site, "notes")))
})

test_that("build_owned_path() names only what a build writes", {
  owned <- c(
    "index.html",
    "_headers",
    "sw.js",
    "packages.json",
    ".webrarian-build",
    "exlibris-r.js",
    "exlibris-r.0123abcd.css",
    "vfs-files/data/a.csv",
    "webr/v0.6.0/R.wasm",
    "repo/bin/emscripten/contrib/4.6/PACKAGES",
    "library/library-0123abcd.tgz",
    "LICENSES/index.html",
    "assets/brand/logo.svg",
    "assets/fonts/Inter.woff2",
    "assets/custom/favicon.png",
    ".index.html.tmp-4242",
    "vfs-files.new-4242/old-vfs-files/a.R"
  )
  kept <- c(
    "CNAME",
    ".nojekyll",
    "robots.txt",
    "notes/index.html",
    "docs/exlibris-r.js",
    "assets/other/photo.png",
    "webrarian.txt",
    "repository/x",
    "vfs-files-notes.txt",
    ".well-known/security.txt"
  )
  expect_true(all(build_owned_path(owned)))
  expect_false(any(build_owned_path(kept)))
  expect_identical(build_owned_path(character()), logical(0))
})

test_that("clean_shelves() also removes staging directories an earlier crash left", {
  root <- local_collection()
  suppressMessages(bind(root))
  fs::dir_create(fs::path(root, ".webrarian", "staging-123-deadbeef"))
  fs::dir_create(fs::path(root, ".webrarian", "old-staging-123-deadbeef"))
  suppressMessages(clean_shelves(root))
  expect_length(staging_leftovers(root), 0L)
})

test_that("a failed Docker build removes its temporary build directory", {
  local_fake_docker(mode = "fail")
  pkg_root <- withr::local_tempdir()
  pkg <- fs::path(pkg_root, "demotools")
  fs::dir_create(pkg)
  writeLines(c("Package: demotools", "Version: 0.1.0"), fs::path(pkg, "DESCRIPTION"))
  out <- withr::local_tempdir()

  seen <- NULL
  local_mocked_bindings(docker_build_dir = function() {
    seen <<- fs::path(tempfile("webrarian-docker-test-"))
    seen
  })

  expect_error(
    suppressMessages(build_packages_rwasm_docker(
      character(),
      "demotools",
      pkg_root,
      out,
      "4.6",
      "0.6.0"
    )),
    "exit code 1"
  )
  expect_false(is.null(seen))
  expect_false(fs::dir_exists(seen))
})

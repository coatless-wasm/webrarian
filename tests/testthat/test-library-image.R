# Library images: a site's packages, installed, in the one-file format
# webr::mount() reads.

test_that("an image holds every file of the library, and its index points at their bytes", {
  lib <- local_fake_library(list(
    "alpha/DESCRIPTION" = "Package: alpha\nVersion: 1.0\n",
    "alpha/R/alpha" = strrep("x", 1500),
    "beta/NAMESPACE" = "export(b)\n",
    "beta/empty" = ""
  ))
  dest <- fs::path(withr::local_tempdir(), "lib.tgz")
  write_library_image(lib, dest)
  img <- read_library_image(dest)

  expect_setequal(
    names(img$files),
    c("alpha/DESCRIPTION", "alpha/R/alpha", "beta/NAMESPACE", "beta/empty")
  )
  expect_identical(rawToChar(img$files[["alpha/R/alpha"]]), strrep("x", 1500))
  expect_length(img$index$files, 4L)
  for (entry in img$index$files) {
    rel <- sub("^/", "", entry$filename)
    got <- if (entry$end > entry$start) img$bytes[(entry$start + 1):entry$end] else raw()
    expect_identical(got, img$files[[rel]], info = rel)
  }
})

test_that("R's own tar reads the image, with the index as its last member", {
  lib <- local_fake_library(list("alpha/DESCRIPTION" = "Package: alpha\n"))
  dest <- fs::path(withr::local_tempdir(), "lib.tgz")
  write_library_image(lib, dest)
  members <- utils::untar(dest, list = TRUE, tar = "internal")
  expect_identical(members, c("alpha/DESCRIPTION", ".vfs-index.json"))
})

test_that("a long path is stored whole, split into the ustar prefix", {
  rel <- file.path("pkg", strrep("d", 60), strrep("e", 60), "file.txt")
  lib <- local_fake_library(stats::setNames(list("x"), rel))
  dest <- fs::path(withr::local_tempdir(), "lib.tgz")
  write_library_image(lib, dest)
  expect_identical(names(read_library_image(dest)$files), rel)
  expect_identical(utils::untar(dest, list = TRUE, tar = "internal")[[1]], rel)
})

test_that("a path the tar format cannot hold is an error naming it", {
  expect_error(tar_header(strrep("a", 300), 1), "too long")
})

test_that("an old index inside a package binary is not copied into the image", {
  lib <- local_fake_library(list(
    "alpha/DESCRIPTION" = "x",
    ".vfs-index.json" = "{}",
    "alpha/.vfs-index.json" = "{}"
  ))
  dest <- fs::path(withr::local_tempdir(), "lib.tgz")
  write_library_image(lib, dest)
  expect_identical(names(read_library_image(dest)$files), "alpha/DESCRIPTION")
})

test_that("an image lists its files in the same order in every locale, so its name does not change", {
  lib <- local_fake_library(list("alpha/DESCRIPTION" = "a", "Beta/DESCRIPTION" = "b"))
  # A collation that puts "alpha" before "Beta", as most desktop locales do (testthat runs in C).
  suppressWarnings(withr::local_collate("en_US.UTF-8", .local_envir = environment()))
  skip_if_not(
    identical(sort(c("Beta", "alpha")), c("alpha", "Beta")),
    "no en_US collation on this machine"
  )
  dest <- fs::path(withr::local_tempdir(), "lib.tgz")
  write_library_image(lib, dest)
  expect_identical(
    utils::untar(dest, list = TRUE, tar = "internal"),
    c("Beta/DESCRIPTION", "alpha/DESCRIPTION", ".vfs-index.json")
  )
})

test_that("build_library_image() unpacks the repo's binaries into one content-named image", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  for (pkg in c("alpha", "beta")) {
    write_fake_binary(pkg_dir, pkg, "1.0")
  }
  write.dcf(
    data.frame(Package = c("alpha", "beta"), Version = "1.0"),
    fs::path(pkg_dir, "PACKAGES")
  )
  stale <- fs::path(site, "library", "library-00000000.tgz")
  fs::dir_create(fs::path_dir(stale))
  writeLines("old", stale)

  rel <- build_library_image(pkg_dir, site)
  expect_match(rel, "^library/library-[0-9a-f]{8}\\.tgz$")
  expect_identical(
    as.character(fs::dir_ls(fs::path(site, "library"))),
    as.character(fs::path(site, rel))
  )
  expect_setequal(
    names(read_library_image(fs::path(site, rel))$files),
    c("alpha/DESCRIPTION", "beta/DESCRIPTION")
  )

  # Same packages, same name: the name is a hash of the contents.
  expect_identical(build_library_image(pkg_dir, site), rel)
})

test_that("build_library_image() writes nothing for a repo with no binaries", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  expect_null(build_library_image(pkg_dir, site))
  expect_false(fs::dir_exists(fs::path(site, "library")))
})

# A rebuild in place (build.clean: false) can leave an older binary of a package next to the one
# the index names; unpacking both would merge two versions' files into one broken package.
test_that("build_library_image() takes each package's binary from the index, not every .tgz there", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  write_fake_binary(pkg_dir, "alpha", "1.0", files = list("R/alpha" = "old", "R/removed" = "stale"))
  write_fake_binary(pkg_dir, "alpha", "2.0", files = list("R/alpha" = "new"))
  write.dcf(data.frame(Package = "alpha", Version = "2.0"), fs::path(pkg_dir, "PACKAGES"))
  img <- read_library_image(fs::path(site, build_library_image(pkg_dir, site)))
  expect_setequal(names(img$files), c("alpha/DESCRIPTION", "alpha/R/alpha"))
  expect_identical(rawToChar(img$files[["alpha/R/alpha"]]), "new")
  expect_match(rawToChar(img$files[["alpha/DESCRIPTION"]]), "Version: 2.0", fixed = TRUE)
})

test_that("build_library_image() writes no image, and says why, when the index names a missing binary", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  write_fake_binary(pkg_dir, "alpha", "1.0")
  write.dcf(
    data.frame(Package = c("alpha", "beta"), Version = "1.0"),
    fs::path(pkg_dir, "PACKAGES")
  )
  expect_warning(res <- build_library_image(pkg_dir, site), "beta_1.0.tgz", fixed = TRUE)
  expect_null(res)
  expect_false(fs::dir_exists(fs::path(site, "library")))
})

# A mounted image makes the viewer skip webr::install() for the packages it holds, and that is
# what fetches a dependency the site does not bundle.
test_that("build_library_image() writes no image when a package needs one the repository does not hold", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  write_fake_binary(pkg_dir, "demotools", "0.1.0")
  write.dcf(
    data.frame(Package = "demotools", Version = "0.1.0", Imports = "glue (>= 1.6.0), utils"),
    fs::path(pkg_dir, "PACKAGES")
  )
  expect_message(res <- build_library_image(pkg_dir, site), "glue")
  expect_null(res)
  expect_false(fs::dir_exists(fs::path(site, "library")))
})

test_that("build_library_image() writes an image when every dependency is bundled or part of R", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  write_fake_binary(pkg_dir, "demotools", "0.1.0")
  write_fake_binary(pkg_dir, "glue", "1.8.0")
  write.dcf(
    data.frame(
      Package = c("demotools", "glue"),
      Version = c("0.1.0", "1.8.0"),
      Depends = c("R (>= 4.1.0)", NA),
      Imports = c("glue (>= 1.6.0), utils", "methods")
    ),
    fs::path(pkg_dir, "PACKAGES")
  )
  rel <- build_library_image(pkg_dir, site)
  expect_setequal(
    names(read_library_image(fs::path(site, rel))$files),
    c("demotools/DESCRIPTION", "glue/DESCRIPTION")
  )
})

# The image is one file holding every bundled package; some hosts refuse a single file over
# 25 MiB (Cloudflare Pages), so a large one is written with a message saying how to leave it out.
test_that("build_library_image() says how to leave out an image larger than some hosts accept", {
  site <- withr::local_tempdir()
  pkg_dir <- fs::path(site, "repo", "bin", "emscripten", "contrib", "4.6")
  fs::dir_create(pkg_dir)
  # Random bytes do not compress, so the gzipped image stays larger than the lowered threshold.
  withr::local_seed(1)
  noise <- rawToChar(as.raw(sample(c(1:9, 11:255), 4000, replace = TRUE)))
  write_fake_binary(pkg_dir, "alpha", "1.0", files = list("data/blob" = noise))
  write.dcf(data.frame(Package = "alpha", Version = "1.0"), fs::path(pkg_dir, "PACKAGES"))

  expect_message(
    rel <- build_library_image(pkg_dir, site, warn_bytes = 1000),
    "library-image: false",
    fixed = TRUE
  )
  expect_true(fs::file_exists(fs::path(site, rel)))
  expect_gt(fs::file_size(fs::path(site, rel)), 1000)

  # Under the default (25 MiB) the same image is written without a word.
  expect_no_message(build_library_image(pkg_dir, site))
})

# --- bind() ships the image -----------------------------------------------------

build_with_glue <- function(..., env = parent.frame()) {
  local_fake_engine(env = env)
  local_isolated_cache(env = env)
  repo <- local_fixture_repo(list(glue = c(Version = "1.8.0")), env = env)
  local_mocked_bindings(default_repo_url = function() repo$url, .env = env)
  # build.bundle-engine: true (the default outside the tests) copies glue into repo/.
  root <- local_collection(env = env, bundle_engine = TRUE)
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue"), ...))
  result <- suppressMessages(bind(root))
  list(root = root, site = fs::path(root, "_site"), result = result)
}

test_that("a build that bundles packages ships a library image and names it in the config", {
  skip_if_not_installed("httpuv")
  b <- build_with_glue()
  images <- unlist(read_site_config(b$site)$packages[["library-images"]])
  expect_length(images, 1L)
  expect_match(images, "^\\./library/library-[0-9a-f]{8}\\.tgz$")
  img <- read_library_image(fs::path(b$site, sub("^\\./", "", images)))
  expect_true("glue/DESCRIPTION" %in% names(img$files))
  # repo/ stays: it is the fallback when the image cannot be mounted.
  expect_length(
    fs::dir_ls(fs::path(b$site, "repo"), recurse = TRUE, regexp = "glue_[^/]*\\.tgz$"),
    1L
  )
  # The image counts as package bytes in bind()'s summary.
  expect_gte(
    b$result$size_by_part[["packages"]],
    fs::file_size(fs::path(b$site, sub("^\\./", "", images)))
  )
})

test_that("build.library-image: false ships no library image", {
  skip_if_not_installed("httpuv")
  b <- build_with_glue("build.library-image" = FALSE)
  expect_false(fs::dir_exists(fs::path(b$site, "library")))
  expect_null(read_site_config(b$site)$packages[["library-images"]])
})

# reading_room(watch = TRUE) runs rebuild_files_only() when only included files change: it
# rewrites the page's config from the one it finds there, and must carry the image over.
test_that("a file-only rebuild during watch keeps the library image in the config", {
  skip_if_not_installed("httpuv")
  b <- build_with_glue()
  before <- unlist(read_site_config(b$site)$packages[["library-images"]])
  expect_length(before, 1L)
  suppressMessages(rebuild_files_only(b$root))
  expect_identical(unlist(read_site_config(b$site)$packages[["library-images"]]), before)
  expect_true(fs::file_exists(fs::path(b$site, sub("^\\./", "", before))))
})

test_that("a site with no bundled packages ships no library image", {
  # local_collection() builds with build.bundle-engine: false: the page installs glue when it opens.
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  suppressMessages(bind(root))
  expect_false(fs::dir_exists(fs::path(root, "_site", "library")))
  expect_null(read_site_config(fs::path(root, "_site"))$packages[["library-images"]])
})

# A package's source directory, as test-compiled-packages.R makes them for the fake docker.
make_source_package <- function(dir, name, version = "0.1.0", imports = NULL) {
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

# With build.bundle-engine: false the site bundles only what it compiles. The compiled package's
# dependencies (glue here) come from the public repository when the page opens, fetched by
# webr::install(); a mounted image would report the package installed and skip that, and
# attaching it would fail.
test_that("a compiled package whose dependencies the site does not bundle ships no library image", {
  skip_if_not_installed("httpuv")
  gh_src <- withr::local_tempdir()
  make_source_package(fs::path(gh_src, "demotools"), "demotools", imports = "glue")
  local_fake_docker("ok", github = gh_src)
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.github" = list("someone/demotools")))
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  expect_length(
    fs::dir_ls(fs::path(site, "repo"), recurse = TRUE, regexp = "demotools_[^/]*\\.tgz$"),
    1L
  )
  expect_false(fs::dir_exists(fs::path(site, "library")))
  expect_null(read_site_config(site)$packages[["library-images"]])
})

test_that("library images are cached for good, by _headers and by the service worker", {
  rules <- cache_rules()
  paths <- vapply(rules, `[[`, character(1), "path")
  expect_identical(
    rules[[match("/library/*", paths)]]$headers[["Cache-Control"]],
    "public, max-age=31536000, immutable"
  )
  sw <- paste(
    readLines(system.file("templates", "sw.js", package = "webrarian"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl("\\/library\\/library-[0-9a-f]{8}\\.tgz$", sw, fixed = TRUE))
})

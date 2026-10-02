# Shared helpers for tests that build real collections.

# A fresh collection in its own temporary base directory, so tests can also
# create siblings of it. It holds one user file that must never be deleted.
# It loads the engine from the webR CDN (build.bundle-engine: false), so a
# bind() downloads nothing; pass bundle_engine = TRUE, usually together with
# local_fake_engine(), for a site that carries its engine and packages.
local_collection <- function(env = parent.frame(), name = "proj", bundle_engine = FALSE) {
  base <- withr::local_tempdir(.local_envir = env)
  root <- fs::path(base, name)
  fs::dir_create(root)
  suppressMessages(catalog(root, detect = FALSE))
  writeLines("precious <- TRUE", fs::path(root, "precious.R"))
  if (!isTRUE(bundle_engine)) {
    suppressMessages(settings_set(root, "build.bundle-engine" = FALSE))
  }
  root
}

# The wire config inlined into a built site's index.html.
read_site_config <- function(site) {
  html <- paste(readLines(fs::path(site, "index.html"), warn = FALSE), collapse = "\n")
  wire <- read_inline_viewer_config(html)
  testthat::expect_false(is.null(wire))
  wire
}

# Replace the ~40 MB webR engine download with a few placeholder files, for
# tests that build a site carrying its engine.
local_fake_engine <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    webr_assets_ensure = function(version = webr_assets_version()) {
      dir <- fs::path(tempdir(), paste0("fake-webr-engine-", version))
      if (!fs::dir_exists(dir)) {
        fs::dir_create(fs::path(dir, "vfs"))
        for (f in c("webr-worker.js", "R.js", "R.wasm", "libRblas.so", "vfs/usr.data")) {
          writeLines("fake", fs::path(dir, f))
        }
      }
      invisible(dir)
    },
    .env = env
  )
}

# bind() and clean_shelves() take the output directory from build.output-dir
# only; a test that builds elsewhere sets it first.
set_output_dir <- function(root, output_dir) {
  suppressMessages(settings_set(root, "build.output-dir" = output_dir))
  invisible(root)
}

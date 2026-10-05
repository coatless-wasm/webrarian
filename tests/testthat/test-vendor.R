# The R viewer no longer builds its own webR REPL bundle (js/viewer); it
# vendors exlibris's prebuilt exlibris-r.{js,css} into inst/viewer (see
# tools/vendor-exlibris.sh). These are hygiene checks that the vendored
# assets are the right ones - not stale, not the old webrarian-repl bundle,
# and not exlibris's Pyodide sibling bundle.

test_that("the vendored exlibris-r.js bundle is present and non-trivial", {
  path <- system.file("viewer", "exlibris-r.js", package = "webrarian")
  expect_true(nzchar(path))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 100 * 1024)
})

test_that("the vendored exlibris-r.css bundle is present and non-trivial", {
  path <- system.file("viewer", "exlibris-r.css", package = "webrarian")
  expect_true(nzchar(path))
  expect_true(file.exists(path))
  expect_gt(file.size(path), 1024)
})

test_that("no Pyodide bundle leaks into inst/viewer", {
  viewer_dir <- system.file("viewer", package = "webrarian")
  expect_true(nzchar(viewer_dir))
  files <- fs::dir_ls(viewer_dir, recurse = TRUE, type = "file")
  expect_length(grep("py", fs::path_file(files), ignore.case = TRUE), 0)
})

test_that("no old webrarian-repl bundle lingers in inst/viewer", {
  viewer_dir <- system.file("viewer", package = "webrarian")
  files <- fs::path_file(fs::dir_ls(viewer_dir, recurse = TRUE, type = "file"))
  expect_length(grep("webrarian-repl", files, fixed = TRUE), 0)
})

test_that("the host index.html references exlibris-r.js, not the old bundle names", {
  html <- paste(
    readLines(system.file("viewer", "index.html", package = "webrarian"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(html, "exlibris-r.js", fixed = TRUE)
  expect_false(grepl("webrarian-repl.js", html, fixed = TRUE))
  expect_false(grepl("app.js", html, fixed = TRUE))
})

test_that("the host index.html has the exact anchors emit_viewer()'s gsub targets", {
  html <- paste(
    readLines(system.file("viewer", "index.html", package = "webrarian"), warn = FALSE),
    collapse = "\n"
  )
  # <title>webrarian</title> -> patched to the project title
  expect_match(html, "<title>webrarian</title>", fixed = TRUE)
  # </head> -> exactly once, so both the ui-head and loading-style gsubs land
  expect_length(gregexpr("</head>", html, fixed = TRUE)[[1]], 1)
  # <body> -> the loading overlay is inserted right after it
  expect_match(html, "<body>", fixed = TRUE)
  # <script type="module" -> the inline __VIEWER_CONFIG__ script lands before it
  expect_match(html, '<script type="module"', fixed = TRUE)
  # copy_vendored_viewer() rewrites these two references to the hashed names
  expect_match(html, 'src="./exlibris-r.js"', fixed = TRUE)
  expect_match(html, 'href="./exlibris-r.css"', fixed = TRUE)
})

# --- provenance ------------------------------------------------------------
#
# tools/vendor-exlibris.sh writes inst/viewer/PROVENANCE.json recording the
# exlibris commit it copied from plus a sha256 of every vendored file. These
# checks re-hash what actually ships, so a stale bundle, a hand-edited one, or
# a schema that drifted out of lockstep with the runtime fails here instead of
# silently certifying R output against a contract the viewer no longer honors.

# sha256 through helper-vendor.R: base R on R >= 4.5 (so the Windows leg
# re-hashes too), else shasum/sha256sum.
sha256_file <- function(path) sha256_hex(path)

provenance_path <- function() {
  file.path(system.file("viewer", package = "webrarian"), "PROVENANCE.json")
}

test_that("the vendored bundle ships a well-formed PROVENANCE.json", {
  path <- provenance_path()
  expect_true(file.exists(path))

  prov <- jsonlite::fromJSON(path, simplifyVector = TRUE)

  # A full SHA, not a short ref and not the "unknown" the script records when
  # forced through --allow-dirty against a non-git tree.
  expect_match(prov$exlibrisCommit, "^[0-9a-f]{40}$")
  expect_true(nzchar(prov$exlibrisVersion))
  expect_match(prov$vendoredAt, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")

  # The human-readable pin (tag, branch, or short commit) the bundle was
  # vendored from. It is what a maintainer actually reads when deciding
  # whether the bundle is current, and tools/vendor-exlibris.sh must keep
  # emitting it - an earlier version of the script did not, which silently
  # dropped the pin on every re-vendor.
  expect_false(is.null(prov$exlibrisRef))
  expect_type(prov$exlibrisRef, "character")
  expect_length(prov$exlibrisRef, 1L)
  expect_true(nzchar(prov$exlibrisRef))
  expect_false(identical(prov$exlibrisRef, "unknown"))

  # Never ship a bundle vendored from a dirty exlibris checkout: those bytes
  # cannot be reproduced from the recorded commit.
  if (!is.null(prov$exlibrisDirty)) {
    expect_false(isTRUE(prov$exlibrisDirty))
  }

  # The runtime bundle and the schema the config contract test validates
  # against must both be accounted for - they have to move in lockstep.
  expect_true(all(
    c("exlibris-r.js", "exlibris-r.css", "../viewer-config.schema.json") %in%
      names(prov$files)
  ))
})

test_that("every sha256 in PROVENANCE.json matches the file that actually ships", {
  path <- provenance_path()
  skip_if_not(file.exists(path), "no inst/viewer/PROVENANCE.json")
  skip_if(is.na(sha256_hex(path)), "no way to compute sha256 here")

  viewer_dir <- system.file("viewer", package = "webrarian")
  prov <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  files <- as.list(prov$files)
  expect_gt(length(files), 0)

  for (key in names(files)) {
    target <- file.path(viewer_dir, key)
    expect_true(file.exists(target), info = key)
    actual <- sha256_file(normalizePath(target, mustWork = TRUE))
    # A utility that is on PATH but produced nothing parseable is a broken
    # toolchain, not a bad bundle (this bites on Windows, where shasum /
    # sha256sum come from Rtools/Strawberry and may choke on the path). Skip
    # rather than report a phantom hash mismatch; the Linux legs still enforce
    # the pins.
    skip_if(is.na(actual), "sha256 utility produced no usable output")
    expect_identical(
      actual,
      tolower(files[[key]]),
      info = paste0(
        key,
        " does not match its PROVENANCE.json hash - re-run ",
        "tools/vendor-exlibris.sh instead of hand-editing vendored files"
      )
    )
  }
})

test_that("tools/vendor-exlibris.sh still emits every PROVENANCE.json key", {
  # Source-checkout-only: tools/ is .Rbuildignore'd, so this file is absent
  # from the built package and from R CMD check's test copy.
  script <- test_path("..", "..", "tools", "vendor-exlibris.sh")
  skip_if_not(file.exists(script), "tools/vendor-exlibris.sh not in this tree")

  src <- paste(readLines(script, warn = FALSE), collapse = "\n")

  # Guards the drift the script has already had once: PROVENANCE.json grew a
  # field the generator did not write, so the next vendor run would have
  # dropped it. CI's bundle drift-check excludes PROVENANCE.json and cannot
  # catch that, hence this check.
  prov <- jsonlite::fromJSON(provenance_path(), simplifyVector = TRUE)
  for (key in names(prov)) {
    expect_true(
      grepl(paste0('"', key, '"'), src, fixed = TRUE),
      info = paste0(
        "tools/vendor-exlibris.sh never writes the '",
        key,
        "' key that inst/viewer/PROVENANCE.json carries"
      )
    )
  }
})

test_that("PROVENANCE.json's schema entry points at the vendored contract schema", {
  path <- provenance_path()
  skip_if_not(file.exists(path), "no inst/viewer/PROVENANCE.json")

  viewer_dir <- system.file("viewer", package = "webrarian")
  prov <- jsonlite::fromJSON(path, simplifyVector = TRUE)

  recorded <- normalizePath(
    file.path(viewer_dir, "../viewer-config.schema.json"),
    mustWork = TRUE
  )
  # ...which is exactly the copy tests/testthat/test-config-contract.R validates
  # the generated ViewerConfig against.
  expect_identical(
    recorded,
    normalizePath(
      system.file("viewer-config.schema.json", package = "webrarian"),
      mustWork = TRUE
    )
  )
})

# --- the theming and share-link contract ---
#
# The bundle vendored here must be exlibris at a commit that
# reads the share-link mode and the offline flag from the config, dropped the
# window.__VIEWER_ALLOW_URL_OVERRIDE__ global, and documents the theming
# variables webrarian sets for branding.

test_that("the vendored schema carries the share-links mode", {
  schema <- jsonlite::read_json(
    system.file("viewer-config.schema.json", package = "webrarian")
  )
  expect_setequal(
    unlist(schema$properties[["share-links"]]$enum),
    c("open", "fixed", "off")
  )
})

test_that("the vendored schema carries the offline flag", {
  # An offline site says so in its config (offline sites); the schema has additionalProperties: false, so a viewer
  # that does not know the key would reject every offline site.
  schema <- jsonlite::read_json(
    system.file("viewer-config.schema.json", package = "webrarian")
  )
  expect_identical(schema$properties$offline$type, "boolean")
})

test_that("the vendored viewer no longer reads the allow-url-override global", {
  path <- system.file("viewer", "exlibris-r.js", package = "webrarian")
  js <- readChar(path, file.size(path), useBytes = TRUE)
  expect_false(grepl("__VIEWER_ALLOW_URL_OVERRIDE__", js, fixed = TRUE))
})

test_that("the vendored stylesheet exposes the theming variables", {
  path <- system.file("viewer", "exlibris-r.css", package = "webrarian")
  css <- readChar(path, file.size(path), useBytes = TRUE)
  for (var in c(
    "--accent-color",
    "--bg-primary",
    "--bg-secondary",
    "--text-primary",
    "--font-body",
    "--font-mono"
  )) {
    expect_true(grepl(var, css, fixed = TRUE), info = var)
  }
})

test_that("PROVENANCE.json records the webR client compiled into the viewer", {
  prov <- jsonlite::fromJSON(provenance_path(), simplifyVector = TRUE)
  expect_match(prov$webrClientVersion, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
})

test_that("tools/vendor-exlibris.sh lets the caller name the ref it vendored", {
  script <- test_path("..", "..", "tools", "vendor-exlibris.sh")
  skip_if_not(file.exists(script), "tools/vendor-exlibris.sh not in this tree")
  src <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_match(src, 'EXLIBRIS_REF="$VENDOR_EXLIBRIS_REF"', fixed = TRUE)
})

# --- the persistence and library-image contract ---
#
# The bundle vendored here is exlibris at a commit that reads persist-edits
# and packages.library-images, answers the exlibris-embed/1 postMessage API and
# links each site's LICENSES/index.html.

test_that("the vendored schema carries the persistence and library-image wire keys", {
  schema <- jsonlite::read_json(system.file("viewer-config.schema.json", package = "webrarian"))
  expect_identical(schema$properties[["persist-edits"]]$type, "boolean")
  expect_identical(schema$properties$packages$properties[["library-images"]]$type, "array")
})

test_that("the vendored viewer carries the persistence and library-image features", {
  path <- system.file("viewer", "exlibris-r.js", package = "webrarian")
  js <- readChar(path, file.size(path), useBytes = TRUE)
  for (text in c(
    "exlibris-embed/1",
    "LICENSES/index.html",
    "exlibris-edits",
    "/exlibris/library/",
    "Reset files"
  )) {
    expect_true(grepl(text, js, fixed = TRUE), info = text)
  }
})

# The notices for code nested inside the bundle come from exlibris's generator;
# webrarian keeps no hand-written copy of them.
test_that("the vendored third-party notices carry exlibris's nested-code section", {
  notices <- list.files(
    system.file("viewer", package = "webrarian"),
    pattern = "^THIRD-PARTY.*[.]md$",
    full.names = TRUE
  )
  text <- paste(
    unlist(lapply(notices, readLines, warn = FALSE, encoding = "UTF-8")),
    collapse = "\n"
  )
  expect_true(grepl("## Code nested inside bundled packages", text, fixed = TRUE))
  expect_false(file.exists(test_path("..", "..", "tools", "notices", "THIRD-PARTY-nested.md")))
})

# --- the release pin ---
#
# webrarian 0.1.2 ships exlibris 0.1.2, pinned by its tag, so anyone can rebuild
# the minified bundle from the public repository.

test_that("the vendored viewer is exlibris 0.1.2 at its release tag", {
  prov <- jsonlite::fromJSON(provenance_path(), simplifyVector = TRUE)
  expect_identical(prov$exlibrisRef, "v0.1.2")
  expect_identical(prov$exlibrisVersion, "0.1.2")
  expect_false(prov$exlibrisDirty)
  expect_match(prov$exlibrisCommit, "^[0-9a-f]{40}$")
  js <- system.file("viewer", "exlibris-r.js", package = "webrarian")
  expect_match(readChar(js, 200L, useBytes = TRUE), "exlibris 0.1.2", fixed = TRUE)
})

# exlibris's generator appends the notices esbuild's metafile cannot see: the
# code nested inside jszip.min.js and Bellard's xterm notice (a rule shared with pyodidarian). webrarian keeps no hand-written copy of them.
test_that("the vendored third-party notice carries the code nested inside the bundle", {
  notices <- list.files(
    system.file("viewer", package = "webrarian"),
    pattern = "^THIRD-PARTY.*\\.md$",
    full.names = TRUE
  )
  expect_gte(length(notices), 1L)
  for (notice in notices) {
    text <- paste(readLines(notice, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    for (name in c("lie", "immediate", "setimmediate", "pako", "Bellard")) {
      expect_match(
        text,
        paste0("\\b", name, "\\b"),
        perl = TRUE,
        info = paste(basename(notice), name)
      )
    }
  }
})

# The license notices name exactly the exlibris pin PROVENANCE.json records,
# and tools/vendor-exlibris.sh regenerates them from it.

viewer_file <- function(name) system.file("viewer", name, package = "webrarian")
read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
prov <- function() jsonlite::read_json(viewer_file("PROVENANCE.json"))

# The part of THIRD-PARTY.md webrarian writes; exlibris's own notices follow.
third_party_header <- function() {
  lines <- readLines(viewer_file("THIRD-PARTY.md"), warn = FALSE, encoding = "UTF-8")
  end <- grep("^# Third-party notices: exlibris-r", lines)[1]
  paste(lines[seq_len(end - 1L)], collapse = "\n")
}

test_that("THIRD-PARTY.md names the vendored commit, ref and bundle digests", {
  p <- prov()
  header <- third_party_header()
  expect_match(header, p$exlibrisCommit, fixed = TRUE)
  expect_match(header, p$exlibrisRef, fixed = TRUE)
  expect_match(header, p$files[["exlibris-r.js"]], fixed = TRUE)
  expect_match(header, p$files[["exlibris-r.css"]], fixed = TRUE)
})

test_that("no notice names any other commit or digest", {
  p <- prov()
  known <- unlist(c(p$files, p$sources))
  for (text in list(third_party_header(), read_text(viewer_file("LICENSE.webR.md")))) {
    commits <- unique(regmatches(text, gregexpr("\\b[0-9a-f]{40}\\b", text))[[1]])
    expect_true(all(commits == p$exlibrisCommit))
    digests <- unique(regmatches(text, gregexpr("\\b[0-9a-f]{64}\\b", text))[[1]])
    expect_true(all(digests %in% known))
  }
})

test_that("LICENSE.webR.md names the webR client and says where the runtime ships", {
  text <- read_text(viewer_file("LICENSE.webR.md"))
  expect_match(text, prov()$webrClientVersion, fixed = TRUE)
  expect_match(text, "LICENSES/webR.md", fixed = TRUE)
  expect_false(grepl("is not shipped", text, fixed = TRUE))
})

test_that("the vendored THIRD-PARTY.md ends with the code nested inside bundled packages", {
  lines <- readLines(viewer_file("THIRD-PARTY.md"), warn = FALSE, encoding = "UTF-8")
  start <- match("## Code nested inside bundled packages", lines)
  expect_false(is.na(start))
  nested <- lines[start:length(lines)]
  rows <- sub("^\\| `([^`]+)` \\|.*$", "\\1", grep("^\\| `[^`]+` \\|", nested, value = TRUE))
  expect_true(all(c("lie", "immediate", "setimmediate", "pako") %in% rows))
  expect_true(any(grepl("Fabrice Bellard", nested, fixed = TRUE)))
})

test_that("the vendored notices carry no unrendered placeholder, and PROVENANCE.json hashes them", {
  for (f in c("THIRD-PARTY.md", "LICENSE.webR.md")) {
    expect_false(grepl("@[A-Z][A-Z_]*@", read_text(viewer_file(f))), info = f)
  }
  expect_true(all(c("THIRD-PARTY.md", "EXCEPTION.md", "LICENSE.webR.md") %in% names(prov()$files)))
  expect_match(
    read_text(viewer_file("EXCEPTION.md")),
    "^# Additional permission under GNU AGPL version 3 section 7 \\(exlibris runtime exception\\)"
  )
})

test_that("LICENSE.note carries the output exception and the vendored pin, and the License field is AGPL-3", {
  note <- test_path("..", "..", "LICENSE.note")
  skip_if_not(file.exists(note), "LICENSE.note is only in the source tree")
  text <- read_text(note)
  expect_match(
    text,
    "Additional permission under GNU AGPL version 3 section 7 (output exception)",
    fixed = TRUE
  )
  expect_match(
    text,
    "The files that webrarian writes into a generated site or mirror",
    fixed = TRUE
  )
  expect_match(text, prov()$exlibrisCommit, fixed = TRUE)
  expect_false(grepl("@[A-Z][A-Z_]*@", text))
  expect_identical(
    unname(read.dcf(system.file("DESCRIPTION", package = "webrarian"))[, "License"]),
    "AGPL-3"
  )
})

# R installs no LICENSE.note, so the exception also ships as
# inst/OUTPUT-EXCEPTION.md, which every site's LICENSES/webrarian.md carries.
test_that("the installed output exception says word for word what LICENSE.note says", {
  installed <- system.file("OUTPUT-EXCEPTION.md", package = "webrarian")
  expect_true(nzchar(installed))
  lines <- readLines(installed, warn = FALSE, encoding = "UTF-8")
  expect_identical(
    lines[[1]],
    "# Additional permission under GNU AGPL version 3 section 7 (output exception)"
  )
  squash <- function(x) gsub("\\s+", " ", trimws(paste(x, collapse = " ")))
  body <- squash(lines[-1])
  expect_match(body, "^The files that webrarian writes into a generated site or mirror")
  note <- test_path("..", "..", "LICENSE.note")
  skip_if_not(file.exists(note), "LICENSE.note is only in the source tree")
  expect_true(grepl(body, squash(readLines(note, warn = FALSE, encoding = "UTF-8")), fixed = TRUE))
})

source_tree <- function() {
  root <- test_path("..", "..")
  # tools/ is build-ignored, so this is the git checkout, not R CMD check's copy.
  skip_if_not(dir.exists(file.path(root, "tools")), "not the source tree")
  normalizePath(root)
}

test_that("git checks the hashed vendored files out byte for byte on every platform", {
  root <- source_tree()
  skip_if(!nzchar(Sys.which("git")), "needs git")
  inside <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(root), "rev-parse", "--is-inside-work-tree"),
    stdout = TRUE,
    stderr = FALSE
  ))
  skip_if_not(identical(inside, "true"), "not a git checkout")
  paths <- c(
    "inst/viewer/exlibris-r.js",
    "inst/viewer/exlibris-r.css",
    "inst/viewer/THIRD-PARTY.md",
    "inst/viewer/EXCEPTION.md",
    "inst/viewer/LICENSE.webR.md",
    "inst/viewer/PROVENANCE.json",
    "inst/viewer/index.html",
    "inst/viewer-config.schema.json",
    "tools/notices/THIRD-PARTY-header.md.in"
  )
  out <- system2("git", c("-C", shQuote(root), "check-attr", "text", "--", paths), stdout = TRUE)
  # "<path>: text: unset" is what `-text` gives; "unspecified" means CRLF on Windows.
  expect_identical(sub("^.*: text: ", "", out), rep("unset", length(paths)))
})

test_that("CI's vendor-check builds the exlibris commit PROVENANCE.json pins, not a hard-coded one", {
  wf_path <- file.path(source_tree(), ".github", "workflows", "viewer.yml")
  job <- yaml::read_yaml(wf_path)$jobs[["vendor-check"]]
  checkout <- Filter(function(s) identical(s$with$repository, "coatless-wasm/exlibris"), job$steps)
  expect_length(checkout, 1L)
  expect_identical(checkout[[1]]$with$ref, "${{ steps.pin.outputs.commit }}")
  vendor <- Filter(function(s) startsWith(s$run %||% "", "tools/vendor-exlibris.sh"), job$steps)
  expect_length(vendor, 1L)
  expect_identical(vendor[[1]]$env$VENDOR_EXLIBRIS_REF, "${{ steps.pin.outputs.ref }}")
  # It vendors the dist/ it just built, and says so: --from-dist means the same
  # in pyodidarian's script, where the bare form builds a pinned ref instead.
  expect_match(vendor[[1]]$run, "--from-dist", fixed = TRUE)
  text <- paste(readLines(wf_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("vendored-2026-08-02|37e1595", text))
})

vendor_script <- function() {
  script <- test_path("..", "..", "tools", "vendor-exlibris.sh")
  skip_if_not(file.exists(script), "tools/ is not in this tree")
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")) || !nzchar(Sys.which("git")), "needs bash and git")
  normalizePath(script)
}

# Runs a copy of the script from a throwaway repository tree, with
# tools/notices/ beside it when this tree has one. The script writes into the
# tree above its own directory unless it honors WEBRARIAN_VENDOR_ROOT, so an
# older or regressed script can only ever write into `repo`, never into this
# checkout's tracked inst/viewer/. `ref = NULL` leaves VENDOR_EXLIBRIS_REF
# unset.
run_vendor <- function(
  script,
  checkout,
  dest,
  args = character(),
  ref = "test-ref",
  env = parent.frame()
) {
  repo <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(fs::path(repo, "tools"))
  copy <- fs::path(repo, "tools", "vendor-exlibris.sh")
  fs::file_copy(script, copy)
  notices <- fs::path(fs::path_dir(script), "notices")
  if (fs::dir_exists(notices)) {
    fs::dir_copy(notices, fs::path(repo, "tools", "notices"))
  }
  withr::local_envvar(
    WEBRARIAN_VENDOR_ROOT = dest,
    VENDOR_EXLIBRIS_REF = if (is.null(ref)) NA else ref
  )
  withr::local_path(R.home("bin"), action = "prefix")
  out <- suppressWarnings(system2(
    "bash",
    c(shQuote(copy), shQuote(args), shQuote(checkout)),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(status = attr(out, "status") %||% 0L, output = out, repo = repo)
}

test_that("tools/vendor-exlibris.sh renders every notice from the PROVENANCE.json it writes", {
  script <- vendor_script()
  checkout <- local_fake_exlibris()
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest)
  expect_equal(run$status, 0L, info = paste(run$output, collapse = "\n"))
  # WEBRARIAN_VENDOR_ROOT was honored: nothing landed beside the script's copy.
  expect_false(fs::dir_exists(fs::path(run$repo, "inst")))
  expect_false(fs::file_exists(fs::path(run$repo, "LICENSE.note")))

  viewer <- fs::path(dest, "inst", "viewer")
  p <- jsonlite::read_json(fs::path(viewer, "PROVENANCE.json"))
  # The core schema both tools' vendoring scripts write, plus the engine client.
  expect_setequal(
    names(p),
    c(
      "exlibrisCommit",
      "exlibrisRef",
      "exlibrisVersion",
      "exlibrisDirty",
      "vendoredAt",
      "files",
      "sources",
      "webrClientVersion"
    )
  )
  head_commit <- system2("git", c("-C", shQuote(checkout), "rev-parse", "HEAD"), stdout = TRUE)
  expect_identical(p$exlibrisCommit, head_commit)
  expect_identical(p$exlibrisRef, "test-ref")
  expect_identical(p$webrClientVersion, "0.6.0")
  for (f in names(p$files)) {
    expect_identical(sha256_hex(fs::path(viewer, f)), p$files[[f]], info = f)
  }
  expect_identical(
    p$sources[["THIRD-PARTY-r.md"]],
    sha256_hex(fs::path(checkout, "dist", "THIRD-PARTY-r.md"))
  )

  third_party <- read_text(fs::path(viewer, "THIRD-PARTY.md"))
  expect_match(third_party, head_commit, fixed = TRUE)
  expect_match(third_party, p$files[["exlibris-r.js"]], fixed = TRUE)
  expect_match(third_party, "| `webr` | 0.6.0 |", fixed = TRUE)
  expect_match(third_party, "## Code nested inside bundled packages", fixed = TRUE)
  expect_match(third_party, "Fabrice Bellard", fixed = TRUE)
  note <- read_text(fs::path(dest, "LICENSE.note"))
  expect_match(note, head_commit, fixed = TRUE)
  expect_match(note, "(output exception)", fixed = TRUE)
  expect_match(read_text(fs::path(viewer, "LICENSE.webR.md")), "`webr` 0.6.0", fixed = TRUE)
  expect_identical(
    readLines(fs::path(viewer, "EXCEPTION.md")),
    readLines(fs::path(checkout, "dist", "EXCEPTION.md"))
  )
  for (f in c("THIRD-PARTY.md", "LICENSE.webR.md")) {
    expect_false(grepl("@[A-Z][A-Z_]*@", read_text(fs::path(viewer, f))), info = f)
  }
})

test_that("tools/vendor-exlibris.sh refuses a checkout without the generated notices and writes nothing", {
  script <- vendor_script()
  checkout <- local_fake_exlibris(with_third_party = FALSE)
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest)
  expect_false(run$status == 0L)
  expect_true(any(grepl("THIRD-PARTY-r.md", run$output, fixed = TRUE)))
  expect_false(fs::dir_exists(fs::path(dest, "inst")))
  expect_false(fs::file_exists(fs::path(dest, "LICENSE.note")))
})

test_that("tools/vendor-exlibris.sh refuses an exlibris LICENSE that is not the AGPL-3 text R ships", {
  script <- vendor_script()
  checkout <- local_fake_exlibris()
  writeLines("MIT License", fs::path(checkout, "dist", "LICENSE"))
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest)
  expect_false(run$status == 0L)
  expect_true(any(grepl("AGPL", run$output, fixed = TRUE)))
  expect_false(fs::dir_exists(fs::path(dest, "inst")))
})

# exlibris's generator appends the code nested inside bundled packages; a dist/
# built before it would ship the viewer without those notices.
test_that("tools/vendor-exlibris.sh refuses notices without the code nested inside bundled packages", {
  script <- vendor_script()
  checkout <- local_fake_exlibris(with_nested = FALSE)
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest)
  expect_false(run$status == 0L)
  expect_true(any(grepl("nested inside bundled packages", run$output, fixed = TRUE)))
  expect_false(fs::dir_exists(fs::path(dest, "inst")))
})

# The option pyodidarian's script shares: vendor an exlibris ref, built from
# scratch in a throwaway worktree.
test_that("--ref builds that exlibris ref in a throwaway worktree and records it", {
  script <- vendor_script()
  checkout <- local_fake_exlibris(tag = "v9.9.9")
  local_fake_npm(fs::path(checkout, "dist"))
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest, args = c("--ref", "v9.9.9"), ref = NULL)
  expect_equal(run$status, 0L, info = paste(run$output, collapse = "\n"))
  p <- jsonlite::read_json(fs::path(dest, "inst", "viewer", "PROVENANCE.json"))
  expect_identical(p$exlibrisRef, "v9.9.9")
  expect_identical(
    p$exlibrisCommit,
    system2("git", c("-C", shQuote(checkout), "rev-parse", "HEAD"), stdout = TRUE)
  )
  expect_false(p$exlibrisDirty)
  expect_identical(
    p$files[["exlibris-r.js"]],
    sha256_hex(fs::path(checkout, "dist", "exlibris-r.js"))
  )
  # The worktree is gone again: only the checkout itself is listed.
  expect_length(system2("git", c("-C", shQuote(checkout), "worktree", "list"), stdout = TRUE), 1L)
})

# `--from-dist <checkout>` and `--ref <ref> <checkout>` mean the same in
# pyodidarian's script; only the bare form differs there, where
# it builds a pinned ref. Here the bare form copies dist/, and --from-dist is
# its explicit name.
test_that("--from-dist vendors the checkout's built dist/, exactly as a bare run does", {
  script <- vendor_script()
  checkout <- local_fake_exlibris()
  bare_dest <- withr::local_tempdir()
  bare <- run_vendor(script, checkout, bare_dest)
  expect_equal(bare$status, 0L, info = paste(bare$output, collapse = "\n"))
  dest <- withr::local_tempdir()
  run <- run_vendor(script, checkout, dest, args = "--from-dist")
  expect_equal(run$status, 0L, info = paste(run$output, collapse = "\n"))

  p <- jsonlite::read_json(fs::path(dest, "inst", "viewer", "PROVENANCE.json"))
  expect_identical(
    p$exlibrisCommit,
    system2("git", c("-C", shQuote(checkout), "rev-parse", "HEAD"), stdout = TRUE)
  )
  expect_identical(p$exlibrisRef, "test-ref")
  expect_identical(
    p$files[["exlibris-r.js"]],
    sha256_hex(fs::path(checkout, "dist", "exlibris-r.js"))
  )
  # The same bytes and notices as the bare run (only vendoredAt may differ).
  bare_p <- jsonlite::read_json(fs::path(bare_dest, "inst", "viewer", "PROVENANCE.json"))
  expect_identical(p$files, bare_p$files)
  expect_identical(
    readLines(fs::path(dest, "LICENSE.note")),
    readLines(fs::path(bare_dest, "LICENSE.note"))
  )
})

test_that("--ref and --from-dist together are refused before anything is looked up or written", {
  script <- vendor_script()
  checkout <- local_fake_exlibris(tag = "v9.9.9")
  for (args in list(c("--ref", "v9.9.9", "--from-dist"), c("--from-dist", "--ref", "v9.9.9"))) {
    dest <- withr::local_tempdir()
    run <- run_vendor(script, checkout, dest, args = args, ref = NULL)
    expect_equal(run$status, 2L, info = paste(args, collapse = " "))
    expect_true(any(grepl(
      "error: --ref and --from-dist cannot be combined",
      run$output,
      fixed = TRUE
    )))
    expect_false(fs::dir_exists(fs::path(dest, "inst")))
    expect_false(fs::file_exists(fs::path(dest, "LICENSE.note")))
  }
  # Refused before the checkout is looked for: a path with no checkout gets the
  # same answer, not "no exlibris git checkout".
  nowhere <- fs::path(withr::local_tempdir(), "nowhere")
  missing <- run_vendor(
    script,
    nowhere,
    withr::local_tempdir(),
    args = c("--from-dist", "--ref", "v9.9.9")
  )
  expect_equal(missing$status, 2L)
  expect_true(any(grepl("cannot be combined", missing$output, fixed = TRUE)))
})

# pyodidarian's test_refuses_a_dirty_tree_unless_allowed, here:
# --allow-dirty lets a copy of dist/ come from a dirty checkout, and is refused
# where it would do nothing. --ref builds a clean worktree, so this script
# refuses it with --ref; pyodidarian's refuses it without --from-dist, since
# its bare form builds a ref too. `--from-dist --allow-dirty` works in both.
test_that("--allow-dirty vendors a dirty checkout's dist/ and is refused with --ref", {
  script <- vendor_script()
  checkout <- local_fake_exlibris(tag = "v9.9.9")
  # dist/ is git-ignored, as in exlibris, so the change is to a tracked file.
  writeLines(
    '{"$id": "viewer-config", "changed": true}',
    fs::path(checkout, "src", "core", "viewer-config.schema.json")
  )

  refused_dest <- withr::local_tempdir()
  refused <- run_vendor(script, checkout, refused_dest, args = "--from-dist")
  expect_equal(refused$status, 1L, info = paste(refused$output, collapse = "\n"))
  expect_true(any(grepl("dirty working tree", refused$output, fixed = TRUE)))
  expect_false(fs::dir_exists(fs::path(refused_dest, "inst")))
  expect_false(fs::file_exists(fs::path(refused_dest, "LICENSE.note")))

  for (args in list(c("--from-dist", "--allow-dirty"), "--allow-dirty")) {
    dest <- withr::local_tempdir()
    allowed <- run_vendor(script, checkout, dest, args = args)
    expect_equal(allowed$status, 0L, info = paste(c(args, allowed$output), collapse = "\n"))
    prov_path <- fs::path(dest, "inst", "viewer", "PROVENANCE.json")
    expect_true(fs::file_exists(prov_path), info = paste(args, collapse = " "))
    if (fs::file_exists(prov_path)) {
      expect_true(jsonlite::read_json(prov_path)$exlibrisDirty, info = paste(args, collapse = " "))
    }
  }

  # Refused before the checkout is looked for or anything is written, in
  # either order and without npm (the worktree would never be built).
  for (args in list(c("--ref", "v9.9.9", "--allow-dirty"), c("--allow-dirty", "--ref", "v9.9.9"))) {
    dest <- withr::local_tempdir()
    ignored <- run_vendor(script, checkout, dest, args = args, ref = NULL)
    expect_equal(ignored$status, 2L, info = paste(args, collapse = " "))
    expect_true(
      any(grepl(
        "error: --allow-dirty applies only when copying a checkout's dist/ (--from-dist, or the bare form here)",
        ignored$output,
        fixed = TRUE
      )),
      info = paste(args, collapse = " ")
    )
    expect_false(fs::dir_exists(fs::path(dest, "inst")))
    expect_false(fs::file_exists(fs::path(dest, "LICENSE.note")))
  }
})

# sha256 of a file as lowercase hex: base R (tools::sha256sum, R >= 4.5), else
# shasum/sha256sum, else NA.
sha256_hex <- function(path) {
  tools_ns <- asNamespace("tools")
  if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
    return(tolower(unname(get("sha256sum", envir = tools_ns)(as.character(path)))))
  }
  tool <- if (nzchar(Sys.which("shasum"))) {
    c("shasum", "-a", "256")
  } else if (nzchar(Sys.which("sha256sum"))) {
    "sha256sum"
  } else {
    NULL
  }
  if (is.null(tool)) {
    return(NA_character_)
  }
  out <- suppressWarnings(system2(
    tool[[1]],
    c(tool[-1], shQuote(path)),
    stdout = TRUE,
    stderr = FALSE
  ))
  hash <- sub("^([0-9a-fA-F]{64}).*$", "\\1", out[[1]])
  if (grepl("^[0-9a-fA-F]{64}$", hash)) tolower(hash) else NA_character_
}

# The last section of exlibris's THIRD-PARTY-r.md, as its generator writes it: the code nested inside bundled packages.
fake_nested_section <- function() {
  c(
    "## Code nested inside bundled packages",
    "",
    "| Package | Version | License | Inside |",
    "|---|---|---|---|",
    "| `lie` | 3.3.0 | MIT | `jszip` |",
    "| `immediate` | 3.0.6 | MIT | `jszip` |",
    "| `setimmediate` | 1.0.5 | MIT | `jszip` |",
    "| `pako` | 1.0.11 | (MIT AND Zlib) | `jszip` |",
    "| `jslinux vt100 (via term.js)` | 2011 | MIT | `@xterm/xterm` |",
    "",
    "### `jslinux vt100 (via term.js)` 2011 (inside `@xterm/xterm`): `css/xterm.css`",
    "",
    "````text",
    "Copyright (c) 2011 Fabrice Bellard",
    "````"
  )
}

# A committed, clean exlibris checkout with a built dist/ and node_modules/webr
# (both git-ignored, as in the real repo), for running tools/vendor-exlibris.sh
# against. `with_nested = FALSE` leaves out the nested-code section of
# THIRD-PARTY-r.md; `tag` tags the commit.
local_fake_exlibris <- function(
  env = parent.frame(),
  with_third_party = TRUE,
  with_nested = TRUE,
  tag = NULL
) {
  dir <- withr::local_tempdir(.local_envir = env)
  write <- function(rel, lines) {
    fs::dir_create(fs::path_dir(fs::path(dir, rel)))
    writeLines(lines, fs::path(dir, rel))
  }
  write("package.json", c("{", '  "name": "exlibris",', '  "version": "0.0.1"', "}"))
  write("src/core/viewer-config.schema.json", '{"$id": "viewer-config"}')
  write(".gitignore", c("dist/", "node_modules/"))
  write("node_modules/webr/package.json", c("{", '  "name": "webr",', '  "version": "0.6.0"', "}"))
  write("dist/exlibris-r.js", "/*! exlibris 0.0.1 exlibris-r */ console.log('r');")
  write("dist/exlibris-r.css", "/*! exlibris 0.0.1 exlibris-r */ :root {}")
  write(
    "dist/EXCEPTION.md",
    "# Additional permission under GNU AGPL version 3 section 7 (exlibris runtime exception)"
  )
  if (with_third_party) {
    write(
      "dist/THIRD-PARTY-r.md",
      c(
        "# Third-party notices: exlibris-r",
        "",
        "| Package | Version | License | Bytes |",
        "|---|---|---|---|",
        "| `webr` | 0.6.0 | SEE LICENSE IN LICENCE.md | 10 |",
        if (with_nested) c("", fake_nested_section())
      )
    )
  }
  fs::file_copy(file.path(R.home("share"), "licenses", "AGPL-3"), fs::path(dir, "dist", "LICENSE"))
  # A developer's global git config may sign commits or run hooks; neither may
  # stop the fake commit, and a failed git step must fail here, not later as
  # a confusing notices mismatch.
  git <- function(...) {
    args <- c(
      "-c",
      "commit.gpgsign=false",
      "-c",
      "tag.gpgsign=false",
      "-c",
      "core.hooksPath=/dev/null",
      ...
    )
    status <- system2("git", args, stdout = FALSE, stderr = FALSE)
    if (!identical(as.integer(status), 0L)) {
      stop(
        "`git ",
        paste(c(...), collapse = " "),
        "` failed (status ",
        status,
        ") in the fake exlibris checkout ",
        dir,
        call. = FALSE
      )
    }
  }
  withr::with_dir(dir, {
    withr::with_envvar(
      c(
        GIT_AUTHOR_NAME = "t",
        GIT_AUTHOR_EMAIL = "t@example.org",
        GIT_COMMITTER_NAME = "t",
        GIT_COMMITTER_EMAIL = "t@example.org"
      ),
      {
        git("init", "-q")
        git("add", "package.json", "src", ".gitignore")
        git("commit", "-q", "-m", "fake")
        if (!is.null(tag)) git("tag", tag)
      }
    )
  })
  dir
}

# An `npm` on PATH for --ref: `npm ci` writes node_modules/webr and
# `npm run build` copies `dist` (a fake checkout's built dist/) into dist/.
local_fake_npm <- function(dist, env = parent.frame()) {
  bin <- withr::local_tempdir(.local_envir = env)
  npm <- fs::path(bin, "npm")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "case \"$1\" in",
      "  ci) mkdir -p node_modules/webr && printf '{\\n  \"name\": \"webr\",\\n  \"version\": \"0.6.0\"\\n}\\n' > node_modules/webr/package.json ;;",
      sprintf("  run) mkdir -p dist && cp %s/* dist/ ;;", shQuote(as.character(dist))),
      "  *) echo \"fake npm: unexpected arguments: $*\" >&2; exit 2 ;;",
      "esac"
    ),
    npm
  )
  Sys.chmod(npm, "0755")
  withr::local_path(bin, action = "prefix", .local_envir = env)
  bin
}

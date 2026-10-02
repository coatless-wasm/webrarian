# LICENSES/: what a generated site or mirror redistributes, under which
# license, and where its source is. bind() and collection_mirror()
# write it into every build. The standard license texts come from R's own
# copies in share/licenses, which are the texts the upstream projects ship
# (exlibris's LICENSE is checked against R's AGPL-3 by tools/vendor-exlibris.sh).

#' Text of a license R ships in share/licenses ("AGPL-3", "GPL-2", "GPL-3", ...)
#' @noRd
r_license_text <- function(name) {
  path <- file.path(R.home("share"), "licenses", name)
  if (!file.exists(path)) {
    return(sprintf("The full text is at <https://www.R-project.org/Licenses/%s>.", name))
  }
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

#' The MIT notice of the webR JavaScript client, as webR's LICENSE.md states it
#' @noRd
webr_client_mit_notice <- function() {
  c(
    "Copyright (c) 2023 webR authors",
    "",
    paste(
      "Permission is hereby granted, free of charge, to any person obtaining a copy of this software",
      "and associated documentation files (the \"Software\"), to deal in the Software without",
      "restriction, including without limitation the rights to use, copy, modify, merge, publish,",
      "distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the",
      "Software is furnished to do so, subject to the following conditions:"
    ),
    "",
    paste(
      "The above copyright notice and this permission notice shall be included in all copies or",
      "substantial portions of the Software."
    ),
    "",
    paste(
      "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING",
      "BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND",
      "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,",
      "DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
      "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."
    )
  )
}

#' The vendored viewer's provenance record (inst/viewer/PROVENANCE.json)
#' @noRd
viewer_provenance <- function() {
  jsonlite::read_json(fs::path(viewer_source_dir(), "PROVENANCE.json"))
}

#' Where a prebuilt package's source is: `<repo>/src/contrib/<pkg>_<version>.tar.gz`
#' @noRd
prebuilt_source_url <- function(contrib_url, package, version) {
  repo <- sub("/bin/emscripten/contrib/[^/]+/?$", "", contrib_url)
  sprintf("%s/src/contrib/%s_%s.tar.gz", repo, package, version)
}

#' Source links for downloaded prebuilt packages
#' @param rows Index rows (`Package`, `Version`), as download_webr_packages() returns.
#' @param source Named character: package -> contrib URL it came from.
#' @noRd
prebuilt_sources <- function(rows, source) {
  if (is.null(rows) || nrow(rows) == 0L) {
    return(character())
  }
  pkgs <- unname(rows[, "Package"])
  stats::setNames(
    prebuilt_source_url(unname(source[pkgs]), pkgs, unname(rows[, "Version"])),
    pkgs
  )
}

#' Split `owner/repo[/subdir][@ref]`
#' @return `list(owner, repo, subdir, ref)` (`ref` "HEAD" when absent), or NULL.
#' @noRd
parse_github_spec <- function(spec) {
  m <- regmatches(spec, regexec("^([^/@]+)/([^/@]+)(?:/([^@]+))?(?:@(.+))?$", spec, perl = TRUE))[[
    1
  ]]
  if (length(m) == 0L) {
    return(NULL)
  }
  list(
    owner = m[[2]],
    repo = m[[3]],
    subdir = if (nzchar(m[[4]])) m[[4]] else NULL,
    ref = if (nzchar(m[[5]])) m[[5]] else "HEAD"
  )
}

#' The GitHub page of the source a spec names
#' @noRd
github_source_url <- function(spec) {
  parts <- parse_github_spec(spec)
  if (is.null(parts)) {
    return(spec)
  }
  url <- sprintf(
    "https://github.com/%s/%s/tree/%s",
    parts$owner,
    parts$repo,
    utils::URLencode(parts$ref, reserved = TRUE)
  )
  if (!is.null(parts$subdir)) {
    url <- paste0(url, "/", parts$subdir)
  }
  url
}

#' Source links for compiled local and GitHub packages
#'
#' A local package is named by its configured directory. A GitHub package is
#' matched to the spec whose repository (or subdirectory) has its name, then
#' case-insensitively; with a single spec, that spec.
#' @noRd
compiled_sources <- function(rows, local, github, root) {
  if (is.null(rows) || nrow(rows) == 0L) {
    return(character())
  }
  names <- unname(rows[, "Package"])
  local_names <- character()
  for (p in as.character(local)) {
    src <- tryCatch(resolve_local_package_path(root, p), error = function(e) NULL)
    if (is.null(src)) {
      next
    }
    local_names[[unname(read.dcf(fs::path(src, "DESCRIPTION"))[1, "Package"])]] <- p
  }
  specs <- as.character(github)
  candidates <- vapply(
    specs,
    function(s) {
      parts <- parse_github_spec(s)
      if (is.null(parts)) {
        NA_character_
      } else {
        as.character(fs::path_file(parts$subdir %||% parts$repo))
      }
    },
    character(1),
    USE.NAMES = FALSE
  )

  out <- stats::setNames(character(length(names)), names)
  for (name in names) {
    if (name %in% names(local_names)) {
      out[[name]] <- sprintf("local package `%s` in the collection", local_names[[name]])
      next
    }
    hit <- which(candidates == name)
    if (length(hit) == 0L) {
      hit <- which(tolower(candidates) == tolower(name))
    }
    if (length(hit) == 0L && length(specs) == 1L) {
      hit <- 1L
    }
    out[[name]] <- if (length(hit) > 0L) {
      github_source_url(specs[[hit[[1]]]])
    } else {
      sprintf("compiled from GitHub: one of %s", paste0("`", specs, "`", collapse = ", "))
    }
  }
  out
}

#' The packages in a built directory's repo/, with licenses and sources
#' @noRd
site_package_table <- function(dir, r_version, sources = character()) {
  empty <- data.frame(
    package = character(),
    version = character(),
    license = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  index <- fs::path(dir, "repo", "bin", "emscripten", "contrib", r_version, "PACKAGES.rds")
  if (!fs::file_exists(index)) {
    return(empty)
  }
  rows <- readRDS(index)
  if (!is.matrix(rows) || nrow(rows) == 0L) {
    return(empty)
  }
  pkgs <- unname(rows[, "Package"])
  license <- if ("License" %in% colnames(rows)) {
    unname(rows[, "License"])
  } else {
    rep(NA_character_, length(pkgs))
  }
  license[is.na(license)] <- "not stated in the repository index"
  src <- unname(sources[pkgs])
  src[is.na(src) | !nzchar(src)] <- "not recorded"
  data.frame(
    package = pkgs,
    version = unname(rows[, "Version"]),
    license = license,
    source = src,
    stringsAsFactors = FALSE
  )
}

#' The texts of the standard licenses a set of license fields name
#'
#' Each `|`/`+` component of an R license spec picks the versions R ships
#' that the component allows, and a component that names a GPL family in a
#' form R does not standardize gets every text of that family.
#' @return A named list: license name -> lines, for those R ships.
#' @noRd
standard_license_texts <- function(licenses) {
  shipped <- list(
    GPL = c("GPL-2" = 2, "GPL-3" = 3),
    LGPL = c("LGPL-2" = 2, "LGPL-2.1" = 2.1, "LGPL-3" = 3),
    AGPL = c("AGPL-3" = 3)
  )
  spec <- "^([AL]?GPL)(?:-([0-9.]+))?\\s*(?:\\(\\s*(>=|>|<=|<|==|!=)\\s*([0-9.]+)\\s*\\))?$"
  parts <- trimws(unlist(strsplit(licenses[!is.na(licenses)], "[|+]")))
  wanted <- character()
  for (part in parts) {
    m <- regmatches(part, regexec(spec, part, perl = TRUE))[[1]]
    if (length(m) == 0L) {
      family <- regmatches(part, regexpr("[AL]?GPL", part))
      if (length(family) == 1L) {
        wanted <- c(wanted, names(shipped[[family]]))
      }
      next
    }
    versions <- shipped[[m[2]]]
    keep <- if (nzchar(m[3])) {
      versions == as.numeric(m[3])
    } else if (nzchar(m[4])) {
      match.fun(m[4])(versions, as.numeric(m[5]))
    } else {
      TRUE
    }
    wanted <- c(wanted, names(versions)[keep])
  }
  out <- list()
  for (name in intersect(unlist(lapply(shipped, names), use.names = FALSE), wanted)) {
    out[[name]] <- r_license_text(name)
  }
  out
}

md_cell <- function(x) gsub("|", "\\|", x, fixed = TRUE)

#' LICENSES/webrarian.md: who generated the output, and the output exception
#'
#' R installs no LICENSE.note, so the exception is read from the installed
#' inst/OUTPUT-EXCEPTION.md (tests/testthat/test-notices.R keeps the two
#' word for word alike).
#' @noRd
licenses_webrarian_md <- function(what) {
  exception <- readLines(
    system.file("OUTPUT-EXCEPTION.md", package = "webrarian", mustWork = TRUE),
    warn = FALSE,
    encoding = "UTF-8"
  )
  c(
    "# webrarian",
    "",
    sprintf(
      "webrarian %s generated this %s (<https://github.com/coatless-wasm/webrarian>).",
      as.character(utils::packageVersion("webrarian")),
      what
    ),
    "webrarian is licensed under the GNU Affero General Public License, version 3,",
    sprintf("with the additional permission below: the files webrarian wrote into this %s", what),
    "are yours to license as you choose. The parts listed in [index.html](index.html)",
    "keep their own licenses.",
    "",
    exception
  )
}

licenses_exlibris_md <- function(prov, bundle, what) {
  commit <- prov$exlibrisCommit
  exception <- readLines(
    fs::path(viewer_source_dir(), "EXCEPTION.md"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  c(
    "# exlibris",
    "",
    sprintf(
      "This %s runs the exlibris viewer: %s.",
      what,
      paste0("`", bundle, "`", collapse = " and ")
    ),
    sprintf(
      "They are unmodified builds of exlibris %s, commit `%s` (%s).",
      prov$exlibrisVersion,
      commit,
      prov$exlibrisRef
    ),
    "",
    sprintf("- Source code: <https://github.com/coatless-wasm/exlibris/tree/%s>", commit),
    "- License: the GNU Affero General Public License, version 3 only, with the",
    "  exlibris runtime exception below.",
    "- Third-party code inside the viewer: [THIRD-PARTY-r.md](THIRD-PARTY-r.md).",
    "",
    "The banner at the top of each bundle names exlibris's own notice files.",
    "`EXCEPTION.md` is the exlibris runtime exception, reproduced below word for",
    "word. `THIRD-PARTY-r.md` is [THIRD-PARTY-r.md](THIRD-PARTY-r.md) in this",
    "directory, after a note on the vendored build.",
    "",
    exception,
    "",
    "## GNU Affero General Public License, version 3",
    "",
    "```text",
    r_license_text("AGPL-3"),
    "```"
  )
}

licenses_webr_md <- function(engine, engine_bundled, client_version, what) {
  entry <- webr_versions_entry(engine$version)
  r_full <- if (is.null(entry)) NULL else entry$r_version
  tag <- paste0("v", engine$version)
  r_label <- if (is.null(r_full)) paste0(engine$r_version, ".x") else r_full
  lines <- c(
    "# webR",
    "",
    sprintf("This %s runs webR %s (R %s).", what, engine$version, r_label),
    ""
  )
  if (isTRUE(engine_bundled)) {
    r_source <- if (is.null(r_full)) {
      "- R: <https://cran.r-project.org/src/base/>"
    } else {
      sprintf(
        "- R %s: <https://cran.r-project.org/src/base/R-%s/R-%s.tar.gz>",
        r_full,
        sub("\\..*$", "", r_full),
        r_full
      )
    }
    lines <- c(
      lines,
      sprintf(
        "The webR engine in `%s/` is the unmodified webR %s release,",
        engine_dir_rel(engine$version),
        engine$version
      ),
      sprintf(
        "<https://github.com/r-wasm/webr/releases/tag/%s>. webR distributes these binaries,",
        tag
      ),
      "which contain R and other software compiled to WebAssembly, under the GNU General",
      "Public License, version 3 (text below).",
      "",
      "Their source:",
      "",
      sprintf(
        "- webR %s: <https://github.com/r-wasm/webr/tree/%s>. Its `LICENSE.md` lists every",
        engine$version,
        tag
      ),
      "  component compiled into the binaries and where its source is.",
      r_source
    )
  } else {
    lines <- c(
      lines,
      sprintf("The webR engine is not part of this %s: the page loads it from", what),
      sprintf("<https://webr.r-wasm.org/%s/> when it opens, under webR's own license", tag),
      sprintf("(<https://github.com/r-wasm/webr/blob/%s/LICENSE.md>).", tag)
    )
  }
  lines <- c(
    lines,
    "",
    "## The webR JavaScript client",
    "",
    sprintf(
      "The viewer (see [exlibris.md](exlibris.md)) contains the webR JavaScript client, `webr` %s,",
      client_version
    ),
    "under the MIT license:",
    "",
    "```text",
    webr_client_mit_notice(),
    "```"
  )
  if (isTRUE(engine_bundled)) {
    lines <- c(
      lines,
      "",
      "## GNU General Public License, version 3",
      "",
      "```text",
      r_license_text("GPL-3"),
      "```"
    )
  }
  lines
}

licenses_packages_md <- function(bundled, runtime, runtime_repos, what) {
  lines <- c("# R packages", "")
  if (nrow(bundled) == 0L) {
    lines <- c(lines, sprintf("This %s bundles no R packages.", what))
  } else {
    source_cell <- ifelse(
      grepl("^https?://", bundled$source),
      paste0("<", bundled$source, ">"),
      md_cell(bundled$source)
    )
    lines <- c(
      lines,
      sprintf(
        "This %s bundles %d R package%s, compiled to WebAssembly, in `repo/`.",
        what,
        nrow(bundled),
        if (nrow(bundled) == 1L) "" else "s"
      ),
      "Each keeps its own license. The source link is the source its binary was built from.",
      "",
      "| Package | Version | License | Source |",
      "|---|---|---|---|",
      sprintf(
        "| %s | %s | %s | %s |",
        bundled$package,
        bundled$version,
        md_cell(bundled$license),
        source_cell
      )
    )
  }
  if (length(runtime) > 0L) {
    lines <- c(
      lines,
      "",
      sprintf(
        "Installed from the network when the page opens, and not redistributed by this %s: %s (from %s).",
        what,
        paste(runtime, collapse = ", "),
        paste0("<", runtime_repos, ">", collapse = ", ")
      )
    )
  }
  texts <- standard_license_texts(bundled$license)
  if (length(texts) > 0L) {
    lines <- c(lines, "", "## License texts", "", "The standard licenses named above:")
    for (name in names(texts)) {
      lines <- c(lines, "", paste("###", name), "", "```text", texts[[name]], "```")
    }
  }
  lines
}

#' Remind the author when a site bundles a GPL-family local package
#'
#' PACKAGES.md can only name a local package's directory in the collection,
#' which nobody who receives the site can reach, and the GPL makes whoever
#' distributes the binary offer its source. A message, not a warning: the
#' build is fine, and an author who has published the source could never
#' silence a warning.
#' @param bundled site_package_table()'s result.
#' @return The names of those packages, invisibly.
#' @noRd
alert_gpl_local_packages <- function(bundled) {
  hits <- bundled[
    startsWith(bundled$source, "local package ") & grepl("GPL", bundled$license, fixed = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(hits) == 0L) {
    return(invisible(character()))
  }
  labels <- sprintf("%s (%s)", hits$package, hits$license)
  cli::cli_inform(
    c(
      "!" = "{.pkg {labels}} {?is a/are} GPL-licensed local package{?s} bundled into this site.",
      "i" = "Whoever publishes the site must also offer the source: {.file LICENSES/PACKAGES.md} can name only the directory in the collection, which visitors cannot reach.",
      "i" = "Publish the source, for example on GitHub, and bundle it with {.field packages.github} instead of {.field packages.local}."
    ),
    class = "webrarian_message_gpl_local"
  )
  invisible(hits$package)
}

licenses_index_html <- function(what) {
  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\" />",
    "  <title>Licenses</title>",
    "</head>",
    "<body>",
    "  <h1>Licenses</h1>",
    sprintf("  <p>This %s is made of the parts below. Each keeps its own license.</p>", what),
    "  <ul>",
    sprintf(
      "    <li><a href=\"webrarian.md\">webrarian.md</a>: the tool that generated this %s, and the permission that makes its output yours</li>",
      what
    ),
    "    <li><a href=\"exlibris.md\">exlibris.md</a>: the viewer (AGPL-3.0-only with the exlibris runtime exception, which each bundle's banner calls EXCEPTION.md)</li>",
    "    <li><a href=\"THIRD-PARTY-r.md\">THIRD-PARTY-r.md</a>: third-party code inside the viewer, the file each bundle's banner names</li>",
    "    <li><a href=\"webR.md\">webR.md</a>: the webR engine and its JavaScript client</li>",
    "    <li><a href=\"PACKAGES.md\">PACKAGES.md</a>: the R packages</li>",
    "  </ul>",
    "</body>",
    "</html>"
  )
}

#' Write LICENSES/ into a built site or mirror
#'
#' @param dir The directory being built (bind()'s staging directory).
#' @param engine resolve_webr_version()'s result.
#' @param engine_bundled Whether the webR engine is in `dir` (bind()'s
#'   `bundle`; always for a mirror), not whether the site is offline.
#' @param bundled site_package_table()'s result.
#' @param runtime,runtime_repos Packages the page installs from the network
#'   when it opens, and the repositories it installs them from.
#' @param what "site" or "mirror", for the wording.
#' @return The LICENSES/ path, invisibly.
#' @noRd
emit_licenses <- function(
  dir,
  engine,
  engine_bundled,
  bundled,
  runtime = character(),
  runtime_repos = character(),
  what = "site"
) {
  out <- fs::path(dir, "LICENSES")
  if (fs::dir_exists(out)) {
    fs::dir_delete(out)
  }
  fs::dir_create(out)
  prov <- viewer_provenance()
  bundle <- as.character(fs::path_file(fs::dir_ls(
    dir,
    regexp = "/exlibris-r\\.[0-9a-f]{8}\\.(js|css)$"
  )))
  write_md <- function(lines, name) {
    writeLines(enc2utf8(lines), fs::path(out, name), useBytes = TRUE)
  }
  write_md(licenses_webrarian_md(what), "webrarian.md")
  write_md(licenses_exlibris_md(prov, bundle, what), "exlibris.md")
  # Under the name each bundle's banner uses (decision 9).
  fs::file_copy(fs::path(viewer_source_dir(), "THIRD-PARTY.md"), fs::path(out, "THIRD-PARTY-r.md"))
  write_md(licenses_webr_md(engine, engine_bundled, prov$webrClientVersion, what), "webR.md")
  write_md(licenses_packages_md(bundled, runtime, runtime_repos, what), "PACKAGES.md")
  write_md(licenses_index_html(what), "index.html")
  invisible(out)
}

# WebR Asset/Cache Management
#
# Following the shinylive pattern for asset caching.
# See: https://github.com/posit-dev/r-shinylive/blob/main/R/assets.R

# ============================================================================
# Cache Directory Management
# ============================================================================

#' The webrarian cache directory
#'
#' `tools::R_user_dir("webrarian", "cache")`; see "Package options" in
#' `?webrarian`.
#' @noRd
webr_cache_path <- function() {
  tools::R_user_dir("webrarian", which = "cache")
}

#' Get webR assets directory for a specific version
#'
#' @param version webR version string (e.g., "0.5.8").
#'
#' @return Path to versioned assets directory.
#'
#' @noRd
webr_assets_dir <- function(version) {
  fs::path(webr_cache_path(), paste0("webr-", version))
}

#' Get packages cache directory
#'
#' @param r_version R version string (e.g., "4.5").
#'
#' @return Path to packages cache directory.
#'
#' @noRd
webr_packages_cache_dir <- function(r_version = NULL) {
  base <- fs::path(webr_cache_path(), "packages")
  if (!is.null(r_version)) {
    fs::path(base, r_version)
  } else {
    base
  }
}

# ============================================================================
# Package Download Cache
# ============================================================================
#
# `bind()` defaults to clean = TRUE, so the output directory is wiped before
# every build and the `.tgz` files it holds cannot act as a cache. These
# helpers keep a second, build-independent copy under
# webr_packages_cache_dir(r_version) so a rebuild re-downloads nothing.
#
# Integrity is non-negotiable: an entry is only ever served when its checksum
# matches, so an interrupted or truncated write can never be handed to a build
# (the same contract as pyodidarian's cacheGet()).
#
# The webR repository index (PACKAGES.rds) publishes an `MD5sum` column, so
# that upstream-declared digest is the primary check. Repositories that omit
# it (or rows where it is NA) fall back to the digest webrarian recorded when
# it stored the file, kept in an `index.json` sidecar beside the binaries.
# MD5 is used for both so the two paths share one algorithm and the package
# gains no new dependency; the sidecar records the algorithm name so a future
# change fails closed rather than silently comparing across algorithms.

#' Digest algorithm recorded in the package cache index
#' @noRd
webr_cache_digest_algo <- function() "md5"

#' Digest of a file on disk, or `NA_character_` if it cannot be read
#' @noRd
webr_file_digest <- function(path) {
  digest <- tryCatch(unname(tools::md5sum(as.character(path))), error = function(e) NA_character_)
  if (length(digest) != 1L) {
    return(NA_character_)
  }
  if (is.na(digest)) NA_character_ else tolower(digest)
}

#' Is a checksum string usable for verification?
#' @noRd
is_usable_digest <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

#' Is the package download cache enabled?
#'
#' Escape hatch for users who want every build to hit the network: set the
#' `webrarian.package_cache` option or the `WEBRARIAN_PACKAGE_CACHE`
#' environment variable to a false-y value. The option wins when both are set.
#' @noRd
webr_package_cache_enabled <- function() {
  opt <- getOption("webrarian.package_cache", NULL)
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }

  env <- trimws(Sys.getenv("WEBRARIAN_PACKAGE_CACHE", unset = ""))
  if (!nzchar(env)) {
    return(TRUE)
  }
  !tolower(env) %in% c("0", "false", "no", "off", "n")
}

#' Is this a safe cache entry name?
#'
#' File names are derived from a repository's `PACKAGES` index, which is remote
#' input. Restricting them to a plain base name keeps a hostile or malformed
#' index from writing outside the cache directory.
#' @noRd
is_safe_cache_filename <- function(filename) {
  is.character(filename) &&
    length(filename) == 1L &&
    !is.na(filename) &&
    nzchar(filename) &&
    identical(filename, as.character(fs::path_file(filename))) &&
    !filename %in% c(".", "..") &&
    !grepl("[/\\\\]", filename)
}

#' Path a cached package binary would occupy
#' @noRd
webr_package_cache_file <- function(r_version, filename) {
  fs::path(webr_packages_cache_dir(r_version), filename)
}

#' Path of the cache index sidecar for an R version
#' @noRd
webr_package_cache_index_path <- function(r_version) {
  fs::path(webr_packages_cache_dir(r_version), "index.json")
}

#' Read the cache index, tolerating a missing or damaged file
#' @noRd
webr_package_cache_index_read <- function(r_version) {
  path <- webr_package_cache_index_path(r_version)
  if (!fs::file_exists(path)) {
    return(list())
  }
  index <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  # A damaged index is treated as empty: every entry then looks unverifiable
  # and is re-downloaded, which is the safe direction to fail in.
  if (!is.list(index)) list() else index
}

#' Write the cache index atomically
#'
#' Concurrent builds can race here; the loser's entry is simply absent, which
#' costs one re-download and never yields a wrong digest.
#' @noRd
webr_package_cache_index_write <- function(r_version, index) {
  dir <- webr_packages_cache_dir(r_version)
  ensure_dir(dir)
  path <- webr_package_cache_index_path(r_version)
  tmp <- fs::path(dir, sprintf(".index.json.%s.part", Sys.getpid()))
  jsonlite::write_json(index, tmp, auto_unbox = TRUE, pretty = TRUE)
  fs::file_move(tmp, path)
  invisible(path)
}

#' Look up the digest webrarian recorded for a cached file
#' @noRd
webr_package_cache_index_get <- function(r_version, filename) {
  entry <- webr_package_cache_index_read(r_version)[[filename]]
  if (!is.list(entry)) {
    return(NULL)
  }
  # Fail closed on an algorithm this build does not know how to compute.
  if (!identical(entry$algo, webr_cache_digest_algo())) {
    return(NULL)
  }
  if (!is_usable_digest(entry$digest)) {
    return(NULL)
  }
  tolower(trimws(entry$digest))
}

#' Record the digest of a stored file
#' @noRd
webr_package_cache_index_set <- function(r_version, filename, digest) {
  index <- webr_package_cache_index_read(r_version)
  index[[filename]] <- list(
    algo = webr_cache_digest_algo(),
    digest = unname(digest)
  )
  webr_package_cache_index_write(r_version, index)
}

#' Forget the digest of a file that is no longer cached
#' @noRd
webr_package_cache_index_remove <- function(r_version, filename) {
  index <- webr_package_cache_index_read(r_version)
  if (is.null(index[[filename]])) {
    return(invisible(FALSE))
  }
  index[[filename]] <- NULL
  tryCatch(webr_package_cache_index_write(r_version, index), error = function(e) NULL)
  invisible(TRUE)
}

#' Fetch a package binary from the download cache
#'
#' Returns the cached path only when the entry exists *and* its checksum
#' matches, so a truncated or corrupt entry is never served. A mismatching
#' entry is evicted so the next build starts from a clean slate.
#'
#' @param r_version R version string (e.g. `"4.5"`).
#' @param filename Package binary file name (e.g. `"dplyr_1.1.4.tgz"`).
#' @param expected_md5 MD5 published by the upstream repository index, or
#'   `NA_character_` when the repository does not publish one. In that case the
#'   digest recorded at store time is used instead.
#'
#' @return Path to the verified cached file, or `NULL` on a miss.
#' @noRd
webr_package_cache_get <- function(r_version, filename, expected_md5 = NA_character_) {
  if (!webr_package_cache_enabled() || !is_safe_cache_filename(filename)) {
    return(NULL)
  }

  path <- webr_package_cache_file(r_version, filename)
  if (!fs::file_exists(path)) {
    return(NULL)
  }

  expected <- if (is_usable_digest(expected_md5)) {
    tolower(trimws(expected_md5))
  } else {
    webr_package_cache_index_get(r_version, filename)
  }

  # Bytes we cannot verify are bytes we do not serve.
  if (is.null(expected)) {
    return(NULL)
  }

  actual <- webr_file_digest(path)
  if (is.na(actual) || !identical(actual, expected)) {
    tryCatch(fs::file_delete(path), error = function(e) NULL)
    webr_package_cache_index_remove(r_version, filename)
    return(NULL)
  }

  path
}

#' Store a package binary in the download cache
#'
#' Writes atomically: the bytes are copied to a temporary file inside the cache
#' directory, the digest is recorded, and only then is the file renamed into
#' place. An interrupted build therefore leaves at most a `.part` file, never a
#' truncated entry that a later build would trust.
#'
#' Cache failures (unwritable directory, full disk) are reported but never
#' abort the build - the download already succeeded.
#'
#' @param r_version R version string (e.g. `"4.5"`).
#' @param filename Package binary file name.
#' @param src_file Path to the freshly downloaded file.
#'
#' @return Invisibly, the cached path, or `NULL` if it was not cached.
#' @noRd
webr_package_cache_put <- function(r_version, filename, src_file) {
  if (!webr_package_cache_enabled() || !is_safe_cache_filename(filename)) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      dir <- webr_packages_cache_dir(r_version)
      ensure_dir(dir)

      dest <- fs::path(dir, filename)
      tmp <- fs::path(dir, sprintf(".%s.%s.part", filename, Sys.getpid()))
      on.exit(
        if (fs::file_exists(tmp)) {
          tryCatch(fs::file_delete(tmp), error = function(e) NULL)
        },
        add = TRUE
      )

      fs::file_copy(src_file, tmp, overwrite = TRUE)

      digest <- webr_file_digest(tmp)
      if (is.na(digest)) {
        return(invisible(NULL))
      }

      # Record first, rename second: a crash between the two leaves an index
      # entry for a file that does not exist (harmless), never a file whose
      # digest is unknown.
      webr_package_cache_index_set(r_version, filename, digest)
      fs::file_move(tmp, dest)

      invisible(dest)
    },
    error = function(e) {
      cli::cli_alert_warning("Could not cache {.file {filename}}: {e$message}")
      invisible(NULL)
    }
  )
}

#' Summarize the package download cache
#'
#' @return A data frame with one row per cached R version.
#' @noRd
webr_package_cache_summary <- function() {
  pkg_cache <- webr_packages_cache_dir()
  empty <- data.frame(
    r_version = character(),
    packages = integer(),
    size = numeric(),
    stringsAsFactors = FALSE
  )
  if (!fs::dir_exists(pkg_cache)) {
    return(empty)
  }

  version_dirs <- fs::dir_ls(pkg_cache, type = "directory")
  if (length(version_dirs) == 0) {
    return(empty)
  }

  rows <- lapply(version_dirs, function(dir) {
    pkgs <- fs::dir_ls(dir, type = "file", glob = "*.tgz")
    data.frame(
      r_version = as.character(fs::path_file(dir)),
      packages = length(pkgs),
      size = sum(fs::file_info(pkgs)$size, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ============================================================================
# Version Management
# ============================================================================
#' The webR version to use when none is named (see webr_version_default())
#' @noRd
webr_assets_version <- function() {
  webr_version_default()
}

# ============================================================================
# Asset Installation
# ============================================================================

#' The cached webR engine for a version, downloading it if needed
#'
#' A reused engine is marked used, so webr_cache_prune() keeps the engines
#' that are still built with.
#' @return The engine directory, invisibly.
#' @noRd
webr_assets_ensure <- function(version = webr_assets_version()) {
  assets_dir <- webr_assets_dir(version)

  # Only reuse a cache that finished downloading. A non-empty but partial
  # directory (interrupted download) must be re-fetched, not served forever.
  if (cache_is_complete(assets_dir)) {
    mark_engine_used(assets_dir)
    cli::cli_alert_success("Using cached webR v{version}")
    return(invisible(assets_dir))
  }

  webr_assets_download(version)
}

#' Is a cached webR engine directory complete?
#'
#' Reuse is gated on a sentinel written only after a download's copy fully
#' succeeded. A symlink (an install_link() from an older webrarian) is never
#' complete, so the next download replaces the link instead of writing into
#' whatever it points at.
#' @noRd
cache_is_complete <- function(assets_dir) {
  !fs::link_exists(assets_dir) && fs::file_exists(fs::path(assets_dir, ".webrarian-complete"))
}

#' Record that a cached engine was just used: webr_cache_prune() ranks
#' engines by their sentinel's modification time
#' @noRd
mark_engine_used <- function(assets_dir) {
  fs::file_touch(fs::path(assets_dir, ".webrarian-complete"))
  invisible(assets_dir)
}

#' Checksums pinned in webr-versions.json for a webR release tarball
#' @return `list(sha256, md5)`, or NULL for a version with no pin.
#' @noRd
webr_release_checksums <- function(version) {
  entry <- webr_versions_entry(version)
  if (is.null(entry) || is.null(entry$tarball_sha256)) {
    return(NULL)
  }
  list(sha256 = entry$tarball_sha256, md5 = entry$tarball_md5)
}

#' Digest of a file: SHA-256 where R has tools::sha256sum() (R >= 4.5), else MD5
#' @noRd
release_digest <- function(path) {
  tools_ns <- asNamespace("tools")
  if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
    sha256sum <- get("sha256sum", envir = tools_ns)
    return(list(algo = "sha256", value = tolower(unname(sha256sum(as.character(path))))))
  }
  list(algo = "md5", value = tolower(unname(tools::md5sum(as.character(path)))))
}

#' Check a downloaded release tarball against its pinned checksum
#' @noRd
verify_webr_tarball <- function(path, checksums, version) {
  if (is.null(checksums)) {
    cli::cli_alert_info(
      "webR {version} is not a tested version and has no pinned checksum; the download is not verified."
    )
    return(invisible(TRUE))
  }
  digest <- release_digest(path)
  expected <- tolower(checksums[[digest$algo]] %||% "")
  if (!identical(digest$value, expected)) {
    cli::cli_abort(
      c(
        "The webR {version} download does not match its pinned {digest$algo} checksum.",
        "x" = "Expected {.val {expected}}, got {.val {digest$value}}.",
        "i" = "Nothing was cached. Try again; if it persists, report it."
      ),
      class = "webrarian_error_checksum"
    )
  }
  invisible(TRUE)
}

#' Download a webR release and cache its engine
#'
#' The tarball is streamed to disk, checked against the checksum pinned in
#' webr-versions.json, unpacked into a private temporary directory, and its
#' engine files are copied into a staging directory in the cache. Only then is
#' the staging directory moved into place, so an interrupted or failed
#' download never leaves a directory that is listed or reused. Afterwards,
#' the cache is pruned to the most recently used engines (webr_cache_prune()).
#' @return The engine directory, invisibly.
#' @noRd
webr_assets_download <- function(version = webr_assets_version()) {
  validate_webr_version(version)
  cache <- webr_cache_path()
  assets_dir <- webr_assets_dir(version)
  ensure_dir(cache)

  temp_tar <- tempfile(fileext = ".tar.gz")
  temp_extract <- tempfile("webrarian-webr-extract-")
  staging <- fs::path(cache, sprintf(".staging-webr-%s-%s", version, Sys.getpid()))
  on.exit(unlink(c(temp_tar, temp_extract, staging), recursive = TRUE, force = TRUE), add = TRUE)

  cli::cli_alert_info("Downloading webR v{version} release...")
  tarball_url <- sprintf(
    "https://github.com/r-wasm/webr/releases/download/v%s/webr-%s.tar.gz",
    version,
    version
  )
  tryCatch(
    {
      req <- httr2::req_timeout(httr2::request(tarball_url), 300)
      if (rlang::is_interactive()) {
        req <- httr2::req_progress(req)
      }
      # Streamed to disk rather than held in memory (the release is ~40 MB).
      resp <- httr2::req_perform(req, path = temp_tar)
      if (httr2::resp_status(resp) != 200) {
        stop(sprintf("HTTP %s", httr2::resp_status(resp)), call. = FALSE)
      }
    },
    error = function(e) {
      cli::cli_abort("Failed to download webR: {conditionMessage(e)}")
    }
  )
  cli::cli_alert_success(
    "Downloaded webR release ({format(fs::fs_bytes(fs::file_size(temp_tar)))})"
  )
  verify_webr_tarball(temp_tar, webr_release_checksums(version), version)

  fs::dir_create(temp_extract)
  utils::untar(temp_tar, exdir = temp_extract)
  extracted_dir <- fs::path(temp_extract, paste0("webr-", version))
  if (!fs::dir_exists(extracted_dir)) {
    dirs <- fs::dir_ls(temp_extract, type = "directory")
    if (length(dirs) == 0L) {
      cli::cli_abort("The webR {version} release has no top-level directory.")
    }
    extracted_dir <- dirs[[1]]
  }

  # Only the engine: the REPL UI is the vendored viewer, and the source maps
  # (webr.mjs.map, webr-worker.js.map, ~1 MB) are not needed at run time.
  engine_keep <- c("R.wasm", "R.js", "webr.mjs", "webr-worker.js", "libRblas.so", "libRlapack.so")
  if (fs::dir_exists(staging)) {
    fs::dir_delete(staging)
  }
  fs::dir_create(staging)
  copied <- 0L
  for (f in fs::dir_ls(extracted_dir, recurse = TRUE, type = "file")) {
    rel <- as.character(fs::path_rel(f, extracted_dir))
    top <- fs::path_split(rel)[[1]][1]
    if (!(rel %in% engine_keep || identical(top, "vfs"))) {
      next
    }
    dest <- fs::path(staging, rel)
    ensure_dir(fs::path_dir(dest))
    fs::file_copy(f, dest, overwrite = TRUE)
    copied <- copied + 1L
  }
  writeLines(as.character(version), fs::path(staging, ".webrarian-complete"))

  # Swap into place. A symlink from an older webrarian's install_link() is
  # removed as a link: whatever it points at is never written into.
  if (fs::link_exists(assets_dir)) {
    fs::link_delete(assets_dir)
  } else if (fs::dir_exists(assets_dir)) {
    fs::dir_delete(assets_dir)
  }
  fs::file_move(staging, assets_dir)

  cli::cli_alert_success("Cached webR engine v{version} ({copied} files)")
  webr_cache_prune(keep = version)
  invisible(assets_dir)
}

# ============================================================================
# Asset Listing and Cleanup
# ============================================================================

#' Engine versions complete in the cache, newest first
#'
#' Directory names are matched on their own (a cache path that happens to
#' contain "webr-" matches nothing), and only engines whose download finished
#' are listed.
#' @noRd
webr_assets_list <- function() {
  cache <- webr_cache_path()
  if (!fs::dir_exists(cache)) {
    return(character())
  }
  dirs <- fs::dir_ls(cache, type = "directory")
  dirs <- dirs[grepl("^webr-[0-9]", fs::path_file(dirs))]
  dirs <- dirs[vapply(dirs, cache_is_complete, logical(1))]
  if (length(dirs) == 0L) {
    return(character())
  }
  versions <- sub("^webr-", "", fs::path_file(dirs))
  tryCatch(
    as.character(sort(package_version(versions), decreasing = TRUE)),
    error = function(e) sort(versions, decreasing = TRUE)
  )
}

#' Remove engine versions from the cache
#' @return The versions removed, invisibly.
#' @noRd
webr_assets_remove <- function(versions) {
  if (is.null(versions) || length(versions) == 0) {
    return(invisible(character()))
  }

  removed <- character()

  for (version in versions) {
    validate_webr_version(version)
    assets_dir <- webr_assets_dir(version)

    if (fs::dir_exists(assets_dir) || fs::link_exists(assets_dir)) {
      # Get size before deletion for reporting
      if (fs::dir_exists(assets_dir) && !fs::link_exists(assets_dir)) {
        size <- sum(
          fs::file_info(
            fs::dir_ls(assets_dir, recurse = TRUE, type = "file")
          )$size,
          na.rm = TRUE
        )
        fs::dir_delete(assets_dir)
        cli::cli_alert_success(
          "Removed webR v{version} ({format(fs::fs_bytes(size))})"
        )
      } else {
        fs::file_delete(assets_dir)
        cli::cli_alert_success("Removed webR v{version} (symlink)")
      }
      removed <- c(removed, version)
    } else {
      cli::cli_alert_warning("webR v{version} not found in cache")
    }
  }

  invisible(removed)
}

#' The R line a webR engine installs packages for, or NA
#'
#' A listed version's own; for an unlisted patch release, that of a listed
#' version on the same webR line; NA for anything else (a version an older
#' webrarian offered).
#' @noRd
engine_r_line <- function(version) {
  line <- sub("\\.[0-9]+$", "", version)
  for (entry in webr_versions_table()$versions) {
    if (identical(sub("\\.[0-9]+$", "", entry$version), line)) {
      return(version_line(entry$r_version))
    }
  }
  NA_character_
}

#' Remove what the cache no longer needs
#'
#' CRAN asks that cache contents be actively managed. `keep` and the most
#' recently used complete engines, `n_recent` in all, stay (an engine's use
#' is its sentinel's modification time: a download writes it and
#' webr_assets_ensure() touches it); every other engine directory goes, and
#' so does one whose download never finished. A symlink (an older
#' webrarian's install_link()) is never followed. Package caches
#' (`packages/<R line>/`) go when no kept engine and no engine this
#' webrarian offers uses their line; when a kept engine's line is unknown, none
#' goes. Staging directories older than a day (an interrupted download's) go
#' too.
#' @return The complete engine versions removed, invisibly.
#' @noRd
webr_cache_prune <- function(keep = character(), n_recent = 3L) {
  cache <- webr_cache_path()
  if (!fs::dir_exists(cache)) {
    return(invisible(character()))
  }
  engines <- fs::dir_ls(cache, type = "directory")
  engines <- engines[grepl("^webr-[0-9]", fs::path_file(engines))]
  engines <- engines[!vapply(engines, fs::link_exists, logical(1))]
  complete <- engines[vapply(engines, cache_is_complete, logical(1))]
  versions <- sub("^webr-", "", fs::path_file(complete))
  last_used <- fs::file_info(fs::path(complete, ".webrarian-complete"))$modification_time
  by_use <- versions[order(last_used, decreasing = TRUE)]
  pinned <- intersect(keep, versions)
  kept <- utils::head(unique(c(pinned, by_use)), max(n_recent, length(pinned)))

  removed <- character()
  for (dir in engines) {
    version <- sub("^webr-", "", fs::path_file(dir))
    if (version %in% kept) {
      next
    }
    fs::dir_delete(dir)
    if (version %in% versions) removed <- c(removed, version)
  }
  if (length(removed) > 0L) {
    cli::cli_alert_info(
      "Removed webR engine{?s} {.val {removed}} from the cache: only the most recently used engines are kept"
    )
  }

  kept_lines <- vapply(kept, engine_r_line, character(1))
  offered_lines <- unique(vapply(
    webr_versions_table()$versions,
    function(v) version_line(v$r_version),
    character(1)
  ))
  packages <- webr_packages_cache_dir()
  if (!anyNA(kept_lines) && fs::dir_exists(packages)) {
    for (dir in fs::dir_ls(packages, type = "directory")) {
      line <- fs::path_file(dir)
      if (!grepl("^[0-9]+\\.[0-9]+$", line) || line %in% c(offered_lines, kept_lines)) {
        next
      }
      fs::dir_delete(dir)
      cli::cli_alert_info(
        "Removed cached packages for R {line} from the cache (no cached or offered webR engine uses them)"
      )
    }
  }

  staging <- fs::dir_ls(cache, all = TRUE, type = "directory", regexp = "/\\.staging-webr-[^/]+$")
  if (length(staging) > 0L) {
    age <- as.numeric(difftime(
      Sys.time(),
      fs::file_info(staging)$modification_time,
      units = "hours"
    ))
    for (dir in staging[age > 24]) {
      fs::dir_delete(dir)
    }
  }
  invisible(removed)
}

#' Ask a yes/no question at the console (a function so tests can answer it)
#' @return `TRUE` for "y" or "yes", in any case.
#' @noRd
ask_yes_no <- function(question) {
  tolower(trimws(readline(paste0(question, " (yes/no): ")))) %in% c("y", "yes")
}

#' Clear the webrarian cache
#'
#' Removes cached webR engines and, with `versions = NULL`, everything else
#' webrarian keeps in its cache, such as downloaded WebAssembly packages and
#' package indexes. The next build that bundles the engine downloads the ~40 MB
#' engine again, so in an interactive session clearing the whole cache asks
#' first. See "Package options" in `?webrarian` for where the cache is.
#'
#' @param versions `NULL` (the default) to clear the whole cache, or a
#'   character vector of webR engine versions to remove (without asking).
#'
#' @return Invisibly, the paths removed. This is `character()` when nothing
#'   was, for example when you answer "no" or none of `versions` was cached.
#'
#' @export
#'
#' @examples
#' # Use a throwaway cache for this example
#' old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
#' Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "webrarian-cache-demo"))
#'
#' webr_cache_info()
#' webr_cache_clear()
#'
#' if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old)
webr_cache_clear <- function(versions = NULL) {
  cache <- webr_cache_path()
  if (!is.null(versions)) {
    if (!is.character(versions) || anyNA(versions)) {
      cli::cli_abort("{.arg versions} must be NULL or a character vector of webR versions.")
    }
    for (version in versions) {
      validate_webr_version(version)
    }
    removed <- webr_assets_remove(versions)
    # paste0("webr-", character()) is "webr-", not character(): guard it.
    if (length(removed) == 0L) {
      return(invisible(character()))
    }
    return(invisible(as.character(fs::path(cache, paste0("webr-", removed)))))
  }
  if (!fs::dir_exists(cache)) {
    cli::cli_alert_info("The cache is empty")
    return(invisible(character()))
  }
  files <- fs::dir_ls(cache, recurse = TRUE, type = "file", all = TRUE)
  total <- sum(fs::file_info(files)$size, na.rm = TRUE)
  if (rlang::is_interactive()) {
    cli::cli_alert_warning(
      "This deletes {format(fs::fs_bytes(total))} of cached webR engines and packages in {.path {cache}}."
    )
    if (!ask_yes_no("Clear the webrarian cache?")) {
      cli::cli_alert_info("Canceled; nothing was removed")
      return(invisible(character()))
    }
  }
  fs::dir_delete(cache)
  cli::cli_alert_success("Cleared the webrarian cache ({format(fs::fs_bytes(total))} freed)")
  invisible(as.character(cache))
}

#' Show cache information
#'
#' Displays information about the webrarian cache, including installed
#' webR versions, cached packages, and total size.
#'
#' @return Invisibly returns a list with the cache `path`,
#'   whether it `exists`, the installed webR `versions`, a `packages` data
#'   frame with one row per R version in the package download cache
#'   (`r_version`, `packages`, `size`), and the `total_size` in bytes.
#'
#' @export
#'
#' @examples
#' webr_cache_info()
webr_cache_info <- function() {
  cache_path <- webr_cache_path()

  cli::cli_h2("Webrarian Cache")
  cli::cli_text("Location: {.path {cache_path}}")
  cli::cli_text("")

  if (!fs::dir_exists(cache_path)) {
    cli::cli_alert_info("Cache directory does not exist")
    return(invisible(list(
      path = cache_path,
      exists = FALSE,
      versions = character(),
      packages = webr_package_cache_summary(),
      total_size = 0
    )))
  }

  # webR versions
  versions <- webr_assets_list()
  cli::cli_h3("WebR Versions")
  if (length(versions) > 0) {
    for (v in versions) {
      assets_dir <- webr_assets_dir(v)
      if (fs::link_exists(assets_dir)) {
        cli::cli_bullets(c("*" = "v{v} (symlink)"))
      } else {
        size <- sum(
          fs::file_info(
            fs::dir_ls(assets_dir, recurse = TRUE, type = "file")
          )$size,
          na.rm = TRUE
        )
        cli::cli_bullets(c("*" = "v{v} ({format(fs::fs_bytes(size))})"))
      }
    }
  } else {
    cli::cli_alert_info("No webR versions installed")
  }

  # Packages cache: populated by bind() via webr_package_cache_put()
  packages <- webr_package_cache_summary()
  cli::cli_text("")
  cli::cli_h3("Package Cache")
  if (nrow(packages) > 0) {
    for (i in seq_len(nrow(packages))) {
      rv <- packages$r_version[i]
      n <- packages$packages[i]
      size <- packages$size[i]
      cli::cli_bullets(c(
        "*" = "R {rv}: {n} package{?s} ({format(fs::fs_bytes(size))})"
      ))
    }
  } else {
    cli::cli_alert_info("No packages cached")
  }

  # Total size
  cli::cli_text("")
  files <- fs::dir_ls(cache_path, recurse = TRUE, type = "file")
  total_size <- sum(fs::file_info(files)$size, na.rm = TRUE)
  cli::cli_text("Total size: {.val {format(fs::fs_bytes(total_size))}}")

  invisible(list(
    path = cache_path,
    exists = TRUE,
    versions = versions,
    packages = packages,
    total_size = total_size
  ))
}

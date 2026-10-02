# Package repositories
#
# Everything that reads a WebAssembly package repository or writes the local
# one, shared by bind() (R/build.R), collection_mirror() (R/mirror.R) and
# check_inventory() (R/diagnostics.R):
#
#   fetch_repo_index()       the merged index of several repositories (the
#                            first repository with a package wins); empty and
#                            unreachable repositories are reported
#   get_package_filename()   <pkg>_<version>.tgz, refusing anything that is not
#                            a plain package name and version
#   fetch_package_binary()   one verified package: already in place, from the
#                            download cache, or downloaded and checked
#   download_webr_packages() the above for a package set and its dependencies
#   write_repo_index()       PACKAGES, PACKAGES.gz and PACKAGES.rds

#' Build an httr2 request with a timeout and transient-error retry policy
#' @noRd
webr_request <- function(url, timeout = 60, max_tries = 3) {
  req <- httr2::request(url)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_retry(req, max_tries = max_tries)
  req
}

#' Row-bind two PACKAGES matrices whose column sets may differ
#'
#' Repos advertise different fields (Depends/Suggests/License/...). This
#' unions the columns, filling missing cells with NA, so no packages are lost.
#' @noRd
align_and_rbind <- function(a, b) {
  cols <- union(colnames(a), colnames(b))
  fill <- function(m) {
    add <- setdiff(cols, colnames(m))
    if (length(add) > 0) {
      extra <- matrix(
        NA_character_,
        nrow = nrow(m),
        ncol = length(add),
        dimnames = list(rownames(m), add)
      )
      m <- cbind(m, extra)
    }
    m[, cols, drop = FALSE]
  }
  rbind(fill(a), fill(b))
}

#' The contrib URL of a repository for an R line
#' @noRd
repo_contrib_url <- function(repo, r_version) {
  sprintf("%s/bin/emscripten/contrib/%s", sub("/+$", "", repo), r_version)
}

#' Download and read an .rds file
#' @noRd
read_rds_url <- function(url) {
  resp <- httr2::req_perform(webr_request(url))
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(httr2::resp_body_raw(resp), tmp)
  readRDS(tmp)
}

#' The merged package index of several repositories
#'
#' @return `list(packages, source, empty, failed)`: the merged matrix (NULL
#'   when no repository answered with packages), the contrib URL each package
#'   comes from, and the repositories that were empty or unreachable.
#' @noRd
fetch_repo_index <- function(repos, r_version, quiet = FALSE) {
  packages <- NULL
  source <- character()
  empty <- character()
  failed <- character()
  for (repo in unique(as.character(repos))) {
    contrib <- repo_contrib_url(repo, r_version)
    index <- tryCatch(read_rds_url(paste0(contrib, "/PACKAGES.rds")), error = function(e) e)
    if (inherits(index, "error")) {
      failed <- c(failed, repo)
      if (!quiet) {
        cli::cli_alert_warning(
          "Could not read the package index at {.url {contrib}}: {conditionMessage(index)}"
        )
      }
      next
    }
    if (!is.matrix(index) || nrow(index) == 0L || !"Package" %in% colnames(index)) {
      # r-universe answers 200 with an empty index for R lines it does not build.
      empty <- c(empty, repo)
      if (!quiet) {
        cli::cli_alert_warning("{.url {repo}} has no WebAssembly packages for R {r_version}.")
      }
      next
    }
    rownames(index) <- index[, "Package"]
    new <- index[!index[, "Package"] %in% names(source), , drop = FALSE]
    if (nrow(new) == 0L) {
      next
    }
    source <- c(source, stats::setNames(rep(contrib, nrow(new)), new[, "Package"]))
    packages <- if (is.null(packages)) new else align_and_rbind(packages, new)
  }
  list(packages = packages, source = source, empty = empty, failed = failed)
}

#' Is this a valid R package name?
#' @noRd
is_valid_package_name <- function(x) {
  is.character(x) & !is.na(x) & grepl("^([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]|[A-Za-z])$", x)
}

#' Get package filename from available packages
#'
#' Name and version come from a remote index and become a file name in the
#' site, so only a plain package name and a plain version are accepted.
#' @noRd
get_package_filename <- function(pkg, available_pkgs) {
  if (is.null(available_pkgs) || !is.matrix(available_pkgs)) {
    return(sprintf("%s.tgz", pkg))
  }
  idx <- which(available_pkgs[, "Package"] == pkg)
  if (length(idx) == 0) {
    return(NULL)
  }
  version <- available_pkgs[idx[[1]], "Version"]
  if (!is_valid_package_name(pkg) || is.na(version) || !grepl("^[0-9]+([.-][0-9]+)*$", version)) {
    return(NULL)
  }
  sprintf("%s_%s.tgz", pkg, version)
}

#' Get the MD5 a repository publishes for a package binary
#'
#' `repo.r-wasm.org` ships an `MD5sum` column in `PACKAGES.rds`. Repos that
#' omit it (or rows where it is `NA`) yield `NA_character_`, and the caller
#' falls back to the digest recorded when the file was cached.
#' @noRd
get_package_md5 <- function(pkg, available_pkgs) {
  if (is.null(available_pkgs) || !is.matrix(available_pkgs)) {
    return(NA_character_)
  }
  if (!"MD5sum" %in% colnames(available_pkgs)) {
    return(NA_character_)
  }
  idx <- which(available_pkgs[, "Package"] == pkg)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  md5 <- available_pkgs[idx[1], "MD5sum"]
  if (!is_usable_digest(md5)) {
    return(NA_character_)
  }
  tolower(trimws(md5))
}

#' Parse dependency string
#' @noRd
parse_deps <- function(dep_strs) {
  deps <- character()
  for (s in dep_strs) {
    if (is.na(s) || s == "") {
      next
    }
    parts <- strsplit(s, ",")[[1]]
    for (p in parts) {
      pkg_name <- trimws(sub("\\s*\\(.*\\).*$", "", trimws(p)))
      if (nchar(pkg_name) > 0) {
        deps <- c(deps, pkg_name)
      }
    }
  }
  unique(deps)
}

#' Resolve package dependencies
#'
#' @return The packages plus their recursive Depends/Imports/LinkingTo that
#'   are in the index, with attribute "unavailable": the non-base
#'   dependencies no repository has.
#' @noRd
resolve_package_deps <- function(packages, available_pkgs) {
  if (is.null(available_pkgs) || !is.matrix(available_pkgs)) {
    return(packages)
  }
  base <- c(base_package_names(), "R")
  known <- available_pkgs[, "Package"]
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(available_pkgs))

  all_deps <- character()
  unavailable <- character()
  to_process <- packages
  while (length(to_process) > 0) {
    pkg <- to_process[[1]]
    to_process <- to_process[-1]
    if (pkg %in% all_deps) {
      next
    }
    all_deps <- c(all_deps, pkg)

    idx <- which(known == pkg)
    if (length(idx) == 0) {
      next
    }
    deps <- setdiff(parse_deps(available_pkgs[idx[[1]], fields]), base)
    unavailable <- union(unavailable, setdiff(deps, known))
    to_process <- c(to_process, setdiff(intersect(deps, known), all_deps))
  }
  structure(unique(all_deps), unavailable = unavailable)
}

#' Obtain one package binary, verified
#'
#' In order: the file already in `dest_dir` (only if its MD5 matches, or the
#' repository publishes none), the download cache, a download checked against
#' the repository's MD5.
#' @return `list(status, file, reason)`; status is "present", "cache",
#'   "downloaded" or "failed".
#' @noRd
fetch_package_binary <- function(
  pkg,
  filename,
  dest_dir,
  url,
  r_version,
  expected_md5 = NA_character_,
  reuse_existing = TRUE
) {
  dest <- fs::path(dest_dir, filename)
  expected <- if (is_usable_digest(expected_md5)) tolower(trimws(expected_md5)) else NA_character_

  if (reuse_existing && fs::file_exists(dest)) {
    if (is.na(expected) || identical(webr_file_digest(dest), expected)) {
      return(list(status = "present", file = dest, reason = NULL))
    }
  }

  cached <- webr_package_cache_get(r_version, filename, expected_md5)
  if (!is.null(cached)) {
    copied <- tryCatch(
      {
        fs::file_copy(cached, dest, overwrite = TRUE)
        TRUE
      },
      error = function(e) FALSE
    )
    if (copied) {
      return(list(status = "cache", file = dest, reason = NULL))
    }
  }

  tmp <- tempfile(fileext = ".tgz")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    {
      resp <- httr2::req_perform(webr_request(url))
      writeBin(httr2::resp_body_raw(resp), tmp)
      if (!is.na(expected)) {
        actual <- webr_file_digest(tmp)
        if (!identical(actual, expected)) {
          stop(sprintf("checksum mismatch (expected %s, got %s)", expected, actual), call. = FALSE)
        }
      }
      fs::file_copy(tmp, dest, overwrite = TRUE)
      webr_package_cache_put(r_version, filename, tmp)
      list(status = "downloaded", file = dest, reason = NULL)
    },
    error = function(e) {
      list(status = "failed", file = NA_character_, reason = conditionMessage(e))
    }
  )
}

#' Download webR packages and dependencies into a local repo
#'
#' @param packages Packages to obtain.
#' @param exclude Packages to leave out (for example ones compiled locally).
#' @param index A `fetch_repo_index()` result to reuse.
#' @param required The packages that must be obtained or this aborts (by
#'   default all of `packages`; a full mirror passes none). Anything else that
#'   fails is a warning.
#' @return Invisibly, `list(packages, rows, failed, status, source)`; `source`
#'   maps each obtained package to the contrib URL it came from.
#' @noRd
download_webr_packages <- function(
  packages,
  pkg_dir,
  r_version,
  repos,
  include_deps = TRUE,
  exclude = character(),
  reuse_existing = TRUE,
  progress = TRUE,
  index = NULL,
  required = packages
) {
  ensure_dir(pkg_dir)
  cli::cli_alert_info("Fetching package index...")
  index <- index %||% fetch_repo_index(repos, r_version)
  available <- index$packages
  if (is.null(available)) {
    cli::cli_abort(c(
      "Could not fetch the package index from any repository.",
      "i" = "Check your network connection and {.field packages.repos} in {.file _webrarian.yml}."
    ))
  }

  packages <- unique(as.character(packages))
  required <- intersect(as.character(required), packages)
  unknown <- setdiff(required, available[, "Package"])
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Could not obtain requested package{?s}: {.pkg {unknown}}.",
      "i" = "Not in the index of {.url {repos}} for R {r_version}."
    ))
  }

  all_packages <- packages
  if (include_deps) {
    cli::cli_alert_info("Resolving dependencies...")
    all_packages <- resolve_package_deps(packages, available)
    unavailable <- setdiff(attr(all_packages, "unavailable"), exclude)
    if (length(unavailable) > 0L) {
      cli::cli_warn(c(
        "{length(unavailable)} dependenc{?y is/ies are} not available for webR and will be missing: {.pkg {unavailable}}.",
        "i" = "{cli::qty(length(unavailable))}Packages that import {?it/them} may fail to load in the browser."
      ))
    }
  }
  all_packages <- setdiff(as.character(all_packages), exclude)

  results <- list()
  show_bar <- isTRUE(progress) && length(all_packages) > 0L
  if (show_bar) {
    cli::cli_progress_bar("Fetching packages", total = length(all_packages))
  }
  for (pkg in all_packages) {
    filename <- get_package_filename(pkg, available)
    results[[pkg]] <- if (is.null(filename)) {
      list(
        status = "failed",
        file = NA_character_,
        reason = "not a valid package file in the index"
      )
    } else {
      fetch_package_binary(
        pkg,
        filename,
        pkg_dir,
        url = paste0(index$source[[pkg]], "/", filename),
        r_version = r_version,
        expected_md5 = get_package_md5(pkg, available),
        reuse_existing = reuse_existing
      )
    }
    if (show_bar) {
      cli::cli_progress_update()
    }
  }
  if (show_bar) {
    cli::cli_progress_done()
  }

  status <- vapply(results, function(r) r$status, character(1))
  failed <- names(status)[status == "failed"]
  reasons <- vapply(results[failed], function(r) r$reason %||% "", character(1))
  requested_failed <- intersect(failed, required)
  if (length(requested_failed) > 0L) {
    why <- paste(requested_failed, reasons[requested_failed], sep = ": ", collapse = "; ")
    cli::cli_abort(c(
      "Could not obtain requested package{?s}: {.pkg {requested_failed}}.",
      "x" = "{why}"
    ))
  }
  if (length(failed) > 0L) {
    cli::cli_warn("Could not download {length(failed)} package{?s}: {.pkg {failed}}.")
  }

  present <- names(status)[status != "failed"]
  n_down <- sum(status == "downloaded")
  n_cache <- sum(status == "cache")
  n_present <- sum(status == "present")
  cli::cli_alert_success(
    "{length(present)} package{?s}: {n_down} downloaded, {n_cache} from the cache, {n_present} already present"
  )

  rows <- available[match(present, available[, "Package"]), , drop = FALSE]
  if (nrow(rows) > 0L) {
    write_repo_index(pkg_dir, rows)
  }
  invisible(list(
    packages = present,
    rows = rows,
    failed = failed,
    status = status,
    source = index$source[present]
  ))
}

#' Write the local repository index for the given rows
#'
#' write.dcf() indents continuation lines, so multi-line fields stay valid
#' DCF; PACKAGES.gz is written for clients that prefer it, and PACKAGES.rds is
#' what webR reads.
#' @noRd
write_repo_index <- function(pkg_dir, rows) {
  rows <- rows[order(rows[, "Package"]), , drop = FALSE]
  rownames(rows) <- rows[, "Package"]
  saveRDS(rows, fs::path(pkg_dir, "PACKAGES.rds"))

  fields <- intersect(
    c(
      "Package",
      "Version",
      "Depends",
      "Imports",
      "LinkingTo",
      "Suggests",
      "License",
      "MD5sum",
      "NeedsCompilation"
    ),
    colnames(rows)
  )
  text <- rows[, fields, drop = FALSE]
  write.dcf(text, fs::path(pkg_dir, "PACKAGES"))
  con <- gzfile(fs::path(pkg_dir, "PACKAGES.gz"), open = "w")
  on.exit(close(con), add = TRUE)
  write.dcf(text, con)
  invisible(pkg_dir)
}

#' DESCRIPTION fields of a built package tarball
#' @noRd
read_tgz_description <- function(tgz) {
  listing <- utils::untar(tgz, list = TRUE)
  target <- listing[grepl("^[^/]+/DESCRIPTION$", listing)][1]
  if (is.na(target)) {
    cli::cli_abort("{.file {fs::path_file(tgz)}} has no DESCRIPTION.")
  }
  exdir <- tempfile("webrarian-desc-")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::untar(tgz, files = target, exdir = exdir)
  read.dcf(fs::path(exdir, target))[1, ]
}

#' Index rows for built package tarballs, read from their DESCRIPTION
#' @noRd
index_rows_from_tgz <- function(files) {
  fields <- c(
    "Package",
    "Version",
    "Depends",
    "Imports",
    "LinkingTo",
    "Suggests",
    "License",
    "MD5sum"
  )
  rows <- lapply(files, function(tgz) {
    desc <- read_tgz_description(tgz)
    row <- stats::setNames(rep(NA_character_, length(fields)), fields)
    keep <- intersect(names(desc), fields)
    row[keep] <- desc[keep]
    row[["MD5sum"]] <- webr_file_digest(tgz)
    row
  })
  m <- do.call(rbind, rows)
  rownames(m) <- m[, "Package"]
  m
}

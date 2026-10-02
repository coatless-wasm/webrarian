# Version drift: repo.r-wasm.org lags CRAN for some packages, so a
# site can bundle an older version than the author tested with. check_inventory()
# and bind() compare each WebAssembly version with CRAN and with the version
# installed in the author's own R.

#' The CRAN repository to compare with, or NULL to skip the comparison
#'
#' The `webrarian.cran_repo` option wins: a URL, or FALSE for no request at all.
#' Otherwise the CRAN mirror in `getOption("repos")`, else cloud.r-project.org.
#' @noRd
drift_cran_repo <- function() {
  opt <- getOption("webrarian.cran_repo")
  if (isFALSE(opt)) {
    return(NULL)
  }
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    return(opt)
  }
  repos <- getOption("repos")
  cran <- if ("CRAN" %in% names(repos)) unname(repos[["CRAN"]]) else NA_character_
  if (is.na(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
    "https://cloud.r-project.org"
  } else {
    cran
  }
}

#' Every package's current source version in a CRAN-like repository, read at most once a day
#'
#' Kept in the user cache directory (`tools::R_user_dir("webrarian", "cache")`,
#' under `cran-index/`), one file per repository holding the day it was read,
#' so every build and every `check_inventory()` on the same day reuses it. A
#' failed read is kept for the day as well, so a machine that cannot reach the
#' repository waits for the timeout once a day, not on every build. A
#' repository that cannot be reached is not an error: nothing is printed.
#'
#' @param today The day the cached copy must be from.
#' @return A character vector of versions named by package, or NULL when the
#'   repository cannot be read.
#' @noRd
cran_index_versions <- function(repo, today = Sys.Date()) {
  file <- fs::path(
    tools::R_user_dir("webrarian", which = "cache"),
    "cran-index",
    paste0(substr(rlang::hash(repo), 1, 16), ".rds")
  )
  day <- format(today, "%Y-%m-%d")
  cached <- if (fs::file_exists(file)) tryCatch(readRDS(file), error = function(e) NULL)
  if (is.list(cached) && identical(cached$day, day)) {
    if (isTRUE(cached$failed)) {
      return(NULL)
    }
    if (is.character(cached$versions)) {
      return(cached$versions)
    }
  }
  # Best effort: a cache directory that cannot be written costs a re-read, never the build.
  remember <- function(record) {
    tryCatch(
      {
        fs::dir_create(fs::path_dir(file))
        saveRDS(record, file)
      },
      error = function(e) NULL
    )
  }
  index <- read_repo_index(repo)
  if (is.null(index) || nrow(index) == 0L) {
    remember(list(day = day, versions = NULL, failed = TRUE))
    return(NULL)
  }
  versions <- stats::setNames(unname(index[, "Version"]), index[, "Package"])
  remember(list(day = day, versions = versions, failed = FALSE))
  versions
}

#' A CRAN-like repository's index, or NULL when it cannot be read
#'
#' The only network request of the drift report, with a 20-second limit
#' (`available.packages()` may try PACKAGES.rds, PACKAGES.gz and PACKAGES).
#' @noRd
read_repo_index <- function(repo) {
  old <- options(timeout = 20)
  on.exit(options(old), add = TRUE)
  tryCatch(
    suppressWarnings(utils::available.packages(
      repos = repo,
      type = "source",
      filters = "duplicates"
    )),
    error = function(e) NULL
  )
}

#' Current source versions on CRAN
#'
#' @return A character vector named by package (NA for a package CRAN lacks),
#'   or NULL when the comparison is skipped or CRAN cannot be reached.
#' @noRd
cran_package_versions <- function(packages, repo = drift_cran_repo()) {
  packages <- as.character(packages)
  if (is.null(repo) || length(packages) == 0L) {
    return(NULL)
  }
  versions <- cran_index_versions(repo)
  if (is.null(versions)) {
    return(NULL)
  }
  stats::setNames(unname(versions[packages]), packages)
}

#' Versions installed in this R session's libraries (NA when not installed)
#' @noRd
local_package_versions <- function(packages) {
  packages <- as.character(packages)
  stats::setNames(
    vapply(
      packages,
      function(p) {
        tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
      },
      character(1),
      USE.NAMES = FALSE
    ),
    packages
  )
}

#' "behind", "current" or "ahead": the WebAssembly version against another
#' @noRd
version_drift <- function(wasm, other) {
  vapply(
    seq_along(wasm),
    function(i) {
      if (is.na(wasm[[i]]) || is.na(other[[i]])) {
        return(NA_character_)
      }
      switch(
        as.character(utils::compareVersion(wasm[[i]], other[[i]])),
        "-1" = "behind",
        "0" = "current",
        "1" = "ahead"
      )
    },
    character(1)
  )
}

#' The drift table for packages at their WebAssembly `versions`
#' @noRd
package_drift <- function(packages, versions, cran = cran_package_versions(packages)) {
  packages <- as.character(packages)
  versions <- as.character(versions)
  cran_version <- if (is.null(cran)) {
    rep(NA_character_, length(packages))
  } else {
    unname(cran[packages])
  }
  data.frame(
    package = packages,
    version = versions,
    cran_version = cran_version,
    local_version = unname(local_package_versions(packages)),
    drift = version_drift(versions, cran_version),
    stringsAsFactors = FALSE
  )
}

#' Name the bundled packages that lag CRAN, and write packages.json
#'
#' @param bundled `site_package_table()`'s data frame (package, version, license, source).
#' @param output_dir The site being built.
#' @return The drift table, invisibly; NULL when nothing is bundled.
#' @noRd
report_package_drift <- function(bundled, output_dir) {
  if (is.null(bundled) || nrow(bundled) == 0L) {
    return(invisible(NULL))
  }
  drift <- package_drift(bundled$package, bundled$version)
  manifest <- lapply(seq_len(nrow(drift)), function(i) {
    list(
      package = drift$package[[i]],
      version = drift$version[[i]],
      `cran-version` = drift$cran_version[[i]],
      # No local-version: packages.json is published with the site, and the versions installed
      # in the author's own R describe their machine, not the site. The console line below has them.
      drift = drift$drift[[i]],
      source = bundled$source[[i]]
    )
  })
  jsonlite::write_json(
    list(
      `generated-by` = paste("webrarian", utils::packageVersion("webrarian")),
      packages = manifest
    ),
    fs::path(output_dir, "packages.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null"
  )
  behind <- drift[drift$drift %in% "behind", , drop = FALSE]
  if (nrow(behind) > 0L) {
    installed <- ifelse(
      is.na(behind$local_version),
      "",
      paste0(", installed here ", behind$local_version)
    )
    details <- sprintf(
      "%s %s (CRAN %s%s)",
      behind$package,
      behind$version,
      behind$cran_version,
      installed
    )
    cli::cli_alert_info(
      "{nrow(behind)} bundled package{?s} {?is/are} older in the WebAssembly repository than on CRAN: {details}."
    )
  }
  invisible(drift)
}

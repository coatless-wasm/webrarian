# Package management

#' Acquire R packages for the collection
#'
#' Adds packages to `_webrarian.yml`. Prebuilt packages are WebAssembly
#' binaries from repo.r-wasm.org (and `packages.repos`). [bind()] copies them
#' into the site (the default `build.bundle-engine`, and always for an offline
#' site) or has the page install them when it opens. GitHub and
#' local packages are compiled to WebAssembly by [bind()], in Docker. Whether
#' dependencies are bundled too is the collection-wide `packages.dependencies`
#' setting (see [settings_set()]), which this function never changes.
#' A prebuilt name that no repository has (repo.r-wasm.org, then
#' `packages.repos`) is added with a warning. Without a network connection
#' the names are not looked up.
#'
#' @param packages Character vector of package names for `"prebuilt"`,
#'   `owner/repo[/subdir][@ref]` for `"github"`, or directories holding a
#'   package for `"local"`.
#' @param path The collection, or any directory inside it.
#' @param source Where the packages come from, one of `"prebuilt"` (the
#'   default), `"local"` or `"github"`.
#'
#' @return Invisibly returns the updated configuration.
#'
#' @export
#'
#' @examples
#' # Prebuilt names are looked up in the package repositories, and nothing is
#' # said without a network connection. The index is cached under tempdir()
#' # here.
#' old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
#' Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "webrarian-example-cache"))
#' collection <- file.path(tempdir(), "webrarian-acquire-packages")
#' catalog(collection)
#'
#' # Add pre-built packages
#' acquire_package(c("dplyr", "ggplot2"), path = collection)
#'
#' # Add a GitHub package (compiled to WebAssembly later, by bind())
#' acquire_package("tidyverse/ggplot2", source = "github", path = collection)
#'
#' # Add a local package directory
#' pkg <- file.path(collection, "mypackage")
#' dir.create(pkg)
#' writeLines(c("Package: mypackage", "Version: 0.1.0"), file.path(pkg, "DESCRIPTION"))
#' acquire_package("mypackage", source = "local", path = collection)
#'
#' collection_packages(collection)
#'
#' unlink(collection, recursive = TRUE)
#' if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old)
acquire_package <- function(packages, path = ".", source = c("prebuilt", "local", "github")) {
  check_collection(path)
  source <- match.arg(source)
  root <- collection_root(path)
  config_path <- fs::path(root, "_webrarian.yml")

  config <- read_config_file(config_path)

  packages <- as.character(packages)
  if (identical(source, "prebuilt")) {
    bad <- packages[!is_valid_package_name(packages)]
    if (length(bad) > 0) {
      cli::cli_abort(c(
        "{.val {bad}} {?is not a valid R package name/are not valid R package names}.",
        "i" = "Use {.code source = \"github\"} for owner/repo, or {.code source = \"local\"} for a directory."
      ))
    }
    warn_unavailable_prebuilt(packages, config)
  } else if (identical(source, "github")) {
    bad <- packages[
      !grepl(
        "^[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?/[A-Za-z0-9._-]+(?:/[^@]+)?(?:@.+)?$",
        packages,
        perl = TRUE
      )
    ]
    if (length(bad) > 0) {
      cli::cli_abort("{.val {bad}} {?is/are} not of the form owner/repo[/subdir][@ref].")
    }
  } else {
    for (p in packages) {
      resolve_local_package_path(root, p)
    }
  }

  source_key <- source
  existing <- config$packages[[source_key]] %||% list()
  new_packages <- setdiff(packages, unlist(existing))

  if (length(new_packages) == 0) {
    cli::cli_alert_info("All packages already in configuration")
    return(invisible(collection_settings(path)))
  }

  config$packages[[source_key]] <- as.list(c(unlist(existing), new_packages))

  write_config_file(config, config_path)

  cli::cli_alert_success(
    "Added {length(new_packages)} package{?s} from {.val {source}}: {.pkg {new_packages}}"
  )

  invisible(collection_settings(path))
}

#' Warn about prebuilt packages that no repository has
#'
#' A misspelled name would otherwise be accepted silently and fail only in the
#' visitor's browser: a site with `build.bundle-engine: false` installs its
#' packages when the page opens, and bind() downloads nothing for it. The
#' names are looked up in the repositories bind() uses (repo.r-wasm.org, then
#' `packages.repos`) through check_inventory()'s index cache, and
#' the package is added either way: the index may be an hour old, or the
#' repository that has it may be added next. When any repository does not
#' answer, or the collection names a webR version webrarian cannot resolve,
#' nothing is said; bind() and check_inventory() report those.
#' @return The names no repository has, invisibly.
#' @noRd
warn_unavailable_prebuilt <- function(packages, config) {
  r_version <- tryCatch(
    suppressWarnings(get_r_version_for_webr(config$webr$version %||% webr_version_default())),
    error = function(e) NULL
  )
  if (is.null(r_version)) {
    return(invisible(character()))
  }
  repos <- unique(c(default_repo_url(), as.character(unlist(config$packages$repos))))
  # fetch_repo_index() announces unreachable repositories; here that is noise.
  index <- suppressMessages(fetch_webr_packages_index(repos, r_version))
  if (is.null(index) || is.null(index$packages)) {
    return(invisible(character()))
  }
  # A repository that did not answer may have these names, so "no repository
  # has it" is unknown, not true (check_inventory() reports NA here).
  if (length(index$failed) > 0L) {
    return(invisible(character()))
  }
  missing <- setdiff(packages, c(index$packages[, "Package"], base_package_names()))
  if (length(missing) > 0) {
    cli::cli_warn(c(
      "No package repository has {.pkg {missing}}.",
      "i" = "{cli::qty(length(missing))}{?It is/They are} added to {.field packages.prebuilt} anyway: check the spelling, or add the repository that has {?it/them} to {.field packages.repos}."
    ))
  }
  invisible(missing)
}

#' Withdraw packages from the collection
#'
#' Removes packages from the project configuration.
#'
#' @param packages Character vector of package names to remove.
#' @param path Project path.
#'
#' @return Invisibly returns the updated configuration.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-withdraw-packages")
#' catalog(collection, packages = c("dplyr", "ggplot2", "tidyr"))
#'
#' withdraw_package("dplyr", path = collection)
#' withdraw_package(c("ggplot2", "tidyr"), path = collection)
#'
#' collection_packages(collection)
#'
#' unlink(collection, recursive = TRUE)
withdraw_package <- function(packages, path = ".") {
  check_collection(path)
  root <- collection_root(path)
  config_path <- fs::path(root, "_webrarian.yml")

  config <- read_config_file(config_path)
  removed <- character()

  for (source in c("prebuilt", "github", "local")) {
    existing <- config$packages[[source]] %||% list()
    existing_vec <- unlist(existing)

    to_remove <- intersect(packages, existing_vec)
    if (length(to_remove) > 0) {
      config$packages[[source]] <- as.list(setdiff(existing_vec, to_remove))
      removed <- c(removed, to_remove)
    }
  }

  if (length(removed) == 0) {
    cli::cli_alert_warning("No matching packages found in configuration")
    return(invisible(collection_settings(path)))
  }

  write_config_file(config, config_path)

  cli::cli_alert_success(
    "Removed {length(removed)} package{?s}: {.pkg {removed}}"
  )

  invisible(collection_settings(path))
}

#' List the packages a collection uses
#'
#' @param path The collection, or any directory inside it.
#'
#' @return A `webrarian_packages` data frame with one row per configured
#'   package, with columns `package` and `source` (`"prebuilt"`, `"github"` or
#'   `"local"`).
#'   It has zero rows when no package is configured. [check_inventory()] says
#'   which of them webR can install.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-packages-demo")
#' catalog(collection, packages = c("dplyr", "ggplot2"))
#' collection_packages(collection)
#'
#' unlink(collection, recursive = TRUE)
collection_packages <- function(path = ".") {
  check_collection(path)
  config <- collection_settings(path)
  rows <- lapply(c("prebuilt", "github", "local"), function(source) {
    pkgs <- as.character(unlist(config$packages[[source]]))
    data.frame(package = pkgs, source = rep(source, length(pkgs)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("webrarian_packages", "data.frame")
  out
}

#' Print method for webrarian_packages
#'
#' @param x A `webrarian_packages` object.
#' @param ... Additional arguments passed to methods (ignored).
#' @return `x`, invisibly.
#' @export
print.webrarian_packages <- function(x, ...) {
  if (nrow(x) == 0) {
    cli::cli_alert_info("No packages configured")
    return(invisible(x))
  }

  cli::cli_h1("Webrarian Packages")

  for (src in unique(x$source)) {
    pkgs <- x$package[x$source == src]
    cli::cli_h2("{src}")
    cli::cli_ul(pkgs)
  }

  invisible(x)
}

# Configuration management

#' Read collection settings
#'
#' Reads and returns the project configuration from `_webrarian.yml`.
#'
#' @param path Path to project directory (the current directory by default)
#'
#' @return A list with class `webrarian_config` containing the configuration
#'
#' @export
#'
#' @examples
#' # Read the settings of one of the bundled example collections
#' config <- collection_settings(collection_example("data-analysis"))
#' config$project$name
#' config$webr$version
#'
#' # The print method summarizes the whole configuration
#' config
collection_settings <- function(path = ".") {
  root <- collection_root(path)

  if (is.null(root)) {
    cli::cli_abort(
      c(
        "Not a webrarian collection",
        "i" = "No {.file _webrarian.yml} found in {.path {path}} or parent directories",
        "i" = "Run {.run webrarian::catalog()} to create a new collection"
      ),
      class = "webrarian_error_not_collection"
    )
  }

  config_path <- fs::path(root, "_webrarian.yml")
  raw <- read_config_file(config_path)
  validate_config(raw)

  config <- apply_config_defaults(raw %||% list(), root)
  config$.path <- root
  class(config) <- c("webrarian_config", class(config))
  config
}

#' Get a settings value
#'
#' @param config A `webrarian_config` object from [collection_settings()].
#' @param key One string naming the setting, its levels separated by `.` or
#'   `/`, as in `"build.output-dir"` or `"build/output_dir"`. Within a level,
#'   hyphens and underscores are interchangeable, except below `brand`, whose
#'   keys are brand.yml's own.
#' @param default Value returned when the key is not set.
#'
#' @return The configuration value or `default`.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-get-demo")
#' catalog(collection, packages = "dplyr")
#' config <- collection_settings(collection)
#'
#' settings_get(config, "webr.version")
#' settings_get(config, "packages.prebuilt")
#' settings_get(config, "files/mount-point")
#'
#' # Keys that are not set fall back to `default`
#' settings_get(config, "packages.conda", default = character())
#'
#' unlink(collection, recursive = TRUE)
settings_get <- function(config, key, default = NULL) {
  if (missing(key)) {
    cli::cli_abort(c(
      "{.arg key} is missing.",
      "i" = "Name the setting, e.g. {.code settings_get(config, \"build.output-dir\")}."
    ))
  }
  keys <- normalize_setting_key(key)

  value <- config
  for (k in keys) {
    if (is.null(value) || !is.list(value) || is.null(value[[k]])) {
      return(default)
    }
    value <- value[[k]]
  }

  # Before 0.1.0 the levels could be passed separately ("build", then
  # "output-dir"); that second level now lands in `default`. Say so.
  if (
    is.list(value) &&
      !is.null(names(value)) &&
      is.character(default) &&
      length(default) == 1L &&
      gsub("-", "_", default, fixed = TRUE) %in% names(value)
  ) {
    cli::cli_warn(c(
      "{.fn settings_get} takes one key.",
      "i" = "Did you mean {.code settings_get(config, \"{key}.{default}\")}?"
    ))
  }
  value
}

#' Set configuration values
#'
#' Update configuration values in the `_webrarian.yml` file. The file is
#' rewritten as a whole, so comments in it are not preserved.
#'
#' @param path Path to project directory.
#' @param ... Named arguments to set. A name is a key whose levels are
#'   separated by `.` or `/` (`"build.output-dir"` or `"build/output_dir"`).
#'   Within a level, hyphens and underscores are interchangeable, except below
#'   `brand`. A `NULL` value writes an explicit null, so the default applies.
#'
#' @return Invisibly returns the updated configuration.
#'
#' @export
#'
#' @examples
#' # Work in a throwaway collection under tempdir()
#' collection <- file.path(tempdir(), "webrarian-settings-demo")
#' catalog(collection)
#'
#' settings_set(collection, "webr.version" = "0.6.0")
#' settings_set(collection, "project.name" = "my-app", "build.output-dir" = "public")
#'
#' config <- collection_settings(collection)
#' settings_get(config, "project.name")
#' settings_get(config, "build.output-dir")
#'
#' unlink(collection, recursive = TRUE)
settings_set <- function(path = ".", ...) {
  check_collection(path)
  root <- collection_root(path)
  config_path <- fs::path(root, "_webrarian.yml")

  updates <- list(...)
  if (length(updates) == 0L) {
    cli::cli_abort("Nothing to set: pass settings as {.code \"key\" = value} pairs.")
  }
  keys <- names(updates)
  if (is.null(keys) || any(!nzchar(keys))) {
    cli::cli_abort(c(
      "Every setting passed to {.fn settings_set} must be named.",
      "i" = "For example: {.code settings_set(path, \"build.output-dir\" = \"docs\")}."
    ))
  }

  config <- read_config_file(config_path) %||% list()
  for (i in seq_along(updates)) {
    path_i <- normalize_setting_key(keys[[i]])
    value_i <- normalize_config_value(paste(path_i, collapse = "."), updates[[i]])
    if (!identical(path_i[[1]], "brand")) {
      validate_config(set_nested_value(list(), path_i, value_i), strict = TRUE)
    }
    config <- set_nested_value(config, path_i, value_i)
  }

  write_config_file(config, config_path)
  invisible(collection_settings(path))
}

#' Split a settings key into normalized levels
#'
#' `"build.output-dir"`, `"build/output_dir"` and friends all become
#' `c("build", "output_dir")`: the settings object is snake_case, and the file
#' spelling is translated on write. Below `brand` nothing is translated,
#' because those names belong to brand.yml.
#' @noRd
normalize_setting_key <- function(key, call = rlang::caller_env()) {
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(trimws(key))) {
    cli::cli_abort(
      "A setting name must be a non-empty string such as {.val build.output-dir}.",
      call = call
    )
  }
  segments <- strsplit(trimws(key), "[./]")[[1]]
  if (any(!nzchar(segments)) || grepl("[./]$", trimws(key))) {
    cli::cli_abort("Setting name {.val {key}} has an empty segment.", call = call)
  }
  if (identical(segments[[1]], "brand")) {
    return(segments)
  }
  gsub("-", "_", segments, fixed = TRUE)
}

#' Apply default configuration values
#'
#' Defaults come from the key table in R/config-spec.R. The two with no
#' static default are filled here: the project name (the collection's
#' directory name) and the webR version (see webr_version_default()).
#' @noRd
apply_config_defaults <- function(config, root = NULL) {
  result <- merge_config(config_defaults(), config)
  if (is.null(result$project$name)) {
    result$project$name <- if (is.null(root)) {
      "webrarian-project"
    } else {
      as.character(fs::path_file(root))
    }
  }
  if (is.null(result$webr$version)) {
    result$webr$version <- webr_version_default()
  }
  result
}

#' Merge two configuration lists
#'
#' Recurses only where both sides are mappings (named lists). A YAML sequence
#' (an unnamed list, including the empty `[]`) replaces the default, so
#' `exclude: []` really clears the default excludes.
#' @noRd
merge_config <- function(defaults, config) {
  if (is.null(config)) {
    return(defaults)
  }
  is_mapping <- function(x) is.list(x) && !is.null(names(x))

  result <- defaults
  for (name in names(config)) {
    # A key present in the file but with no value (a blank `version:`) parses
    # to NULL. Keep the default rather than deleting it.
    if (is.null(config[[name]])) {
      next
    }
    if (is_mapping(defaults[[name]]) && is_mapping(config[[name]])) {
      result[[name]] <- merge_config(defaults[[name]], config[[name]])
    } else {
      result[[name]] <- config[[name]]
    }
  }
  result
}

#' Set a nested value in a list
#'
#' `x[key] <- list(value)` rather than `x[[key]] <- value`, so an explicit NULL
#' is stored instead of deleting the key.
#' @noRd
set_nested_value <- function(x, keys, value) {
  if (length(keys) == 1L) {
    x[keys] <- list(value)
    return(x)
  }
  child <- x[[keys[[1]]]]
  if (is.null(child)) {
    child <- list()
  }
  if (!is.list(child)) {
    cli::cli_abort(
      "Cannot set a key below {.field {keys[[1]]}}: it holds a value, not a group of settings."
    )
  }
  x[[keys[[1]]]] <- set_nested_value(child, keys[-1], value)
  x
}

#' Print method for webrarian_config
#'
#' @param x A `webrarian_config` object.
#' @param ... Additional arguments passed to methods (ignored).
#' @return `x`, invisibly.
#' @export
print.webrarian_config <- function(x, ...) {
  cli::cli_h1("Webrarian Configuration")
  cli::cli_text("Project: {.val {x$project$name}}")
  cli::cli_text("Path: {.path {x$.path}}")

  cli::cli_h2("webR")
  cli::cli_text("Version: {.val {x$webr$version}}")

  cli::cli_h2("Packages")
  n_prebuilt <- length(x$packages$prebuilt)
  n_github <- length(x$packages$github)
  n_local <- length(x$packages$local)
  cli::cli_text("Prebuilt: {n_prebuilt}, GitHub: {n_github}, Local: {n_local}")

  cli::cli_h2("Files")
  cli::cli_text("Include patterns: {length(x$files$include)}")
  cli::cli_text("Mount point: {.path {x$files$mount_point}}")

  cli::cli_h2("REPL")
  shown <- names(Filter(isTRUE, x$repl$panels))
  cli::cli_text("Panels: {.val {shown}}")
  cli::cli_text("Share links: {.val {x$repl$share_links}}")

  if (is.character(x$brand)) {
    # `brand: path/to/brand.yml` stays a path until bind() reads it.
    cli::cli_h2("Brand")
    cli::cli_text("Brand: {.path {x$brand}}")
  } else if (is.list(x$brand)) {
    cli::cli_h2("Brand")
    name <- x$brand$meta$name
    if (is.list(name)) {
      name <- name$full %||% name$short
    }
    if (!is.null(name)) {
      cli::cli_text("Name: {.val {name}}")
    }
    if (!is.null(x$brand$color$primary)) cli::cli_text("Primary: {.val {x$brand$color$primary}}")
  }

  cli::cli_h2("UI")
  if (!is.null(x$ui$custom_css)) {
    cli::cli_text("Custom CSS: {.path {x$ui$custom_css}}")
  }
  if (!is.null(x$ui$loading$message)) {
    cli::cli_text("Loading: {.val {x$ui$loading$message}}")
  }

  cli::cli_h2("Build")
  cli::cli_text("Output: {.path {x$build$output_dir}}")
  cli::cli_text("Offline: {.val {isTRUE(x$build$offline)}}")
  cli::cli_text("Bundle engine: {.val {!isFALSE(x$build$bundle_engine)}}")
  cli::cli_text("Service worker: {.val {isTRUE(x$build$service_worker)}}")

  invisible(x)
}

# Internal utility functions

#' Is Docker usable?
#'
#' `docker --version` succeeds without a daemon, so probe the daemon itself.
#' @return "running", "stopped" (the CLI is installed but the daemon does not
#'   answer), or "missing" (no docker on PATH).
#' @noRd
docker_status <- function(timeout = 10) {
  if (!nzchar(docker_path())) {
    return("missing")
  }
  out <- suppressWarnings(tryCatch(
    system2(
      "docker",
      c("info", "--format", shQuote("{{.ServerVersion}}")),
      stdout = TRUE,
      stderr = TRUE,
      timeout = timeout
    ),
    error = function(e) structure(conditionMessage(e), status = 127L)
  ))
  status <- attr(out, "status") %||% 0L
  if (identical(as.integer(status), 0L) && length(out) > 0L && nzchar(trimws(out[[1]]))) {
    "running"
  } else {
    "stopped"
  }
}

#' The docker executable's path, or "" when there is none
#' @noRd
docker_path <- function() {
  unname(Sys.which("docker"))
}

#' Check if the Docker daemon is running
#' @noRd
docker_available <- function() {
  identical(docker_status(), "running")
}

#' Normalize a path relative to project root
#' @noRd
normalize_project_path <- function(path, root = ".") {
  if (fs::is_absolute_path(path)) {
    return(fs::path_norm(path))
  }
  fs::path_norm(fs::path(root, path))
}

#' Read a YAML file safely
#' @noRd
read_yaml_file <- function(path) {
  if (!fs::file_exists(path)) {
    return(NULL)
  }
  yaml::read_yaml(path)
}

#' Write a YAML file
#' @noRd
write_yaml_file <- function(x, path) {
  yaml::write_yaml(
    x,
    path,
    handlers = list(
      logical = function(x) {
        result <- ifelse(x, "true", "false")
        class(result) <- "verbatim"
        result
      }
    )
  )
  invisible(path)
}

# Config keys are lowercase with hyphens in the file, and snake_case once they are R
# objects. Both spellings describe the same key; only the file is the interface.
#
# The alternative is writing `config$files$`mount-point`` at every one of the several hundred
# places the config is read, which is not R anyone wants to read or maintain. So the
# translation happens where the file is, and nothing downstream changes.
#
# The rule is syntactic rather than a table of key names, so a key added later follows it
# automatically and there is no list to fall out of date.
#
# `brand` is skipped in both directions: it holds a brand.yml document, whose key names belong
# to Posit's standard rather than to webrarian.

#' Rename config keys from the file's hyphens to snake_case
#'
#' Errors on an underscore. Without that check an underscored key would flow through
#' untouched and be read as if it were spelled with hyphens.
#' @noRd
config_keys_to_snake <- function(x, .at = character()) {
  if (!is.list(x) || is.null(names(x))) {
    return(x)
  }
  nms <- names(x)
  bad <- nms[grepl("_", nms, fixed = TRUE)]
  if (length(bad) > 0) {
    where <- if (length(.at) > 0) paste0(paste(.at, collapse = "."), ".") else ""
    cli::cli_abort(
      c(
        "Config key{?s} {.field {paste0(where, bad)}} use{?s/} an underscore.",
        "i" = "Keys in {.file _webrarian.yml} are lowercase with hyphens: {.field {gsub('_', '-', bad)}}."
      ),
      class = "webrarian_error_config_key"
    )
  }
  # lapply, not `x[[i]] <- `: assigning NULL deletes the element instead of setting it, and the
  # config is full of explicit nulls (`startup-script: null`), which would silently vanish.
  renamed <- gsub("-", "_", nms, fixed = TRUE)
  out <- lapply(seq_along(x), function(i) {
    if (identical(renamed[[i]], "brand")) {
      x[[i]]
    } else {
      config_keys_to_snake(x[[i]], c(.at, renamed[[i]]))
    }
  })
  names(out) <- renamed
  out
}

#' Rename config keys from snake_case back to the file's hyphens
#' @noRd
config_keys_to_kebab <- function(x) {
  if (!is.list(x) || is.null(names(x))) {
    return(x)
  }
  nms <- names(x)
  out <- lapply(seq_along(x), function(i) {
    if (identical(nms[[i]], "brand")) x[[i]] else config_keys_to_kebab(x[[i]])
  })
  names(out) <- gsub("_", "-", nms, fixed = TRUE)
  out
}

#' Read `_webrarian.yml`, translating its keys
#'
#' Choice values YAML turned into booleans (an unquoted `share-links: off`)
#' are read as the choice they mean; see normalize_config_value().
#' @noRd
read_config_file <- function(path) {
  normalize_config_values(config_keys_to_snake(read_yaml_file(path)))
}

#' The comment webrarian writes at the top of every `_webrarian.yml`
#'
#' The helpers rewrite the whole file, so comments a user adds do not
#' survive; the header says so and names the key reference.
#' @noRd
config_file_header <- function() {
  c(
    "# Settings for this webrarian collection. Every key is described in",
    "# vignette(\"config-reference\", package = \"webrarian\"). settings_set(),",
    "# acquire_*() and withdraw_*() rewrite this file, so comments are not kept."
  )
}

#' Write `_webrarian.yml`, translating its keys
#'
#' Writes a temporary file next to the target, re-reads it, and only then
#' renames it over `path`. A result the YAML reader rejects (a duplicate key,
#' say) therefore never replaces a working config.
#' @noRd
write_config_file <- function(x, path) {
  tmp <- fs::path(fs::path_dir(path), sprintf(".%s.tmp-%s", fs::path_file(path), Sys.getpid()))
  on.exit(if (fs::file_exists(tmp)) fs::file_delete(tmp), add = TRUE)
  write_yaml_file(config_keys_to_kebab(x), tmp)
  body <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
  writeLines(enc2utf8(c(config_file_header(), body)), tmp, useBytes = TRUE)
  reread <- tryCatch(yaml::read_yaml(tmp), error = function(e) e)
  if (inherits(reread, "error")) {
    cli::cli_abort(c(
      "Refusing to write {.file {fs::path_file(path)}}: the result would not parse.",
      "x" = conditionMessage(reread)
    ))
  }
  fs::file_move(tmp, path)
  invisible(path)
}

#' The webR versions verified with the vendored viewer's webR client
#' @noRd
webr_versions_table <- function() {
  path <- system.file("extdata", "webr-versions.json", package = "webrarian")
  if (!nzchar(path)) {
    cli::cli_abort("The bundled webR version list is missing.")
  }
  jsonlite::read_json(path)
}

#' The default webR version (the entry marked "default" in webr-versions.json)
#' @noRd
default_webr_version <- function() {
  versions <- webr_versions_table()$versions
  for (v in versions) {
    if (isTRUE(v$default)) {
      return(v$version)
    }
  }
  versions[[1]]$version
}

#' The webR version to use when the config names none
#'
#' One precedence rule everywhere: an explicit `webr.version` wins, then the
#' `WEBRARIAN_WEBR_VERSION` environment variable, then the default.
#' @noRd
webr_version_default <- function() {
  env <- Sys.getenv("WEBRARIAN_WEBR_VERSION", unset = "")
  if (nzchar(env)) env else default_webr_version()
}

#' The webr-versions.json entry for a version, or NULL for an unlisted one
#' @noRd
webr_versions_entry <- function(version) {
  for (entry in webr_versions_table()$versions) {
    if (identical(entry$version, version)) {
      return(entry)
    }
  }
  NULL
}

#' Validate that `path` is a webrarian collection or inside one
#'
#' Every function that takes a `path` accepts any directory inside a
#' collection, as collection_settings() does, and then works on the root
#' collection_root() finds.
#' @noRd
check_collection <- function(path = ".", call = rlang::caller_env()) {
  if (is.null(collection_root(path))) {
    cli::cli_abort(
      c(
        "Not a webrarian collection",
        "i" = "No {.file _webrarian.yml} found in {.path {path}} or its parent directories",
        "i" = "Run {.run webrarian::catalog()} to create a new collection"
      ),
      call = call,
      class = "webrarian_error_not_collection"
    )
  }
  invisible(TRUE)
}

#' Ensure a directory exists
#' @noRd
ensure_dir <- function(path) {
  if (!fs::dir_exists(path)) {
    fs::dir_create(path, recurse = TRUE)
  }
  invisible(path)
}

#' HTML-escape a string for safe interpolation into HTML text or attributes
#'
#' Escapes the five characters that are unsafe in HTML element content and
#' double-quoted attribute values, so user-supplied config (title, description,
#' file names) cannot break out of a tag or inject markup into a deployed site.
#' @noRd
html_escape <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

#' Names of the base R packages
#'
#' The set of packages that ship with R itself and therefore never need to be
#' fetched as WebAssembly builds.
#'
#' `utils::installed.packages()` would answer this too, but CRAN discourages it:
#' it scans every library path on every call, is slow on large libraries, and
#' can fail outright on a partially-written library. The base set is fixed by
#' the R distribution, so a literal list is both faster and more predictable.
#' `priority = "base"` in `installed.packages()` returns exactly these.
#'
#' @return A character vector of base package names.
#' @noRd
base_package_names <- function() {
  c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils"
  )
}

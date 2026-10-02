# The example collections shipped in inst/examples/

#' Copy an example collection
#'
#' webrarian ships ready-to-build examples. This copies one into `dest` (or a
#' new temporary directory) and returns the copy's path, so that building it
#' never writes into the installed package.
#'
#' @param name The example, one of `"basic"`, `"branded"`, `"data-analysis"`
#'   (prebuilt packages and a multi-file analysis), `"local-package"` (a local
#'   package compiled in Docker) or `"mirror"` (a script that builds a
#'   mirror).
#' @param dest Directory to copy the example into. It is created if needed and
#'   must be empty if it exists, because nothing is ever overwritten. `NULL` (the
#'   default) copies into a new directory under [tempdir()].
#'
#' @return The path of the copy.
#'
#' @export
#'
#' @examples
#' path <- collection_example("basic")
#' list.files(path)
#'
#' # Build and look at the copy, loading the webR engine from its CDN so
#' # nothing is downloaded; the installed example is untouched
#' settings_set(path, "build.bundle-engine" = FALSE)
#' bind(path)
#' collection_files(path)$files
#'
#' unlink(path, recursive = TRUE)
collection_example <- function(name, dest = NULL) {
  available <- example_names()
  if (missing(name)) {
    cli::cli_abort(c(
      "Name the example to copy.",
      "i" = "Examples: {.val {available}}."
    ))
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% available) {
    cli::cli_abort(c(
      "Unknown example: {.val {name}}.",
      "i" = "Examples: {.val {available}}."
    ))
  }
  dest <- dest %||% tempfile(paste0("webrarian-", name, "-"))
  if (!is.character(dest) || length(dest) != 1L || is.na(dest) || !nzchar(dest)) {
    cli::cli_abort("{.arg dest} must be a single directory path.")
  }
  if (fs::file_exists(dest) && !fs::dir_exists(dest)) {
    cli::cli_abort("{.path {dest}} is a file, not a directory.")
  }
  if (fs::dir_exists(dest) && length(list.files(dest, all.files = TRUE, no.. = TRUE)) > 0L) {
    cli::cli_abort(c(
      "{.path {dest}} already holds files.",
      "i" = "Choose a new or empty directory: {.fn collection_example} never overwrites anything."
    ))
  }

  src <- system.file("examples", name, package = "webrarian")
  files <- list.files(src, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  # A developer may have built an example in place under devtools::load_all().
  files <- files[!grepl("^(_site|\\.webrarian)(/|$)", files)]
  fs::dir_create(dest)
  for (f in files) {
    target <- fs::path(dest, f)
    fs::dir_create(fs::path_dir(target))
    fs::file_copy(fs::path(src, f), target)
  }
  cli::cli_alert_success("Copied the {.val {name}} example to {.path {dest}}")
  fs::path(dest)
}

#' Names of the shipped examples
#' @noRd
example_names <- function() {
  examples_dir <- system.file("examples", package = "webrarian")
  if (!nzchar(examples_dir)) {
    return(character())
  }
  sort(list.dirs(examples_dir, recursive = FALSE, full.names = FALSE))
}

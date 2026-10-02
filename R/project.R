# Project initialization and management

#' Packages used to build or render a project, never needed inside webR
#' @noRd
tooling_packages <- function() {
  c(
    "webrarian",
    "renv",
    "rmarkdown",
    "knitr",
    "quarto",
    "devtools",
    "usethis",
    "testthat",
    "roxygen2",
    "pak",
    "remotes",
    "pkgload",
    "rsconnect",
    "rhub",
    "covr",
    "lintr",
    "styler",
    "shinylive"
  )
}

#' Is the optional renv package installed? (A function so tests can say no.)
#' @noRd
renv_available <- function() {
  requireNamespace("renv", quietly = TRUE)
}

#' Detect package dependencies from R files
#'
#' Scans a directory for R files and extracts package dependencies using
#' renv::dependencies(). Requires the renv package to be installed.
#'
#' @param path Directory to scan
#' @return Character vector of package names (excluding base R packages)
#' @noRd
detect_package_dependencies <- function(path) {
  # renv is optional (Suggests): without it, detection is skipped, not an error.
  if (!renv_available()) {
    cli::cli_inform(c(
      "i" = "Skipping package detection: {.pkg renv} is not installed.",
      " " = "Install it with {.run install.packages(\"renv\")} to detect packages from R files."
    ))
    return(character())
  }

  # Use renv to find dependencies (scans .R, .Rmd, .qmd files)
  deps <- tryCatch(
    renv::dependencies(path, quiet = TRUE, errors = "ignored"),
    error = function(e) NULL
  )

  if (is.null(deps) || nrow(deps) == 0) {
    return(character())
  }

  # Base packages ship with webR; tooling packages build the project and are
  # not available (or useful) in the browser.
  setdiff(unique(deps$Package), c(base_package_names(), tooling_packages()))
}

#' Detect file patterns to include from existing directory
#'
#' Scans a directory for common R-related file patterns and directories
#' that should be included in the webrarian collection.
#'
#' @param path Directory to scan
#' @return Character vector of file patterns (e.g., "*.R", "data/")
#' @noRd
detect_file_patterns <- function(path) {
  patterns <- character()

  # Check for R scripts in root
  r_files <- fs::dir_ls(path, glob = "*.R", recurse = FALSE, type = "file")
  if (length(r_files) > 0) {
    patterns <- c(patterns, "*.R")
  }

  # Check for Quarto/Rmd files
  qmd_files <- fs::dir_ls(path, glob = "*.qmd", recurse = FALSE, type = "file")
  rmd_files <- fs::dir_ls(path, glob = "*.Rmd", recurse = FALSE, type = "file")
  if (length(qmd_files) > 0) {
    patterns <- c(patterns, "*.qmd")
  }
  if (length(rmd_files) > 0) {
    patterns <- c(patterns, "*.Rmd")
  }

  # Check for common directories
  common_dirs <- c("data", "scripts", "R", "src", "code")
  for (dir_name in common_dirs) {
    dir_path <- fs::path(path, dir_name)
    if (fs::dir_exists(dir_path)) {
      patterns <- c(patterns, paste0(dir_name, "/"))
    }
  }

  # Check for data file extensions in root
  data_exts <- c("csv", "rds", "RData", "xlsx", "json", "parquet")
  for (ext in data_exts) {
    files <- fs::dir_ls(path, glob = paste0("*.", ext), recurse = FALSE, type = "file")
    if (length(files) > 0) {
      patterns <- c(patterns, paste0("*.", ext))
      break # Only need one data pattern indicator
    }
  }

  unique(patterns)
}

#' Catalog a new webrarian project
#'
#' Creates a new webrarian collection, with a `_webrarian.yml` and, for some
#' templates, a starting directory structure. The project name is the
#' directory's name. Change it with `settings_set(path, "project.name" = ...)`.
#' When run on an existing directory containing R files, it detects package
#' dependencies and file patterns to include.
#'
#' @param path Path where the project should be created. Use `"."` for the
#'   current directory (default), or provide a directory name. If the directory
#'   exists and contains R files, package dependencies are auto-detected.
#' @param packages Character vector of prebuilt WebAssembly package names to
#'   add to the template's own (see [acquire_package()]).
#' @param template The project template, one of these:
#'   - `"minimal"`: Basic configuration only
#'   - `"data-analysis"`: Includes common data analysis packages
#'   - `"package"`: For bundling an existing R package
#' @param detect If `TRUE` (default), scans existing directories for R files
#'   and auto-detects package dependencies and file patterns. Set to `FALSE`
#'   for a clean configuration without scanning.
#'
#' @details
#' ## Dependency detection
#'
#' When `detect = TRUE` and the directory already exists, webrarian takes two
#' steps:
#'
#' 1. **Scans for package dependencies** using `renv::dependencies()`:
#'    - `library(pkg)` and `require(pkg)` calls
#'    - `pkg::function()` namespace calls
#'    - Package declarations in Rmd/qmd YAML headers
#'
#' 2. **Detects file patterns** to include:
#'    - R scripts (`*.R`, `*.Rmd`, `*.qmd`)
#'    - Common directories (`data/`, `scripts/`, `R/`)
#'    - Data files (`*.csv`, `*.rds`, etc.)
#'
#' Dependency detection requires the `renv` package. Install it with
#' `install.packages("renv")` for full functionality.
#'
#' @return Invisibly returns the project path.
#'
#' @export
#'
#' @examples
#' # Create a collection in a new directory
#' collection <- file.path(tempdir(), "my-webr-app")
#' catalog(collection)
#' is_collection(collection)
#'
#' # Start from a template, with packages registered up front
#' analysis <- file.path(tempdir(), "my-analysis")
#' catalog(
#'   analysis,
#'   packages = c("dplyr", "ggplot2"),
#'   template = "data-analysis"
#' )
#' collection_packages(analysis)
#'
#' unlink(c(collection, analysis), recursive = TRUE)
#'
#' # An existing folder of R scripts: its packages and files are detected
#' scripts <- file.path(tempdir(), "my-scripts")
#' dir.create(scripts)
#' writeLines("library(dplyr)\nsummary(1:10)", file.path(scripts, "analysis.R"))
#' catalog(scripts)
#' unlink(scripts, recursive = TRUE)
catalog <- function(
  path = ".",
  template = c("minimal", "data-analysis", "package"),
  packages = NULL,
  detect = TRUE
) {
  template <- match.arg(template)
  path <- fs::path_abs(path)

  # Track if this is an existing directory
  existing_dir <- fs::dir_exists(path)

  # Create new directory if needed
  if (!existing_dir) {
    fs::dir_create(path, recurse = TRUE)
    cli::cli_alert_success("Created directory {.path {path}}")
  }

  # Fail if already a collection
  if (is_collection(path)) {
    cli::cli_abort(c(
      "Directory is already a webrarian collection",
      "i" = "Found existing {.file _webrarian.yml} in {.path {path}}"
    ))
  }

  # Auto-detect packages and files from existing directory
  detected_packages <- character()
  detected_files <- character()

  if (existing_dir && detect) {
    if (identical(template, "package")) {
      # The package's own sources and data are compiled, not bundled, and its
      # dependencies are bundled with it, so only what examples/
      # uses is detected, never the package itself; the template's
      # files.include (examples/) stays as it is. pyodidarian's package
      # template does the same (package-template-detects-examples-only).
      examples_dir <- fs::path(path, "examples")
      if (fs::dir_exists(examples_dir)) {
        detected_packages <- setdiff(
          detect_package_dependencies(examples_dir),
          local_package_name(path)
        )
      }
    } else {
      detected_packages <- detect_package_dependencies(path)
      detected_files <- detect_file_patterns(path)
    }

    if (length(detected_packages) > 0) {
      cli::cli_alert_info(
        "Detected {length(detected_packages)} package{?s} from R files: {.val {detected_packages}}"
      )
    }
    if (length(detected_files) > 0) {
      cli::cli_alert_info("Detected file patterns: {.val {detected_files}}")
    }
  }

  # path is absolute here, so its last component is the directory's name.
  name <- as.character(fs::path_file(path))

  config <- create_initial_config(name, template)

  # User-specified and detected packages add to the template's own.
  all_packages <- unique(c(unlist(config$packages$prebuilt), packages, detected_packages))
  if (length(all_packages) > 0) {
    config$packages$prebuilt <- as.list(all_packages)
  }

  if (length(detected_files) > 0) {
    config$files$include <- as.list(unique(c(
      unlist(config$files$include),
      detected_files
    )))
  }

  config_path <- fs::path(path, "_webrarian.yml")
  write_config_file(config, config_path)
  cli::cli_alert_success("Created {.file _webrarian.yml}")

  create_project_structure(path, template)

  ignored <- add_to_gitignore(path, c("_site/", ".webrarian/"))
  if (length(ignored) > 0) {
    cli::cli_alert_success("Added {.val {ignored}} to {.file .gitignore}")
  }
  build_ignored <- add_to_rbuildignore(path, c("^_webrarian\\.yml$", "^_site$", "^\\.webrarian$"))
  if (length(build_ignored) > 0) {
    cli::cli_alert_success("Added {.val {build_ignored}} to {.file .Rbuildignore}")
  }

  cli::cli_alert_success("Cataloged webrarian collection: {.val {name}}")
  cli::cli_text("")
  cli::cli_text("Next steps:")
  cli::cli_bullets(catalog_next_steps(path))

  invisible(path)
}

#' Build the "Next steps" bullets printed by catalog()
#'
#' `catalog()` deliberately never calls [setwd()], so when it catalogs a
#' subdirectory the user is still sitting in the parent directory. The `{.run}`
#' links below execute in the *current* working directory, so in that case the
#' plain `webrarian::bind()` form would fail with "Not a webrarian collection".
#' Lead with the move-in step (and mention `path`) whenever the collection is
#' not the current directory.
#'
#' @param path Absolute path to the collection that was just created.
#' @return A named character vector suitable for [cli::cli_bullets()].
#' @noRd
catalog_next_steps <- function(path) {
  steps <- c(
    " " = "Acquire packages: {.run webrarian::acquire_package(\"dplyr\")}",
    " " = "Acquire files: {.run webrarian::acquire_file(\"data/\")}",
    " " = "Bind: {.run webrarian::bind()}",
    " " = "Reading room: {.run webrarian::reading_room()}"
  )

  if (is_current_dir(path)) {
    return(steps)
  }

  # Shown as {.code}, not {.run}: RStudio refuses to run links that call a core
  # package such as base::setwd(), so a runnable link would be a broken promise.
  # The path is escaped because cli treats `{`/`}` in the text as interpolation.
  where <- escape_cli_braces(relative_display_path(path))

  c(
    c(" " = sprintf("Move into the collection: {.code setwd(\"%s\")}", where)),
    steps,
    c(
      "i" = sprintf(
        "Or pass {.arg path} to each call, e.g. {.code webrarian::bind(path = \"%s\")}",
        where
      )
    )
  )
}

#' Escape cli interpolation braces in literal text
#'
#' @param x Character vector to escape.
#' @return `x` with `{` and `}` doubled so cli prints them literally.
#' @noRd
escape_cli_braces <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

#' Resolve a path to its canonical form, or `NA` if it cannot be resolved
#'
#' @param path Path to resolve.
#' @return A length-1 character, `NA_character_` on failure.
#' @noRd
canonical_path <- function(path) {
  tryCatch(as.character(fs::path_real(path)), error = function(e) NA_character_)
}

#' Is `path` the same directory as the current working directory?
#'
#' Compares canonical paths so that symlinked parents (macOS `/tmp` ->
#' `/private/tmp`) and `.` / `./` spellings all compare equal.
#'
#' @param path Path to compare against the working directory.
#' @return `TRUE` only when both paths resolve to the same directory.
#' @noRd
is_current_dir <- function(path) {
  here <- canonical_path(".")
  there <- canonical_path(path)

  if (is.na(here) || is.na(there)) {
    return(FALSE)
  }

  identical(here, there)
}

#' Path to show the user, relative to the working directory when possible
#'
#' Canonicalizes both ends before comparing so that a symlinked parent (macOS
#' `/tmp` -> `/private/tmp`) still yields the short relative form.
#'
#' @param path Path to display.
#' @return A relative path when `path` sits inside the working directory,
#'   otherwise the path as given.
#' @noRd
relative_display_path <- function(path) {
  here <- canonical_path(".")
  there <- canonical_path(path)

  rel <- NA_character_
  if (!is.na(here) && !is.na(there)) {
    rel <- tryCatch(
      as.character(fs::path_rel(there, start = here)),
      error = function(e) NA_character_
    )
  }

  if (is.na(rel) || rel == "" || rel == "." || startsWith(rel, "..")) {
    return(as.character(path))
  }

  rel
}

#' Check if directory is a webrarian collection
#'
#' @param path Path to check
#'
#' @return `TRUE` if the directory contains a `_webrarian.yml` file.
#'
#' @export
#'
#' @examples
#' is_collection(collection_example("basic"))
#'
#' is_collection(tempdir())
is_collection <- function(path = ".") {
  config_path <- fs::path(path, "_webrarian.yml")
  fs::file_exists(config_path)
}

#' Find collection root
#'
#' Searches upward from the given path to find a webrarian collection root
#' (directory containing `_webrarian.yml`).
#'
#' @param path Starting path to search from
#'
#' @return Path to collection root, or `NULL` if not found.
#'
#' @export
#'
#' @examples
#' example_dir <- collection_example("basic")
#' collection_root(example_dir)
#'
#' # Searching upward from a subdirectory finds the same root
#' collection_root(file.path(example_dir, "data"))
#'
#' # `NULL` when there is no collection above the starting path
#' collection_root(tempdir())
collection_root <- function(path = ".") {
  path <- fs::path_abs(path)

  while (path != fs::path_dir(path)) {
    if (fs::file_exists(fs::path(path, "_webrarian.yml"))) {
      return(path)
    }
    path <- fs::path_dir(path)
  }

  if (fs::file_exists(fs::path(path, "_webrarian.yml"))) {
    return(path)
  }

  NULL
}

#' Create initial configuration based on template
#' @noRd
create_initial_config <- function(name, template) {
  config <- list(
    project = list(
      name = name,
      description = ""
    ),
    webr = list(
      version = webr_version_default()
    ),
    packages = list(
      prebuilt = list(),
      repos = list(),
      github = list(),
      local = list(),
      dependencies = TRUE
    ),
    files = list(
      include = list(),
      exclude = list("**/*.Rhistory", "**/.DS_Store"),
      mount_point = "/home/web_user"
    ),
    repl = list(
      startup_script = NULL,
      auto_open = NULL,
      auto_run = list(),
      share_links = "open",
      panels = list(editor = TRUE, terminal = TRUE, files = TRUE, plot = TRUE, environment = TRUE)
    ),
    build = list(
      output_dir = "_site",
      offline = FALSE,
      bundle_engine = TRUE,
      clean = TRUE
    )
  )

  if (template == "data-analysis") {
    config$packages$prebuilt <- list("dplyr", "ggplot2", "tidyr", "readr")
    config$files$include <- list("data/", "scripts/")
    config$repl$startup_script <- "scripts/init.R"
  } else if (template == "package") {
    config$packages$local <- list(".")
    # The package is compiled and installed from ./repo; its sources are not
    # what a visitor came to read. examples/ is (create_project_structure()
    # writes examples/example.R), as in pyodidarian's package template.
    config$files$include <- list("examples/")
    config$repl$auto_open <- list("examples/example.R")
  }

  config
}

#' The name of the package a collection showcases
#'
#' The `Package` field of `DESCRIPTION`; else the directory's name, when that
#' is a valid package name; else "mypackage".
#' @noRd
local_package_name <- function(path) {
  desc <- fs::path(path, "DESCRIPTION")
  name <- NA_character_
  if (fs::file_exists(desc)) {
    name <- tryCatch(
      unname(read.dcf(desc, fields = "Package")[1, 1]),
      error = function(e) NA_character_
    )
  }
  if (!isTRUE(is_valid_package_name(name))) {
    name <- as.character(fs::path_file(fs::path_abs(path)))
  }
  if (isTRUE(is_valid_package_name(name))) name else "mypackage"
}

#' Create project directory structure
#' @noRd
create_project_structure <- function(path, template) {
  if (template == "package") {
    example <- fs::path(path, "examples", "example.R")
    if (!fs::file_exists(example)) {
      ensure_dir(fs::path_dir(example))
      pkg <- local_package_name(path)
      writeLines(
        c(
          "# Try the package out. bind() compiles the package in this collection",
          "# (packages.local: [.]) and the site installs it when the page opens.",
          "",
          sprintf("library(%s)", pkg),
          sprintf("packageVersion(\"%s\")", pkg)
        ),
        example
      )
      cli::cli_alert_success("Created {.file examples/example.R}")
    }
    # examples/ is not a standard package directory; keep the package's own
    # R CMD check quiet about it.
    rbuildignore <- fs::path(path, ".Rbuildignore")
    if (
      fs::file_exists(rbuildignore) &&
        !"^examples$" %in% readLines(rbuildignore, warn = FALSE)
    ) {
      add_to_rbuildignore(path, "^examples$")
      cli::cli_alert_success("Added {.val ^examples$} to {.file .Rbuildignore}")
    }
    return(invisible())
  }
  if (template != "data-analysis") {
    return(invisible())
  }
  ensure_dir(fs::path(path, "data"))
  ensure_dir(fs::path(path, "scripts"))

  readme <- fs::path(path, "data", "README.md")
  if (!fs::file_exists(readme)) {
    writeLines(
      c(
        "# Data",
        "",
        "Files in this directory are bundled into the site under",
        "`/home/web_user/data/`."
      ),
      readme
    )
  }

  init_script <- fs::path(path, "scripts", "init.R")
  if (!fs::file_exists(init_script)) {
    writeLines(
      c(
        "# Startup script: runs once when the site opens, after the files are in",
        "# place and before anything else runs (repl.startup-script in",
        "# _webrarian.yml). Objects it creates are available in the console.",
        "",
        "message(\"Welcome to the webR environment!\")"
      ),
      init_script
    )
    cli::cli_alert_success("Created {.file scripts/init.R}")
  }
  invisible()
}

#' Add entries to .gitignore
#' @noRd
add_to_gitignore <- function(path, entries) {
  gitignore_path <- fs::path(path, ".gitignore")

  existing <- character()
  if (fs::file_exists(gitignore_path)) {
    existing <- readLines(gitignore_path, warn = FALSE)
  }

  new_entries <- setdiff(entries, existing)

  if (length(new_entries) > 0) {
    all_entries <- c(existing, "", "# webrarian", new_entries)
    writeLines(all_entries, gitignore_path)
  }
  invisible(new_entries)
}

#' Add entries to .Rbuildignore
#' @noRd
add_to_rbuildignore <- function(path, entries) {
  rbuildignore_path <- fs::path(path, ".Rbuildignore")

  if (!fs::file_exists(rbuildignore_path)) {
    return(invisible(character()))
  }

  existing <- readLines(rbuildignore_path, warn = FALSE)
  new_entries <- setdiff(entries, existing)

  if (length(new_entries) > 0) {
    all_entries <- c(existing, new_entries)
    writeLines(all_entries, rbuildignore_path)
  }
  invisible(new_entries)
}

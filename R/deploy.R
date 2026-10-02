# Deployment configuration generation

#' Circulate via GitHub Actions
#'
#' Writes `.github/workflows/deploy-webr.yml`, a GitHub Actions workflow that
#' builds the collection with the webrarian release that generated it and
#' deploys the site to GitHub Pages.
#'
#' @param path The collection, or any directory inside it.
#' @param overwrite If `TRUE`, replace an existing workflow file. Without it,
#'   an existing file is an error and is left as it is.
#'
#' @details
#' The workflow does four things:
#' - Runs on pushes to main/master, pull requests and manual dispatch
#' - Installs the webrarian release that generated it, then runs [bind()]
#'   (local and GitHub packages compile in Docker, which the runner has)
#' - Caches webrarian's downloads between runs
#' - Deploys to GitHub Pages from a separate job that alone may write to
#'   Pages, and never for pull requests
#'
#' GitHub runs only the workflows at the root of a repository, so the file
#' goes in `.github/workflows/` there, even when the collection is in a
#' subdirectory. The workflow then runs its steps in the collection's
#' directory. Outside a git repository it is written in the collection, to be
#' moved to the repository root once there is one.
#'
#' After running this function, push the workflow and set the repository's
#' Pages source to "GitHub Actions".
#'
#' @return Invisibly, a character vector of every path written, which here is
#'   the workflow file.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-github-demo")
#' catalog(collection)
#'
#' circulate_via_github(collection)
#'
#' # Re-running requires an explicit overwrite
#' circulate_via_github(collection, overwrite = TRUE)
#'
#' unlink(collection, recursive = TRUE)
circulate_via_github <- function(path = ".", overwrite = FALSE) {
  check_collection(path)
  root <- collection_root(path)

  # GitHub reads workflows only at the repository root (workflow_file()).
  workflow_path <- workflow_file(root, "deploy-webr.yml")

  if (fs::file_exists(workflow_path) && !overwrite) {
    cli::cli_abort(c(
      "Workflow file already exists: {.path {workflow_path}}",
      "i" = "Use {.code overwrite = TRUE} to replace it"
    ))
  }

  # Rendered before anything is written: a refused name leaves nothing behind.
  workflow_content <- generate_github_actions_workflow(root)
  note_workflow_outside_repository(root)
  ensure_dir(fs::path_dir(workflow_path))
  writeLines(workflow_content, workflow_path)

  cli::cli_alert_success("Created {.path {workflow_path}}")
  cli::cli_text("")
  cli::cli_text("Next steps:")
  cli::cli_bullets(c(
    " " = "Commit and push the workflow file",
    " " = "In the repository's Settings > Pages, set Source to {.val GitHub Actions}",
    "i" = "GitHub Pages cannot set response headers, so the site runs without cross-origin isolation there: Ctrl+C cannot interrupt R."
  ))

  invisible(as.character(workflow_path))
}

#' Circulate via Netlify
#'
#' Writes a `netlify.toml` that names the publish directory, and a GitHub
#' Actions workflow that builds the site and deploys it with the Netlify CLI.
#' Netlify's own build image has no R, so the site is never built on Netlify.
#' Every response header the site needs, cross-origin isolation included, is
#' in the `_headers` file [bind()] writes into the site, so dropping the
#' output directory on Netlify works too.
#'
#' The `netlify.toml` goes next to `_webrarian.yml`. The workflow goes in
#' `.github/workflows/` at the root of the collection's git repository, as
#' for [circulate_via_github()], and runs its steps in the collection's
#' directory.
#'
#' @param path Project path.
#' @param overwrite If `TRUE`, replace existing files.
#'
#' @return Invisibly, the paths of the files written.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-netlify-demo")
#' catalog(collection)
#'
#' circulate_via_netlify(collection)
#'
#' unlink(collection, recursive = TRUE)
circulate_via_netlify <- function(path = ".", overwrite = FALSE) {
  check_collection(path)
  root <- collection_root(path)

  targets <- c(
    fs::path(root, "netlify.toml"),
    workflow_file(root, "netlify-deploy.yml")
  )
  existing <- targets[fs::file_exists(targets)]
  if (length(existing) > 0 && !overwrite) {
    cli::cli_abort(c(
      "{cli::qty(length(existing))}{?A file/Files} already exist{?s/}: {.path {existing}}",
      "i" = "{cli::qty(length(existing))}Use {.code overwrite = TRUE} to replace {?it/them}."
    ))
  }

  written <- generate_netlify_config(root, overwrite = TRUE)
  cli::cli_alert_success("Generated Netlify deployment files")
  # Paths are data: a directory named `site{x}` must not be read as a cli
  # expression, so each is interpolated rather than passed as an item text.
  paths_list <- cli::cli_ul()
  for (w in written) {
    cli::cli_li("{.path {w}}")
  }
  cli::cli_end(paths_list)
  cli::cli_text("")
  cli::cli_bullets(c(
    "i" = "Add repository secrets {.envvar NETLIFY_AUTH_TOKEN} and {.envvar NETLIFY_SITE_ID}; the workflow deploys on every push.",
    "i" = "Or deploy by hand: run {.fn bind} and drop {.path {workflow_output_dir(root)}} on https://app.netlify.com/drop - its {.file _headers} file carries every header the site needs, cross-origin isolation included."
  ))

  invisible(written)
}

#' Generate GitHub Actions workflow content
#' @noRd
generate_github_actions_workflow <- function(root = ".") {
  render_template("deploy-github-pages.yml", workflow_template_values(root))
}

#' Generate Netlify deployment config
#' @noRd
generate_netlify_config <- function(root, overwrite) {
  generated <- character()
  # Checked before anything is written: the names go into a TOML string, a
  # YAML string and a shell command, and workflow_output_dir() and
  # workflow_repository() refuse one that needs quoting.
  output_dir <- workflow_output_dir(root)
  workflow <- workflow_file(root, "netlify-deploy.yml")
  workflow_content <- render_template("deploy-netlify.yml", workflow_template_values(root))

  netlify_toml <- fs::path(root, "netlify.toml")
  if (!fs::file_exists(netlify_toml) || overwrite) {
    writeLines(netlify_toml_content(output_dir), netlify_toml)
    generated <- c(generated, as.character(netlify_toml))
  } else {
    cli::cli_alert_info(
      "Kept existing {.path {netlify_toml}}; use {.code overwrite = TRUE} to replace it."
    )
  }

  if (!fs::file_exists(workflow) || overwrite) {
    note_workflow_outside_repository(root)
    ensure_dir(fs::path_dir(workflow))
    writeLines(workflow_content, workflow)
    generated <- c(generated, as.character(workflow))
  } else {
    cli::cli_alert_info(
      "Kept existing {.path {workflow}}; use {.code overwrite = TRUE} to replace it."
    )
  }

  generated
}

#' Fill a template from inst/templates
#'
#' Replaces `{{key}}` for every name in `data`. GitHub's own `${{ ... }}`
#' expressions contain spaces and are left alone; any `{{lowercase_key}}`
#' still present afterwards is an error, so a template and its caller cannot
#' drift apart silently.
#' @noRd
render_template <- function(name, data) {
  text <- paste(readLines(template_path(name), warn = FALSE), collapse = "\n")
  for (key in names(data)) {
    text <- gsub(paste0("{{", key, "}}"), data[[key]], text, fixed = TRUE)
  }
  left <- regmatches(text, gregexpr("\\{\\{[a-z_]+\\}\\}", text))[[1]]
  if (length(left) > 0) {
    cli::cli_abort("Unfilled placeholder{?s} {.val {left}} in {.file inst/templates/{name}}.")
  }
  text
}

#' The install source a generated workflow uses: this webrarian's release tag
#'
#' A development version (a fourth component) and anything below 0.1.0, the
#' first release, have no tag, so they install the default branch.
#' @noRd
webrarian_install_ref <- function(version = as.character(utils::packageVersion("webrarian"))) {
  v <- package_version(version)
  if (length(unlist(v)) > 3L || v < "0.1.0") {
    "coatless-wasm/webrarian"
  } else {
    sprintf("coatless-wasm/webrarian@v%s", version)
  }
}

#' The configured output directory, relative to the collection root
#' @noRd
output_dir_rel <- function(root) {
  config <- collection_settings(root)
  output <- resolve_output_dir(root, config$build$output_dir)
  as.character(fs::path_rel(output, fs::path_real(root)))
}

#' The output directory as the generated deployment files name it
#'
#' It is spliced into a double-quoted YAML scalar (`path: "..."`), a shell
#' argument (`--dir="..."`) and a TOML string (`publish = "..."`). A `"`, `$`,
#' backtick, `\` or ` #` would break one of them, or change what it means,
#' with no error until the workflow runs, so only characters that need no
#' quoting in any of the three are accepted.
#' @noRd
workflow_output_dir <- function(root) {
  rel <- output_dir_rel(root)
  if (!is_workflow_safe_path(rel)) {
    cli::cli_abort(c(
      "{.field build.output-dir} {.val {rel}} cannot be written into a generated deployment file.",
      "i" = "Use only letters, digits, {.code .}, {.code _}, {.code -} and {.code /}."
    ))
  }
  rel
}

#' Can a relative path go into a generated deployment file unquoted?
#'
#' Letters, digits, `.`, `_`, `-` and `/` only: no quoting is needed in a
#' double-quoted YAML scalar, a shell argument, a TOML string or a
#' `hashFiles()` pattern (no `*`, `?`, `[`, `\`, `'`, or a leading `#` or `!`).
#' @noRd
is_workflow_safe_path <- function(x) {
  grepl("^[A-Za-z0-9._/-]+$", x)
}

#' The git repository a collection's workflows go in, and the collection's
#' directory in it
#'
#' GitHub runs only the workflows in `.github/workflows/` at a repository's
#' root. A collection in a subdirectory of a git repository (a layout
#' netlify_tomls() reads too) therefore has its workflows there, and they run
#' their steps in `workdir`, the collection's path from the repository root
#' (`"."` for a collection at the root). Outside a git repository the
#' collection stands in for the repository root.
#' @return `list(repo, workdir)`.
#' @noRd
workflow_repository <- function(root) {
  repo <- repo_root(root)
  if (is.null(repo)) {
    return(list(repo = fs::path(root), workdir = "."))
  }
  workdir <- as.character(fs::path_rel(fs::path_real(root), repo))
  if (!is_workflow_safe_path(workdir)) {
    cli::cli_abort(c(
      "The collection's directory {.path {workdir}} cannot be written into a generated workflow.",
      "i" = "Its path from the repository root, {.path {repo}}, may use only letters, digits, {.code .}, {.code _}, {.code -} and {.code /}."
    ))
  }
  list(repo = repo, workdir = workdir)
}

#' Where a generated workflow is written: `.github/workflows/<name>` at the
#' collection's repository root (workflow_repository())
#' @noRd
workflow_file <- function(root, name) {
  fs::path(workflow_repository(root)$repo, ".github", "workflows", name)
}

#' Say where a workflow written outside a git repository must go
#' @noRd
note_workflow_outside_repository <- function(root) {
  if (is.null(repo_root(root))) {
    cli::cli_alert_warning(
      "{.path {root}} is not in a git repository, so the workflow is written there. Move {.path .github/workflows/} to the repository root when you create one."
    )
  }
  invisible()
}

#' The values a workflow template is rendered with
#'
#' Run steps start in the collection (`defaults.run.working-directory`), so
#' `bind()` and the Netlify CLI's `--dir` need no prefix. `hashFiles()` and an
#' action's `with:` paths ignore that default and start at the repository
#' root, so the config file and the uploaded site are named from there.
#' @noRd
workflow_template_values <- function(root) {
  output_dir <- workflow_output_dir(root)
  workdir <- workflow_repository(root)$workdir
  from_repo_root <- function(x) {
    if (identical(workdir, ".")) x else paste(workdir, x, sep = "/")
  }
  list(
    workdir = workdir,
    config_path = from_repo_root("_webrarian.yml"),
    output_dir = output_dir,
    site_path = from_repo_root(output_dir),
    install_ref = webrarian_install_ref(),
    webrarian_version = as.character(utils::packageVersion("webrarian"))
  )
}

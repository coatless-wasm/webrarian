# The build output directory
#
# bind() and clean_shelves() delete their output directory, and its path can
# come from a cloned repository's _webrarian.yml. Everything that decides what
# may be deleted lives here: the path must resolve strictly inside the
# collection (after symlinks), must not be git's or webrarian's own directory,
# and an existing non-empty directory must carry the marker a previous bind()
# wrote.

#' Name of the marker file bind() writes into every output directory
#' @noRd
build_marker_name <- function() ".webrarian-build"

#' Resolve symlinks in the longest existing prefix of a path
#'
#' fs::path_real() needs the path to exist, and the output directory usually
#' does not exist yet. So resolve the deepest ancestor that does exist and put
#' the missing tail back on. This is what makes /var/... and /private/var/...
#' (macOS tempdir) compare equal.
#' @noRd
path_real_existing <- function(path) {
  path <- fs::path_norm(fs::path_expand(path))
  tail <- character()
  probe <- path
  while (!fs::dir_exists(probe) && !fs::file_exists(probe)) {
    parent <- fs::path_dir(probe)
    if (identical(as.character(parent), as.character(probe))) {
      break
    }
    tail <- c(fs::path_file(probe), tail)
    probe <- parent
  }
  base <- fs::path_real(probe)
  if (length(tail) == 0L) base else fs::path(base, paste(tail, collapse = "/"))
}

#' Is `path` a strict descendant of `root`? (Both already resolved.)
#' @noRd
path_strictly_inside <- function(path, root) {
  path <- as.character(path)
  root <- sub("/+$", "", as.character(root))
  !identical(path, root) && startsWith(path, paste0(root, "/"))
}

#' Resolve and guard the build output directory
#'
#' @param root Collection root.
#' @param output_dir The configured or requested output directory: relative to
#'   the root, or absolute.
#' @return The absolute, symlink-resolved output path.
#' @noRd
resolve_output_dir <- function(root, output_dir, call = rlang::caller_env()) {
  if (
    !is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(trimws(output_dir))
  ) {
    cli::cli_abort(
      c(
        "{.field build.output-dir} must be a non-empty directory name.",
        "i" = "The default is {.val _site}."
      ),
      call = call
    )
  }
  root_real <- fs::path_real(root)
  candidate <- if (fs::is_absolute_path(output_dir) || startsWith(output_dir, "~")) {
    output_dir
  } else {
    fs::path(root_real, output_dir)
  }
  resolved <- path_real_existing(candidate)
  if (!path_strictly_inside(resolved, root_real)) {
    cli::cli_abort(
      c(
        "Refusing to use {.path {output_dir}} as the build output directory.",
        "x" = "It resolves to {.path {resolved}}, which is not inside the collection at {.path {root_real}}.",
        "i" = "Set {.field build.output-dir} to a subdirectory such as {.val _site}; {.fn bind} deletes and rewrites it."
      ),
      call = call
    )
  }
  top <- strsplit(as.character(fs::path_rel(resolved, root_real)), "/", fixed = TRUE)[[1]][[1]]
  if (top %in% c(".git", ".webrarian")) {
    cli::cli_abort(
      "Refusing to build into {.path {output_dir}}: {.path {top}} belongs to git or webrarian.",
      call = call
    )
  }
  resolved
}

#' Was this directory written by bind()?
#'
#' Sites built before the marker existed are recognized by their shape: an
#' index.html next to the exlibris viewer bundle.
#' @noRd
is_webrarian_output <- function(dir) {
  if (fs::file_exists(fs::path(dir, build_marker_name()))) {
    return(TRUE)
  }
  files <- list.files(dir, all.files = TRUE, no.. = TRUE)
  "index.html" %in% files && any(grepl("^exlibris-r(\\.[0-9a-f]{8})?\\.js$", files))
}

#' May bind() replace this directory?
#' @noRd
check_output_dir_replaceable <- function(output_path, call = rlang::caller_env()) {
  if (!fs::file_exists(output_path)) {
    return(invisible(TRUE))
  }
  if (!fs::dir_exists(output_path)) {
    cli::cli_abort(
      "{.path {output_path}} exists and is a file, not a build directory.",
      call = call
    )
  }
  contents <- list.files(output_path, all.files = TRUE, no.. = TRUE)
  if (length(contents) == 0L || is_webrarian_output(output_path)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "Refusing to replace {.path {output_path}}: it was not created by {.fn webrarian::bind}.",
      "i" = "It holds {length(contents)} entr{?y/ies} and no {.file {build_marker_name()}} marker.",
      "i" = "Choose another {.field build.output-dir}, or move that directory's contents yourself."
    ),
    call = call
  )
}

#' Write the marker that lets a later bind() or clean_shelves() delete `dir`
#' @noRd
write_build_marker <- function(dir, build_id = new_build_id()) {
  marker <- fs::path(dir, build_marker_name())
  jsonlite::write_json(
    list(
      tool = "webrarian",
      version = as.character(utils::packageVersion("webrarian")),
      build_id = build_id,
      built = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    marker,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(marker)
}

# --- staged builds ------------------------------------------------------------
#
# bind() assembles the site in .webrarian/staging-*, which is inside the
# collection (so the final rename stays on one filesystem), excluded from file
# selection, and git-ignored by catalog(). Only when every step succeeded is
# the old output moved aside and the staging directory moved into place. With
# clean = FALSE the build is staged all the same; the previous site's files
# that the build did not write are copied into the staging directory just
# before the swap (carry_over_previous_site()).

#' Start a staged build
#' @noRd
begin_staged_build <- function(root) {
  # No RNG: a package must not advance the user's random seed.
  salt <- substr(rlang::hash(list(Sys.time(), Sys.getpid(), proc.time()[["elapsed"]])), 1L, 8L)
  staging <- fs::path(root, ".webrarian", sprintf("staging-%s-%s", Sys.getpid(), salt))
  fs::dir_create(staging)
  staging
}

#' Swap a finished staging directory into place
#' @noRd
commit_staged_build <- function(staging, output_path) {
  old <- NULL
  if (fs::dir_exists(output_path)) {
    old <- fs::path(fs::path_dir(staging), paste0("old-", fs::path_file(staging)))
    fs::file_move(output_path, old)
  }
  fs::dir_create(fs::path_dir(output_path))
  moved <- tryCatch(
    {
      fs::file_move(staging, output_path)
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(moved)) {
    if (!is.null(old)) {
      fs::file_move(old, output_path)
    }
    cli::cli_abort(c(
      "Could not move the new build into {.path {output_path}}; the previous site is unchanged.",
      "x" = conditionMessage(moved)
    ))
  }
  if (!is.null(old)) {
    fs::dir_delete(old)
  }
  invisible(output_path)
}

#' Remove a staging directory that was not committed
#' @noRd
discard_staged_build <- function(staging) {
  if (fs::dir_exists(staging)) {
    fs::dir_delete(staging)
  }
  invisible(NULL)
}

#' Staging directories left behind by a crashed R session
#' @noRd
staging_leftovers <- function(root) {
  dir <- fs::path(root, ".webrarian")
  if (!fs::dir_exists(dir)) {
    return(character())
  }
  as.character(fs::dir_ls(dir, type = "directory", regexp = "/(old-)?staging-[^/]+$"))
}

#' Paths inside a site that only a build writes
#'
#' Their names change with a content hash, an engine version or the file
#' list, so a previous build's copy under one of them is stale by definition,
#' and bind(clean = FALSE) never carries it over: an old hashed bundle or
#' engine is dead weight, and the vfs-files/ copy of a file just removed from
#' files.include would stay publicly downloadable. pyodidarian keeps the same
#' rule with its own names (build.clean: false keeps unwritten files).
#' `rel` is relative to the site, with forward slashes.
#' @noRd
build_owned_path <- function(rel) {
  rel <- as.character(rel)
  dirs <- c(
    "vfs-files",
    "webr",
    "repo",
    "library",
    "LICENSES",
    "assets/brand",
    "assets/fonts",
    "assets/custom"
  )
  files <- c("index.html", "_headers", "sw.js", "packages.json", build_marker_name())
  under_dir <- Reduce(`|`, lapply(dirs, function(d) startsWith(rel, paste0(d, "/"))), FALSE)
  under_dir |
    rel %in% files |
    grepl("^exlibris-r\\.[^/]+$", rel) |
    # Left by a files-only rebuild in watch mode that crashed.
    grepl("^vfs-files\\.new-[^/]+/", rel) |
    grepl("^\\.index\\.html\\.tmp-[^/]+$", rel)
}

#' Copy the previous site's files that this build did not write
#'
#' For bind(clean = FALSE): every regular file of `previous` that is not
#' build-owned and not already in `staging`. Symbolic links are never
#' followed or copied, so one cannot pull a file from outside the collection
#' into the site.
#' @return Invisibly, the number of files copied.
#' @noRd
carry_over_previous_site <- function(previous, staging) {
  if (!fs::dir_exists(previous)) {
    return(invisible(0L))
  }
  files <- fs::dir_ls(previous, recurse = TRUE, all = TRUE, type = "file")
  rel <- as.character(fs::path_rel(files, previous))
  keep <- rel[!build_owned_path(rel) & !fs::file_exists(fs::path(staging, rel))]
  for (r in keep) {
    dest <- fs::path(staging, r)
    fs::dir_create(fs::path_dir(dest))
    fs::file_copy(fs::path(previous, r), dest)
  }
  invisible(length(keep))
}

#' The nearest directory at or above `path` holding `.git`
#'
#' `.git` is a directory in a clone and a file in a worktree or submodule;
#' either counts. Symlinks in `path` are resolved first.
#' @return The directory, or `NULL` when there is none.
#' @noRd
repo_root <- function(path) {
  dir <- fs::path_real(path)
  repeat {
    if (fs::file_exists(fs::path(dir, ".git"))) {
      return(dir)
    }
    parent <- fs::path_dir(dir)
    if (identical(parent, dir)) {
      return(NULL)
    }
    dir <- parent
  }
}

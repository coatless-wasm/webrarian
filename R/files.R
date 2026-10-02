# File and VFS management

#' Acquire files for the collection
#'
#' Adds files, directories or glob patterns to `files.include` in
#' `_webrarian.yml`. [bind()] copies what they match into the site, under the
#' collection-wide `files.mount-point` (see [settings_set()]), which this
#' function never changes. A path or pattern that matches nothing, or a path outside
#' the collection, is an error.
#'
#' @param paths Character vector of file or directory paths (`data/`) or
#'   glob patterns (`scripts/*.R`), relative to the collection.
#' @param path The collection, or any directory inside it.
#'
#' @return Invisibly returns the updated configuration.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-acquire-files")
#' catalog(collection)
#' dir.create(file.path(collection, "data"))
#' dir.create(file.path(collection, "scripts"))
#' writeLines("x,y\n1,2", file.path(collection, "data", "dataset.csv"))
#' writeLines("summary(1:10)", file.path(collection, "scripts", "analysis.R"))
#'
#' # Add a directory
#' acquire_file("data/", path = collection)
#'
#' # Add a specific file
#' acquire_file("scripts/analysis.R", path = collection)
#'
#' # Add with a glob pattern
#' acquire_file("scripts/*.R", path = collection)
#'
#' collection_files(collection)
#'
#' unlink(collection, recursive = TRUE)
acquire_file <- function(paths, path = ".") {
  check_collection(path)
  root <- collection_root(path)
  root_real <- fs::path_real(root)
  config_path <- fs::path(root, "_webrarian.yml")
  config <- read_config_file(config_path)

  # An absolute path inside the collection is stored relative to it.
  paths <- vapply(
    as.character(paths),
    function(p) {
      q <- gsub("\\\\", "/", p)
      if (fs::is_absolute_path(q) || startsWith(q, "~")) {
        absolute <- path_real_existing(q)
        if (!path_strictly_inside(absolute, root_real)) {
          cli::cli_abort(c(
            "{.path {p}} is outside the collection.",
            "i" = "Files must live under {.path {root}}."
          ))
        }
        q <- as.character(fs::path_rel(absolute, root_real))
        if (fs::dir_exists(absolute)) q <- paste0(q, "/")
      }
      q
    },
    character(1),
    USE.NAMES = FALSE
  )

  matched <- resolve_file_patterns(
    paths,
    character(),
    root,
    output_dir = config$build$output_dir %||% "_site"
  )
  unmatched <- attr(matched, "unmatched")
  if (length(unmatched) > 0) {
    cli::cli_abort(c(
      "{cli::qty(length(unmatched))}Nothing matches {.val {unmatched}}.",
      "i" = "Create the files first; paths are relative to {.path {root}}."
    ))
  }

  existing <- config$files$include %||% list()
  new_paths <- setdiff(paths, unlist(existing))
  if (length(new_paths) == 0) {
    cli::cli_alert_info("All paths already in configuration")
    return(invisible(collection_settings(path)))
  }

  config$files$include <- as.list(c(unlist(existing), new_paths))
  write_config_file(config, config_path)

  cli::cli_alert_success("Added {length(new_paths)} path{?s}: {.path {new_paths}}")
  invisible(collection_settings(path))
}

#' Withdraw files from the collection
#'
#' Removes file paths or patterns from the project configuration.
#'
#' @param paths Character vector of paths to remove from configuration.
#' @param path Project path.
#'
#' @return Invisibly returns the updated configuration.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-withdraw-files")
#' catalog(collection)
#' dir.create(file.path(collection, "data", "temp"), recursive = TRUE)
#' dir.create(file.path(collection, "scripts"))
#' writeLines("x,y\n1,2", file.path(collection, "data", "temp", "scratch.csv"))
#' writeLines("old <- TRUE", file.path(collection, "scripts", "old.R"))
#' acquire_file(c("data/", "scripts/old.R", "data/temp/"), path = collection)
#'
#' withdraw_file("data/", path = collection)
#' withdraw_file(c("scripts/old.R", "data/temp/"), path = collection)
#'
#' collection_files(collection)
#'
#' unlink(collection, recursive = TRUE)
withdraw_file <- function(paths, path = ".") {
  check_collection(path)
  root <- collection_root(path)
  config_path <- fs::path(root, "_webrarian.yml")

  config <- read_config_file(config_path)

  existing <- config$files$include %||% list()
  existing_vec <- unlist(existing)

  to_remove <- intersect(paths, existing_vec)

  if (length(to_remove) == 0) {
    cli::cli_alert_warning("No matching paths found in configuration")
    return(invisible(collection_settings(path)))
  }

  config$files$include <- as.list(setdiff(existing_vec, to_remove))

  write_config_file(config, config_path)

  cli::cli_alert_success(
    "Removed {length(to_remove)} path{?s}: {.path {to_remove}}"
  )

  invisible(collection_settings(path))
}

#' List the files a collection bundles
#'
#' @param path The collection, or any directory inside it.
#'
#' @return A `webrarian_files` list with the configured `include` and
#'   `exclude` patterns, the `mount_point`, and `files`, the paths (relative to
#'   the collection) that [bind()] would bundle. Every element is present, and
#'   empty when nothing is configured.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-files-demo")
#' catalog(collection)
#' dir.create(file.path(collection, "data"))
#' writeLines("x,y\n1,2", file.path(collection, "data", "a.csv"))
#' acquire_file("data/", path = collection)
#'
#' files <- collection_files(collection)
#' files$files
#' files
#'
#' unlink(collection, recursive = TRUE)
collection_files <- function(path = ".") {
  check_collection(path)
  config <- collection_settings(path)
  root <- collection_root(path)
  include <- as.character(unlist(config$files$include))
  files <- if (length(include) > 0L) {
    output_path <- resolve_output_dir(root, config$build$output_dir)
    as.character(select_collection_files(config, root, output_path))
  } else {
    character()
  }
  structure(
    list(
      include = include,
      exclude = as.character(unlist(config$files$exclude)),
      mount_point = config$files$mount_point,
      files = files
    ),
    class = "webrarian_files"
  )
}

#' Convert a glob pattern to regex
#'
#' Translates in a single left-to-right pass so wildcard translations never
#' rewrite each other (the naive multi-`gsub` approach let the trailing `?`
#' substitution clobber the `?` quantifier introduced for `**/`, which made the
#' leading path group mandatory and silently broke root-level and deep matches).
#'
#' Glob semantics: `**/` matches zero or more leading directories, `**` matches
#' across separators, `*` matches within a single path segment, and `?` matches
#' exactly one non-separator character.
#'
#' With `dots = FALSE` (include patterns), a wildcard never supplies a leading
#' dot: a `*`, `?` or `**` that starts a path segment does not match a name
#' that starts with ".", and `**` never crosses such a directory. A dot the
#' pattern spells itself (".Rprofile", ".*", "data/.env") still matches.
#' @noRd
glob_to_regex <- function(glob, dots = TRUE) {
  chars <- strsplit(glob, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- character(0)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    guard <- if (!dots && (i == 1L || chars[i - 1L] == "/")) "(?!\\.)" else ""
    if (ch == "*") {
      if (i < n && chars[i + 1L] == "*") {
        if (i + 2L <= n && chars[i + 2L] == "/") {
          # **/ : zero or more directories
          out <- c(out, if (dots) "(?:.*/)?" else "(?:(?!\\.)[^/]*/)*")
          i <- i + 3L
        } else {
          # ** : across separators
          out <- c(out, if (dots) ".*" else paste0(guard, "(?:[^/]|/(?!\\.))*"))
          i <- i + 2L
        }
      } else {
        out <- c(out, guard, "[^/]*") # * : within one segment
        i <- i + 1L
      }
    } else if (ch == "?") {
      out <- c(out, guard, "[^/]") # ? : one non-separator character
      i <- i + 1L
    } else if (grepl("[[:alnum:]]", ch) || ch %in% c("/", "-", "_")) {
      out <- c(out, ch)
      i <- i + 1L
    } else {
      out <- c(out, "\\", ch) # escape any other regex metacharacter
      i <- i + 1L
    }
  }
  paste0("^", paste(out, collapse = ""), "$")
}

#' Does any segment of a root-relative path start with a dot?
#' @noRd
has_dot_segment <- function(paths) grepl("(^|/)\\.", paths)

#' The files below a directory, leaving out dot names under it
#'
#' `dir` itself may be a dot directory the pattern names (".github/"); only
#' the part of each path below it is checked.
#' @noRd
files_below <- function(files, dir) {
  inside <- files[startsWith(files, paste0(dir, "/"))]
  inside[!has_dot_segment(substring(inside, nchar(dir) + 2L))]
}

#' Expand shell-style braces: "*.{csv,rds}" -> c("*.csv", "*.rds")
#' @noRd
expand_braces <- function(pattern) {
  m <- regexpr("\\{[^{}]*\\}", pattern)
  if (m == -1L) {
    return(pattern)
  }
  len <- attr(m, "match.length")
  inner <- substr(pattern, m + 1L, m + len - 2L)
  before <- substr(pattern, 1L, m - 1L)
  after <- substr(pattern, m + len, nchar(pattern))
  alternatives <- strsplit(inner, ",", fixed = TRUE)[[1]]
  if (length(alternatives) == 0L) {
    alternatives <- ""
  }
  unique(unlist(lapply(paste0(before, alternatives, after), expand_braces), use.names = FALSE))
}

#' Does a pattern use glob syntax?
#' @noRd
has_glob_chars <- function(x) grepl("[*?{]", x)

#' Tidy a user pattern: forward slashes, no leading "./", "." means everything
#' @noRd
normalize_pattern <- function(pattern) {
  p <- gsub("\\\\", "/", trimws(as.character(pattern)))
  p <- sub("^(\\./)+", "", p)
  if (identical(p, ".") || identical(p, "")) {
    p <- "**"
  }
  p
}

#' Does a pattern point outside the collection root?
#' @noRd
pattern_escapes_root <- function(p) {
  grepl("^(/|~|[A-Za-z]:)", p) || any(strsplit(p, "/", fixed = TRUE)[[1]] == "..")
}

#' A gitignore-flavored exclude pattern as a regex over root-relative paths
#'
#' No slash: matches a name at any depth ("*.log", ".DS_Store").
#' Trailing slash: matches a directory and everything below it ("data/raw/").
#' Any other slash: anchored at the collection root ("data/*.log", "/notes.txt").
#' @noRd
exclude_to_regex <- function(pattern) {
  p <- normalize_pattern(pattern)
  dir_only <- endsWith(p, "/")
  p <- sub("/+$", "", p)
  anchored <- grepl("/", p, fixed = TRUE)
  p <- sub("^/", "", p)
  body <- sub("^\\^", "", sub("\\$$", "", glob_to_regex(p)))
  prefix <- if (anchored) "^" else "^(?:.*/)?"
  if (dir_only) paste0(prefix, body, "/.*$") else paste0(prefix, body, "(?:/.*)?$")
}

#' The output directory as a path relative to the collection root
#'
#' `build.output-dir` may be absolute, inside the collection, and
#' collection_files(), the watch manifest, diagnose_config() and acquire_file()
#' pass it as written. The walk's `prune_paths` and the anchored
#' always-excluded pattern need a root-relative path, so an absolute one
#' (spelled through either side of a symlink) is made relative here. One that
#' is not below the root holds no collection files and is dropped (bind()
#' refuses it anyway).
#' @noRd
output_dir_below_root <- function(output_dir, root) {
  if (is.null(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    return(NULL)
  }
  output_dir <- as.character(output_dir)
  if (!fs::is_absolute_path(output_dir) && !startsWith(output_dir, "~")) {
    return(output_dir)
  }
  rel <- as.character(fs::path_rel(path_real_existing(output_dir), fs::path_real(root)))
  if (identical(rel, ".") || identical(rel, "..") || startsWith(rel, "../")) {
    return(NULL)
  }
  rel
}

#' Paths no collection ever bundles
#' @noRd
always_excluded_patterns <- function(output_dir = NULL) {
  c(
    if (!is.null(output_dir) && nzchar(output_dir)) {
      paste0("/", sub("/+$", "", gsub("\\\\", "/", output_dir)), "/")
    },
    "/.webrarian/",
    ".git/",
    "/.Rproj.user/",
    "/renv/library/",
    "/renv/staging/",
    "node_modules/",
    "/_webrarian.yml"
  )
}

#' Every file under the root, as root-relative paths, skipping heavy directories
#'
#' Breadth-first, sorted within each directory. Never descends into a
#' directory named in `prune` (at any depth), a root-relative directory in
#' `prune_paths` (renv's library and staging area, the output directory), or a
#' symlinked directory (it can loop, or lead outside the collection). Each
#' directory's files are collected as one vector in a growing list, so the
#' walk stays linear in the number of files: the watch loop runs it every poll.
#' @noRd
list_collection_files <- function(
  root,
  prune = c(".git", ".webrarian", ".Rproj.user", "node_modules"),
  prune_paths = c("renv/library", "renv/staging")
) {
  prune_paths <- sub("/+$", "", sub("^(\\./)+", "", gsub("\\\\", "/", as.character(prune_paths))))
  found <- list()
  queue <- list("")
  head <- 1L
  while (head <= length(queue)) {
    rel <- queue[[head]]
    head <- head + 1L
    here <- if (nzchar(rel)) file.path(root, rel) else root
    entries <- sort(list.files(here, all.files = TRUE, no.. = TRUE))
    if (length(entries) == 0L) {
      next
    }
    children <- if (nzchar(rel)) paste0(rel, "/", entries) else entries
    full <- file.path(root, children)
    is_dir <- dir.exists(full)
    if (any(!is_dir)) {
      found[[length(found) + 1L]] <- children[!is_dir]
    }
    descend <- is_dir & !(entries %in% prune) & !(children %in% prune_paths) & !fs::is_link(full)
    for (child in children[descend]) {
      queue[[length(queue) + 1L]] <- child
    }
  }
  as.character(unlist(found, use.names = FALSE))
}

#' Resolve include/exclude patterns to the files a collection bundles
#'
#' A name that starts with a dot (`.Renviron`, `.env`, `.RData`) is selected
#' only by an include pattern that spells that dot: `"."`, `"**"`, a
#' directory include and wildcards skip it, so a broad include cannot publish
#' secrets (pyodidarian applies the same rule). Excludes are plain gitignore.
#'
#' @param include,exclude Patterns from `files.include` / `files.exclude`.
#' @param root Collection root.
#' @param output_dir The output directory, relative to the root or absolute
#'   (as `build.output-dir` may be written); always excluded.
#' @return Root-relative paths (files only), in include order, with attribute
#'   `"unmatched"`: the include patterns that matched nothing.
#' @noRd
resolve_file_patterns <- function(
  include,
  exclude,
  root,
  output_dir = NULL,
  call = rlang::caller_env()
) {
  include <- as.character(unlist(include))
  exclude <- as.character(unlist(exclude))
  if (length(include) == 0L) {
    return(structure(character(), unmatched = character()))
  }
  output_dir <- output_dir_below_root(output_dir, root)

  all_files <- list_collection_files(
    root,
    prune_paths = c("renv/library", "renv/staging", if (!is.null(output_dir)) output_dir)
  )
  selected <- character()
  unmatched <- character()
  for (raw in include) {
    p <- normalize_pattern(raw)
    if (pattern_escapes_root(p)) {
      cli::cli_abort(
        c(
          "Include pattern {.val {raw}} points outside the collection.",
          "i" = "Patterns are relative to the collection root and may not use {.code ..} or absolute paths."
        ),
        call = call
      )
    }
    hits <- if (identical(p, "**")) {
      all_files[!has_dot_segment(all_files)]
    } else if (endsWith(p, "/")) {
      dir <- sub("/+$", "", p)
      if (has_glob_chars(dir)) {
        rx <- paste0(sub("\\$$", "", glob_to_regex(dir, dots = FALSE)), "(?:/(?!\\.)[^/]+)+$")
        all_files[grepl(rx, all_files, perl = TRUE)]
      } else {
        files_below(all_files, dir)
      }
    } else if (has_glob_chars(p)) {
      rx <- vapply(expand_braces(p), glob_to_regex, character(1), dots = FALSE)
      all_files[Reduce(`|`, lapply(rx, grepl, x = all_files, perl = TRUE))]
    } else if (p %in% all_files) {
      p
    } else {
      files_below(all_files, p)
    }
    if (length(hits) == 0L) {
      unmatched <- c(unmatched, raw)
    }
    selected <- c(selected, setdiff(hits, selected))
  }

  for (pattern in c(exclude, always_excluded_patterns(output_dir))) {
    rx <- vapply(expand_braces(pattern), exclude_to_regex, character(1))
    drop <- Reduce(`|`, lapply(rx, grepl, x = selected, perl = TRUE))
    selected <- selected[!drop]
  }

  structure(selected, unmatched = unmatched)
}

#' Drop symlinked files that lead outside the collection
#'
#' The walk never descends into a symlinked directory, but a symlinked file is
#' listed like any other. One that resolves outside the collection (a link to
#' ~/.Renviron, say) or to nothing would publish a file the author never put
#' in the collection, so it is dropped with one warning. Links that stay
#' inside are kept.
#' @noRd
drop_escaping_links <- function(files, root) {
  if (length(files) == 0L) {
    return(files)
  }
  full <- file.path(root, files)
  is_link <- fs::is_link(full)
  if (!any(is_link)) {
    return(files)
  }
  real_root <- as.character(fs::path_real(root))
  escapes <- vapply(
    full[is_link],
    function(p) {
      target <- tryCatch(as.character(fs::path_real(p)), error = function(e) NA_character_)
      is.na(target) || !path_strictly_inside(target, real_root)
    },
    logical(1),
    USE.NAMES = FALSE
  )
  bad <- files[is_link][escapes]
  if (length(bad) > 0L) {
    cli::cli_warn(c(
      "Skipping {length(bad)} symbolic link{?s} that lead{?s/} outside the collection or to nothing: {.file {bad}}.",
      "i" = "Copy the file into the collection if it should be published."
    ))
  }
  files[!files %in% bad]
}

#' The files bind() bundles, warning about include patterns that match nothing
#' @noRd
select_collection_files <- function(config, root, output_path) {
  output_rel <- as.character(fs::path_rel(path_real_existing(output_path), fs::path_real(root)))
  files <- resolve_file_patterns(
    config$files$include,
    config$files$exclude,
    root,
    output_dir = output_rel
  )
  unmatched <- attr(files, "unmatched")
  if (length(unmatched) > 0L) {
    cli::cli_warn(c(
      "{cli::qty(length(unmatched))}Include pattern{?s} {.val {unmatched}} matched no files.",
      "i" = "Check {.field files.include} in {.file _webrarian.yml}."
    ))
  }
  drop_escaping_links(as.character(files), root)
}

#' Print method for webrarian_files
#'
#' @param x A `webrarian_files` object from [collection_files()].
#' @param ... Additional arguments passed to methods (ignored).
#' @return `x`, invisibly.
#' @export
print.webrarian_files <- function(x, ...) {
  cli::cli_h1("Collection files")
  show <- function(label, values) {
    if (length(values) == 0L) {
      cli::cli_text("{label}: (none)")
    } else {
      cli::cli_text("{label}: {.val {values}}")
    }
  }
  show("Include", x$include)
  show("Exclude", x$exclude)
  cli::cli_text("Mount point: {.path {x$mount_point}}")

  n <- length(x$files)
  cli::cli_h2("{n} file{?s} bundled")
  if (n > 0L) {
    # Each name goes in as data, never as a cli template: cli_ul(x$files)
    # would evaluate the {x} of a file called data/{x}.csv.
    ul <- cli::cli_ul()
    for (f in utils::head(x$files, 20L)) {
      cli::cli_li("{.file {f}}")
    }
    cli::cli_end(ul)
    if (n > 20L) {
      cli::cli_text("... and {n - 20L} more")
    }
  }
  invisible(x)
}

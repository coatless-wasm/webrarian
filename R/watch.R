# Watch mode: rebuild on change, preview while watching

#' The watch loop behind reading_room(watch = TRUE)
#'
#' Builds the collection once, serves its site, then polls for changes: a
#' change to the included files only updates vfs-files/ and the page's file
#' list; any other change runs a full bind(). Between polls it answers the
#' page's reload checks (watch_serve_for()), whose build id is the count of
#' successful rebuilds it records after every poll. Runs until interrupted.
#' @return The stopped server, after an interrupt.
#' @noRd
watch_collection <- function(
  root,
  port = NULL,
  open_browser = FALSE,
  poll_interval = watch_poll_interval(),
  debounce_delay = 0.5
) {
  # bind()'s "Preview: reading_room()" hint is wrong here: the console is busy.
  webrarian_env$in_watch <- TRUE
  on.exit(webrarian_env$in_watch <- FALSE, add = TRUE)

  config <- collection_settings(root)
  output_path <- resolve_output_dir(root, config$build$output_dir)
  cli::cli_h1("Reading room: watching {.val {config$project$name}}")
  cli::cli_text("Watching: {.path {root}}")
  cli::cli_text("Output: {.path {output_path}}")
  cli::cli_alert_info("Press Ctrl+C to stop watching")

  cli::cli_h2("Initial build")
  tryCatch(
    bind(root),
    error = function(e) {
      cli::cli_alert_danger("Initial build failed: {conditionMessage(e)}")
    }
  )
  if (!fs::file_exists(fs::path(output_path, "index.html"))) {
    cli::cli_abort(c(
      "There is no site to preview yet.",
      "i" = "Fix the error above and call {.fn reading_room} again."
    ))
  }

  site <- as.character(fs::path_real(output_path))
  server <- serve_site(site, port, watch = TRUE)
  # The page's build id: successful rebuilds this session, from 0.
  watch_record_builds(site, 0L)
  on.exit(watch_record_builds(site, NULL), add = TRUE)
  cli::cli_alert_info("The page reloads itself after each successful rebuild")
  if (open_browser) {
    open_in_browser(server$url)
  }

  state <- new_watch_state(root, server_url = server$url)
  tryCatch(
    repeat {
      watch_serve_for(poll_interval)
      state <- watch_step(state, Sys.time(), debounce_delay)
      watch_record_builds(site, state$builds)
    },
    interrupt = function(cnd) {
      cli::cli_text("")
      cli::cli_alert_info("Stopping watch mode...")
    },
    finally = {
      if (stop_static_server(server)) {
        cli::cli_alert_info("Stopped the preview server")
      }
    }
  )
  server
}

#' Seconds between two polls of the watch loop (a function so tests can
#' shorten it)
#' @noRd
watch_poll_interval <- function() 1

#' Answer R-side preview requests for `seconds`
#'
#' In watch mode the page and its build id are answered by R (preview_app()),
#' which happens only while R runs httpuv's event loop: this waits in it.
#' @noRd
watch_serve_for <- function(seconds) {
  until <- Sys.time() + seconds
  repeat {
    left <- as.numeric(difftime(until, Sys.time(), units = "secs"))
    if (left <= 0) {
      break
    }
    httpuv::service(min(100, ceiling(left * 1000)))
  }
  invisible()
}

#' The state watch_step() carries from one poll to the next
#' @noRd
new_watch_state <- function(path, server_url = NULL) {
  list(
    path = path,
    root = collection_root(path),
    server_url = server_url,
    signature = NULL,
    changed_at = NULL,
    pending = FALSE,
    failed_signature = NULL,
    last_error = NULL,
    warned_config_hash = NULL,
    builds = 0L
  )
}

#' One poll of the watch loop
#'
#' The first poll that sees a change set arms the debounce; a later poll with
#' the same change set, once `debounce_delay` has passed, rebuilds. A failed
#' rebuild is remembered and not retried until the change set differs.
#' @return The updated state.
#' @noRd
watch_step <- function(
  state,
  now = Sys.time(),
  debounce_delay = 0.5,
  on_error = "continue",
  verbose = FALSE
) {
  # Unknown keys warn every time the file is read, and this runs
  # every poll: collect the warnings and show them once per version of the file.
  warnings <- character()
  config <- tryCatch(
    withCallingHandlers(
      collection_settings(state$path),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(config, "error")) {
    message <- conditionMessage(config)
    if (!identical(message, state$last_error)) {
      cli::cli_alert_danger("Config error: {message}")
      state$last_error <- message
    }
    return(state)
  }
  state$last_error <- NULL
  config_hash <- unname(tools::md5sum(fs::path(state$root, "_webrarian.yml")))
  if (!identical(config_hash, state$warned_config_hash)) {
    for (w in unique(warnings)) {
      cli::cli_alert_warning("{w}")
    }
    state$warned_config_hash <- config_hash
  }

  changes <- detect_changes(config, state$root)
  if (!changes$any_changed) {
    state$pending <- FALSE
    return(state)
  }
  signature <- change_signature(changes)
  if (identical(signature, state$failed_signature)) {
    return(state)
  }
  if (!identical(signature, state$signature)) {
    state$signature <- signature
    state$changed_at <- now
    state$pending <- TRUE
    if (verbose) {
      cli::cli_alert_info("Detected changes:")
      if (changes$config_changed) {
        cli::cli_bullets(c("*" = "Configuration file changed"))
      }
      for (f in changes$files_changed) {
        cli::cli_bullets(c("*" = "File changed: {.file {f}}"))
      }
    }
    return(state)
  }
  if (
    !isTRUE(state$pending) ||
      as.numeric(difftime(now, state$changed_at, units = "secs")) < debounce_delay
  ) {
    return(state)
  }

  state$pending <- FALSE
  ok <- tryCatch(
    {
      if (changes$config_changed) {
        cli::cli_alert_info("Config changed - full rebuild")
        bind(state$path)
      } else {
        cli::cli_alert_info("Files changed - updating the site")
        rebuild_files_only(state$path)
      }
      # Record what this poll saw, not what is on disk after the rebuild: an edit
      # saved while it ran is still a change on the next poll. This also replaces
      # the manifest bind() saved.
      save_build_manifest(changes$manifest, state$root)
      cli::cli_alert_success("Rebuild complete")
      TRUE
    },
    error = function(e) {
      if (identical(on_error, "stop")) {
        stop(e)
      }
      cli::cli_alert_danger("Build failed: {conditionMessage(e)}")
      cli::cli_alert_info("Waiting for the next change...")
      FALSE
    }
  )
  state$failed_signature <- if (ok) NULL else signature
  if (ok) {
    # The page polls this count and reloads when it changes (watch_build_id()).
    state$builds <- (state$builds %||% 0L) + 1L
  }
  state$signature <- NULL
  state
}

#' Update a built site after included files changed
#'
#' Rewrites vfs-files/ from the current selection (so deleted files go too)
#' and replaces the page's inline config with a fresh file list and repl
#' entries, keeping the engine, package sources and offline flag bind() wrote.
#' The new page and the new vfs-files/ are prepared beside the live ones and
#' swapped in only when both are complete, so a rebuild that fails part-way
#' (a file deleted mid-copy, a permission error) leaves the last good site
#' being served, as reading_room(watch = TRUE) promises.
#' @noRd
rebuild_files_only <- function(path) {
  config <- collection_settings(path)
  root <- collection_root(path)
  output_path <- resolve_output_dir(root, config$build$output_dir)
  index_path <- fs::path(output_path, "index.html")
  html <- if (fs::file_exists(index_path)) {
    paste(readLines(index_path, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  wire <- read_inline_viewer_config(html)
  if (is.null(wire)) {
    return(invisible(bind(path)))
  }

  files <- select_collection_files(config, root, output_path)
  repl_files <- resolve_repl_files(config, files)

  # The package sources and the offline flag are bind()'s; carry them over
  # exactly (an offline site with no packages has no repo-url at all).
  fresh <- build_viewer_config(
    config,
    files,
    wire[["engine-base-url"]],
    packages = list(
      install = unlist(wire$packages$install),
      repos = unlist(wire$packages$repos),
      repo_url = wire$packages[["repo-url"]],
      # The image bind() wrote (build.library-image) is unchanged by a file-only rebuild.
      library_images = unlist(wire$packages[["library-images"]])
    ),
    repl_files = repl_files,
    offline = isTRUE(wire$offline)
  )

  # Nothing the preview serves changes until the new page and the new files
  # are both complete: on an error, on.exit() removes what was prepared and
  # the last good site stays as it was.
  page_tmp <- fs::path(output_path, sprintf(".index.html.tmp-%s", Sys.getpid()))
  staging <- fs::path(output_path, sprintf("vfs-files.new-%s", Sys.getpid()))
  on.exit(
    {
      if (fs::file_exists(page_tmp)) {
        fs::file_delete(page_tmp)
      }
      discard_staged_build(staging)
    },
    add = TRUE
  )
  writeLines(replace_inline_viewer_config(html, fresh), page_tmp)
  discard_staged_build(staging)
  build_files_vfs(files, root, staging)

  # Swap: commit_staged_build() moves the old vfs-files/ aside, renames the
  # new one into place and deletes the old one (putting it back if the rename
  # fails). With nothing selected any more there is no new directory, and the
  # old one goes.
  staged <- fs::path(staging, "vfs-files")
  vfs_dir <- fs::path(output_path, "vfs-files")
  if (fs::dir_exists(staged)) {
    commit_staged_build(staged, vfs_dir)
  } else if (fs::dir_exists(vfs_dir)) {
    fs::dir_delete(vfs_dir)
  }
  fs::file_move(page_tmp, index_path)
  invisible(output_path)
}

# ============================================================================
# Build Manifest
# ============================================================================

#' Create a build manifest for change detection
#' @noRd
create_build_manifest <- function(config, root) {
  config_file <- fs::path(root, "_webrarian.yml")
  config_hash <- if (fs::file_exists(config_file)) {
    as.character(tools::md5sum(config_file))
  } else {
    NA_character_
  }

  packages <- list(
    prebuilt = unlist(config$packages$prebuilt) %||% character(),
    github = unlist(config$packages$github) %||% character(),
    local = unlist(config$packages$local) %||% character()
  )

  include_patterns <- unlist(config$files$include) %||% character()
  exclude_patterns <- unlist(config$files$exclude) %||% character()
  file_hashes <- list()
  if (length(include_patterns) > 0) {
    files <- as.character(resolve_file_patterns(
      include_patterns,
      exclude_patterns,
      root,
      output_dir = config$build$output_dir
    ))
    for (f in files) {
      full_path <- fs::path(root, f)
      if (fs::file_exists(full_path)) {
        file_hashes[[f]] <- as.character(tools::md5sum(full_path))
      }
    }
  }

  list(
    version = "1.0",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    config_hash = config_hash,
    packages = packages,
    file_hashes = file_hashes
  )
}

#' Save build manifest
#' @noRd
save_build_manifest <- function(manifest, root) {
  manifest_dir <- fs::path(root, ".webrarian")
  ensure_dir(manifest_dir)
  manifest_file <- fs::path(manifest_dir, "build-manifest.json")
  jsonlite::write_json(manifest, manifest_file, auto_unbox = TRUE, pretty = TRUE)
}

#' Load build manifest
#' @noRd
load_build_manifest <- function(root) {
  manifest_file <- fs::path(root, ".webrarian", "build-manifest.json")
  if (!fs::file_exists(manifest_file)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(manifest_file), error = function(e) NULL)
}

# ============================================================================
# Change Detection
# ============================================================================

#' Stable identity for a detected change set
#'
#' Two polls that observe the same pending change produce identical
#' signatures, so the debounce restarts only when something new changes. The
#' new config hash and the changed files' new hashes are part of it, so a
#' second, different edit of the same file is a new change set.
#' @noRd
change_signature <- function(changes) {
  hashes <- changes$file_hashes %||% list()
  files <- as.character(changes$files_changed)
  file_ids <- vapply(
    files,
    function(f) paste0(f, "@", hashes[[f]] %||% ""),
    character(1),
    USE.NAMES = FALSE
  )
  list(
    config = isTRUE(changes$config_changed),
    config_hash = changes$config_hash %||% NA_character_,
    packages = isTRUE(changes$packages_changed),
    files = sort(file_ids)
  )
}

#' Detect changes since last build
#' @noRd
detect_changes <- function(config, root) {
  old_manifest <- load_build_manifest(root)
  new_manifest <- create_build_manifest(config, root)

  if (is.null(old_manifest)) {
    return(list(
      any_changed = TRUE,
      config_changed = TRUE,
      packages_changed = TRUE,
      files_changed = character(),
      config_hash = new_manifest$config_hash,
      file_hashes = list(),
      manifest = new_manifest
    ))
  }

  config_changed <- is.na(old_manifest$config_hash) ||
    is.na(new_manifest$config_hash) ||
    old_manifest$config_hash != new_manifest$config_hash

  normalize_pkgs <- function(pkgs) {
    list(
      prebuilt = as.character(unlist(pkgs$prebuilt) %||% character()),
      github = as.character(unlist(pkgs$github) %||% character()),
      local = as.character(unlist(pkgs$local) %||% character())
    )
  }
  packages_changed <- !identical(
    normalize_pkgs(old_manifest$packages),
    normalize_pkgs(new_manifest$packages)
  )

  old_hashes <- old_manifest$file_hashes %||% list()
  new_hashes <- new_manifest$file_hashes %||% list()
  files_changed <- character()
  for (f in names(new_hashes)) {
    if (is.null(old_hashes[[f]]) || old_hashes[[f]] != new_hashes[[f]]) {
      files_changed <- c(files_changed, f)
    }
  }
  for (f in names(old_hashes)) {
    if (is.null(new_hashes[[f]])) {
      files_changed <- c(files_changed, f)
    }
  }

  list(
    any_changed = config_changed || packages_changed || length(files_changed) > 0,
    config_changed = config_changed,
    packages_changed = packages_changed,
    files_changed = files_changed,
    config_hash = new_manifest$config_hash,
    file_hashes = new_hashes[intersect(files_changed, names(new_hashes))],
    manifest = new_manifest
  )
}

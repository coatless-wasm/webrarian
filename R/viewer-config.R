# ViewerConfig: the wire object exlibris reads
#
# build_viewer_config() turns webrarian's settings (snake_case, see R/config.R)
# plus what bind() learned while building (which files were bundled, where the
# engine lives, which packages to install) into the object exlibris reads from
# window.__VIEWER_CONFIG__. Its keys are exactly the ones the vendored schema
# declares (inst/viewer-config.schema.json, copied from exlibris by
# tools/vendor-exlibris.sh); tests/testthat/test-config-contract.R validates
# real output against it.
#
#   setting                        wire key
#   -----------------------------  ----------------------------------------
#   webr.version                   engine-version
#   (bind: engine location)        engine-base-url
#   project.name                   project-name
#   (bind: packages)               packages.install / repos / repo-url
#   (bind: build.library-image)    packages.library-images (only when an image was written)
#   (bind: selected files)         files[]{name, vfs-path, fetch-path}
#   files.mount-point              mount-point
#   repl.panels.*                  panels.*
#   repl.auto-open                 auto-open (VFS paths; unset: the first R
#                                  file that is not the startup script)
#   repl.auto-run                  auto-run (VFS paths)
#   repl.startup-script            startup-script (a VFS path, or null)
#   repl.share-links               share-links (open | fixed | off)
#   build.offline                  offline (written only when true)
#   repl.persist-edits             persist-edits (always written)
#
# jsonlite::toJSON(auto_unbox = TRUE) collapses a length-1 vector to a scalar,
# so every field that must stay a JSON array is built as a list.

#' The public webR package repository
#' @noRd
default_repo_url <- function() "https://repo.r-wasm.org"

#' Where the page installs packages from
#'
#' The order is the parent spec's ("Offline sites and share links"): the
#' site's own repo/ when it bundles packages; then, unless the site is
#' offline, repo.r-wasm.org and `packages.repos`. webr::install() takes each
#' package from the first source that has it, and exlibris's webR driver sets
#' `webr_pkg_repos` to the same list, so boot installs, the Packages tab, a
#' share link's `?packages=` and the console's `install.packages()` all search
#' the same places.
#'
#' * A site with a bundled `repo/` puts it first, so the bundled (or compiled
#'   development) version is the one that installs.
#' * An offline site searches nothing else: it makes no request to another
#'   origin, and visitors can add only bundled packages. One that bundles no
#'   packages has no source at all (`repo_url = NULL`).
#' @param offline The build's resolved `build.offline`.
#' @param has_local_repo Whether the site ships a `repo/` with packages in it.
#' @param config_repos `packages.repos`.
#' @return `list(repo_url, repos)`, page-relative where local;
#'   viewer_url_resolver_script() makes them absolute in the browser.
#' @noRd
viewer_package_repos <- function(offline, has_local_repo, config_repos = character()) {
  config_repos <- as.character(unlist(config_repos))
  public <- if (isTRUE(offline)) character() else unique(c(default_repo_url(), config_repos))
  if (isTRUE(has_local_repo)) {
    return(list(repo_url = "./repo", repos = public))
  }
  list(repo_url = if (length(public) > 0L) public[[1]], repos = public[-1])
}

#' The inline script that makes the URLs webR resolves absolute
#'
#' webR resolves `engine-base-url` and the package repositories inside its
#' worker: from webr/v<version>/webr-worker.js for a bundled engine, or from a
#' blob: URL when the engine comes from the CDN. A page-relative URL such as
#' `./repo` would resolve against the worker, not the page. This classic
#' script runs after the inline config and before the viewer's module script
#' (module scripts are deferred) and rewrites every relative URL against
#' `document.baseURI`, which also covers a site served from a sub-path such as
#' a GitHub Pages project site. URLs with a scheme are left exactly as they
#' are. The config in the HTML stays page-relative, so the site can move.
#' @return HTML `<script>` string.
#' @noRd
viewer_url_resolver_script <- function() {
  '<script>
  (function () {
    // webR resolves its engine and package repository URLs inside its worker
    // (served from webr/v<version>/, or a blob: URL when the engine comes from
    // the CDN), never against this page. Make every relative one absolute
    // before the viewer reads the config.
    var config = window.__VIEWER_CONFIG__;
    if (!config) return;
    function absolute(url) {
      if (typeof url !== "string" || url === "" || /^[a-z][a-z0-9+.-]*:/i.test(url)) return url;
      return new URL(url, document.baseURI).href;
    }
    if (config["engine-base-url"]) config["engine-base-url"] = absolute(config["engine-base-url"]);
    var packages = config.packages;
    if (packages) {
      if (packages["repo-url"]) packages["repo-url"] = absolute(packages["repo-url"]);
      if (Array.isArray(packages.repos)) packages.repos = packages.repos.map(absolute);
    }
  })();
  </script>'
}

#' Coerce to a plain list of strings, which always serializes as a JSON array
#' @noRd
as_json_string_array <- function(x) {
  as.list(as.character(unlist(x, use.names = FALSE)))
}

#' Percent-encode each segment of a site-relative path
#'
#' The viewer fetches bundled files by URL, so a name containing `#`, `?`, `%`
#' or a space must be encoded or the browser requests a different URL.
#' `repeated = TRUE` matters: by default URLencode() returns a string that
#' already contains a `%XX` sequence unchanged, so `pct%41.csv` would be
#' fetched as `pctA.csv`. File names are never pre-encoded, so every `%` is
#' encoded.
#' @noRd
encode_url_path <- function(path) {
  vapply(
    as.character(path),
    function(p) {
      segments <- strsplit(p, "/", fixed = TRUE)[[1]]
      paste(
        vapply(segments, utils::URLencode, character(1), reserved = TRUE, repeated = TRUE),
        collapse = "/"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' VFS paths for root-relative files (none for none: paste0() would recycle)
#' @noRd
vfs_paths <- function(mount, rel) {
  if (length(rel) == 0L) {
    return(character())
  }
  paste0(sub("/+$", "", mount), "/", rel)
}

#' Resolve one auto-open / auto-run / startup-script entry to a bundled file
#'
#' Entries are paths relative to the collection root. A bare file name is
#' accepted when exactly one bundled file has that name.
#' @noRd
resolve_repl_entry <- function(entry, files, key, call = rlang::caller_env()) {
  entry <- sub("^(\\./)+", "", gsub("\\\\", "/", as.character(entry)))
  if (entry %in% files) {
    return(entry)
  }
  hits <- files[basename(files) == entry]
  if (length(hits) == 1L) {
    return(hits)
  }
  if (length(hits) > 1L) {
    cli::cli_abort(
      c(
        "{.field {key}} entry {.val {entry}} matches {length(hits)} bundled files: {.file {hits}}.",
        "i" = "Write the path relative to the collection root instead."
      ),
      call = call
    )
  }
  cli::cli_abort(
    c(
      "{.field {key}} entry {.val {entry}} is not one of the bundled files.",
      "i" = "Add it to {.field files.include}, or remove it from {.field {key}}."
    ),
    call = call
  )
}

#' @noRd
resolve_repl_entries <- function(entries, files, key, call = rlang::caller_env()) {
  entries <- as.character(unlist(entries, use.names = FALSE))
  unique(vapply(
    entries,
    resolve_repl_entry,
    character(1),
    files = files,
    key = key,
    call = call,
    USE.NAMES = FALSE
  ))
}

#' Resolve the repl settings that name files
#'
#' @param files Root-relative paths of the bundled files, in bundle order.
#' @return `list(auto_open, auto_run, startup_script)` of root-relative paths.
#' @noRd
resolve_repl_files <- function(config, files, call = rlang::caller_env()) {
  repl <- config$repl %||% list()
  files <- as.character(files)

  startup <- repl$startup_script
  startup <- if (is.null(startup) || !nzchar(startup)) {
    NULL
  } else {
    resolve_repl_entry(startup, files, "repl.startup-script", call)
  }

  # Unset: the first bundled R file that is not the startup script, which has
  # already run by the time files open (setdiff() with NULL drops nothing).
  auto_open <- if (is.null(repl$auto_open)) {
    utils::head(setdiff(files[grepl("\\.[Rr]$", files)], startup), 1L)
  } else {
    resolve_repl_entries(repl$auto_open, files, "repl.auto-open", call)
  }

  list(
    auto_open = auto_open,
    auto_run = resolve_repl_entries(repl$auto_run, files, "repl.auto-run", call),
    startup_script = startup
  )
}

#' Build the wire ViewerConfig
#'
#' `packages$repo_url = NULL` means the site has no package source at all (an
#' offline site that bundles none): `repo-url` is then left out.
#'
#' @param config Settings from `collection_settings()` (defaults applied).
#' @param files Root-relative paths of the bundled files.
#' @param engine_base_url Where the page loads the webR engine from.
#' @param packages `list(install, repos, repo_url, library_images)`; `library_images`
#'   (page-relative URLs of package library images) is optional.
#' @param repl_files `resolve_repl_files()`'s result.
#' @param offline The build's resolved `build.offline`: `TRUE` writes
#'   `"offline": true`; the key is absent otherwise, which exlibris reads as
#'   `false`.
#' @noRd
build_viewer_config <- function(
  config,
  files,
  engine_base_url,
  packages,
  repl_files = resolve_repl_files(config, files),
  offline = FALSE
) {
  files <- as.character(files)
  mount <- config$files$mount_point %||% "/home/web_user"
  panels <- config$repl$panels %||% list()
  panel <- function(name) !isFALSE(panels[[name]])

  wire <- list(
    `schema-version` = 1L,
    engine = "webr",
    `engine-version` = as.character(config$webr$version),
    `engine-base-url` = engine_base_url,
    `project-name` = as.character(config$project$name %||% "webR"),
    packages = c(
      list(
        install = as_json_string_array(packages$install),
        repos = as_json_string_array(packages$repos)
      ),
      if (!is.null(packages$repo_url)) list(`repo-url` = packages$repo_url),
      # Named only when the site ships a library image (bind(), build.library-image).
      if (length(packages$library_images) > 0L) {
        list(`library-images` = as_json_string_array(packages$library_images))
      }
    ),
    files = lapply(files, function(f) {
      list(
        # The path relative to the mount point, not the basename: two bundled
        # files with one name (a/setup.R, b/setup.R) stay distinct.
        name = f,
        `vfs-path` = vfs_paths(mount, f),
        `fetch-path` = paste0("vfs-files/", encode_url_path(f))
      )
    }),
    `mount-point` = mount,
    panels = list(
      editor = panel("editor"),
      terminal = panel("terminal"),
      files = panel("files"),
      plot = panel("plot"),
      environment = panel("environment")
    ),
    `auto-open` = as_json_string_array(vfs_paths(mount, repl_files$auto_open)),
    `auto-run` = as_json_string_array(vfs_paths(mount, repl_files$auto_run)),
    `startup-script` = if (is.null(repl_files$startup_script)) {
      NULL
    } else {
      vfs_paths(mount, repl_files$startup_script)
    },
    `share-links` = config$repl$share_links %||% "open",
    # The viewer keeps visitors' editor files in their browser unless this is false.
    `persist-edits` = !isFALSE(config$repl$persist_edits)
  )
  # Absent, the viewer leaves the color scheme to each visitor and shows its
  # Settings gear; light or dark pins the site and hides the gear.
  theme <- site_theme(config)
  if (!identical(theme, "auto")) {
    wire$theme <- theme
  }
  if (isTRUE(offline)) {
    wire$offline <- TRUE
  }
  wire
}

#' The scheme a site is pinned to, or "auto"
#'
#' `ui.theme` says whether each visitor chooses (auto) or the site is light or
#' dark for everyone. A brand whose palette is dark carries one palette for
#' both schemes (see brand_root_rule()), so its site is dark whatever the
#' visitor's system: it is pinned to dark, and `ui.theme: light` cannot hold.
#' @param config Settings, with the brand in `.brand` when there is one.
#' @return "auto", "light" or "dark".
#' @noRd
site_theme <- function(config) {
  theme <- config$ui$theme %||% "auto"
  if (is_dark_palette(brand_variables(config$.brand))) {
    if (identical(theme, "light")) {
      cli::cli_warn(
        "{.field ui.theme: light} is ignored: the brand's palette is dark, so the site is pinned to dark."
      )
    }
    return("dark")
  }
  theme
}

#' The wire config as JSON that is safe inside an inline <script>
#'
#' jsonlite escapes `/` but not `<`, so a file name containing `<!--<script>`
#' could leave the tag unterminated; U+2028/U+2029 end a JavaScript line.
#' @noRd
viewer_config_json <- function(wire) {
  json <- as.character(jsonlite::toJSON(wire, auto_unbox = TRUE, null = "null"))
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  json <- gsub("\u2028", "\\u2028", json, fixed = TRUE)
  gsub("\u2029", "\\u2029", json, fixed = TRUE)
}

#' The inline config `<script>` tag for the vendored viewer
#' @noRd
inject_viewer_config <- function(wire) {
  sprintf("<script>window.__VIEWER_CONFIG__ = %s;</script>", viewer_config_json(wire))
}

#' @noRd
inline_viewer_config_pattern <- function() {
  "<script>window\\.__VIEWER_CONFIG__ = (.*?);</script>"
}

#' Read the inline config back out of a page (NULL when there is none)
#' @noRd
read_inline_viewer_config <- function(html) {
  m <- regexpr(inline_viewer_config_pattern(), html, perl = TRUE)
  if (m == -1L) {
    return(NULL)
  }
  tag <- regmatches(html, m)
  json <- sub(";</script>$", "", sub("^<script>window\\.__VIEWER_CONFIG__ = ", "", tag))
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

#' Replace the inline config in a page
#'
#' Spliced by position rather than with sub(): the JSON carries backslashes
#' (\u003c), which a regex replacement string would reinterpret.
#' @noRd
replace_inline_viewer_config <- function(html, wire) {
  m <- regexpr(inline_viewer_config_pattern(), html, perl = TRUE)
  if (m == -1L) {
    cli::cli_abort("The page has no inline viewer config to replace.")
  }
  paste0(
    substr(html, 1L, m - 1L),
    inject_viewer_config(wire),
    substr(html, m + attr(m, "match.length"), nchar(html))
  )
}

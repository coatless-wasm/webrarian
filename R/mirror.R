# Enterprise webR Mirror
#
# Create self-hosted/enterprise webR deployments. Everything is
# bundled locally for true offline/air-gapped operation.

#' Create a webR mirror for self-hosted or air-gapped deployment
#'
#' Builds a self-contained webR site in `dest` with the webR engine, the requested
#' packages with their dependencies (or whole repositories), a package index so
#' `install.packages()` works with no network, the viewer, a `_headers` file
#' and a `LICENSES/` directory. Everything is downloaded at build time and
#' checked against the repositories' checksums, and nothing is fetched when the
#' page runs.
#'
#' @param dest Directory to build the mirror in, created if needed. Package
#'   files already there whose checksum matches are reused, so an interrupted
#'   mirror resumes when the same call is run again.
#' @param packages Character vector of packages for `mode = "packages"`.
#'   `NULL` builds an empty REPL (with an empty package index).
#' @param mode `"packages"` (the default) mirrors `packages` and their
#'   dependencies, and `"full"` mirrors every package of every repository (tens
#'   of GB).
#' @param share_links What a share link may do on the mirror, which is `"off"`
#'   (the default, where links are ignored and the Share button is hidden), `"fixed"`
#'   (links may add files only) or `"open"` (links may also add packages, which
#'   the mirror can install only if they are in its repository).
#' @param webr_version webR version. `NULL` uses the `WEBRARIAN_WEBR_VERSION`
#'   environment variable, else the newest version tested with this webrarian.
#' @param repos Extra WebAssembly package repositories, searched after
#'   repo.r-wasm.org (an r-universe, for example).
#' @param favicon Path to an icon (svg, ico or png) for the mirror's page.
#'   `NULL` uses a built-in default icon.
#'
#' @return Invisibly, `dest`.
#'
#' @export
#'
#' @examples
#' # Downloads the ~40 MB webR engine and the packages. Run only in an
#' # interactive session with a network connection, so R CMD check never makes
#' # this download (--as-cran runs the "donttest" examples too). The download
#' # cache goes under tempdir() here.
#' if (interactive() && requireNamespace("curl", quietly = TRUE) &&
#'     curl::has_internet()) {
#'   old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
#'   Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "webrarian-cache"))
#'   mirror <- file.path(tempdir(), "webr-mirror")
#'   collection_mirror(mirror, packages = "cli")
#'   list.files(mirror)
#'   unlink(mirror, recursive = TRUE)
#'   if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old)
#' }
#'
#' \dontrun{
#' # Every package of repo.r-wasm.org and an r-universe: tens of GB
#' collection_mirror("/srv/webr-mirror", mode = "full",
#'                   repos = "https://user.r-universe.dev")
#' }
collection_mirror <- function(
  dest,
  packages = NULL,
  mode = c("packages", "full"),
  share_links = "off",
  webr_version = NULL,
  repos = NULL,
  favicon = NULL
) {
  mode <- match.arg(mode)
  if (
    !is.character(share_links) ||
      length(share_links) != 1L ||
      !share_links %in% c("open", "fixed", "off")
  ) {
    cli::cli_abort("{.arg share_links} must be one of {.val open}, {.val fixed} or {.val off}.")
  }
  if (!is.character(dest) || length(dest) != 1L || is.na(dest) || !nzchar(dest)) {
    cli::cli_abort("{.arg dest} must be a single directory path.")
  }
  if (!is.null(packages) && !is.character(packages)) {
    cli::cli_abort("{.arg packages} must be a character vector or NULL.")
  }
  if (mode == "full" && !is.null(packages)) {
    cli::cli_warn("{.arg packages} is ignored when {.arg mode} is {.val full}.")
  }
  progress <- rlang::is_interactive()
  all_repos <- unique(c(default_repo_url(), as.character(unlist(repos))))

  # One check for the whole mirror: an untested or unsupported engine fails
  # before anything is downloaded.
  engine <- resolve_webr_version(webr_version %||% webr_version_default())
  webr_version <- engine$version
  r_version <- engine$r_version

  cli::cli_h1("Creating webR mirror")
  cli::cli_text("webR version: {.val {webr_version}} (R {.val {r_version}})")
  cli::cli_text("Mode: {.val {mode}}")
  cli::cli_text("Output: {.path {dest}}")
  ensure_dir(dest)

  cli::cli_progress_step("Installing webR engine...")
  webr_assets_copy_to(dest, webr_version)

  pkg_dir <- fs::path(dest, "repo", "bin", "emscripten", "contrib", r_version)
  mirrored <- NULL
  if (mode == "full") {
    cli::cli_progress_step("Mirroring the full repository...")
    mirrored <- mirror_full_repository(pkg_dir, r_version, all_repos, progress, resume = TRUE)
  } else if (length(packages) > 0) {
    cli::cli_progress_step("Mirroring selected packages...")
    mirrored <- download_webr_packages(
      packages,
      pkg_dir,
      r_version,
      all_repos,
      include_deps = TRUE,
      reuse_existing = TRUE,
      progress = progress
    )
  } else {
    cli::cli_alert_info("No packages specified - creating empty REPL")
    # An empty index, so install.packages() says "not available" instead of
    # failing to fetch one.
    ensure_dir(pkg_dir)
    write_repo_index(
      pkg_dir,
      matrix(character(), 0, 2, dimnames = list(NULL, c("Package", "Version")))
    )
  }

  cli::cli_progress_step("Generating index.html...")
  generate_mirror_html(
    dest,
    list(
      webr_version = webr_version,
      r_version = r_version,
      favicon = favicon,
      share_links = share_links
    )
  )
  # A mirror has no service worker. The kill-switch releases visitors whose
  # browser still runs one from a site that was deployed here before.
  emit_service_worker_retirement(dest)
  emit_cache_headers(dest, root = dest)
  sources <- if (is.null(mirrored)) {
    character()
  } else {
    prebuilt_sources(mirrored$rows, mirrored$source)
  }
  emit_licenses(
    dest,
    engine,
    engine_bundled = TRUE,
    bundled = site_package_table(dest, r_version, sources),
    what = "mirror"
  )
  generate_mirror_config(
    dest,
    list(
      webr_version = webr_version,
      r_version = r_version,
      mode = mode,
      packages = packages,
      repos = all_repos
    )
  )
  cli::cli_progress_done()

  all_files <- fs::dir_ls(dest, recurse = TRUE, type = "file")
  total_size <- sum(fs::file_info(all_files)$size, na.rm = TRUE)
  cli::cli_alert_success("Mirror complete! Total size: {format(fs::fs_bytes(total_size))}")
  cli::cli_text("Deploy the files in {.path {dest}} to any static host")

  invisible(dest)
}

#' Copy webR assets to a destination directory
#'
#' Copies webR REPL assets from cache to a specified directory.
#' Downloads assets first if not cached.
#'
#' @param dest_dir Destination directory.
#' @param version webR version. Defaults to `webr_assets_version()`.
#'
#' @return Invisibly returns the destination directory.
#'
#' @noRd
webr_assets_copy_to <- function(dest_dir, version = webr_assets_version()) {
  ensure_dir(dest_dir)
  install_webr_engine(dest_dir, version)
  invisible(dest_dir)
}

#' Mirror every package of every repository
#'
#' The first repository with a package wins, as everywhere else. A package
#' that fails to download is reported and left out of the index; nothing is
#' listed that is not on disk.
#' @noRd
mirror_full_repository <- function(pkg_dir, r_version, repos, progress, resume) {
  index <- fetch_repo_index(repos, r_version)
  if (is.null(index$packages)) {
    cli::cli_abort(c(
      "Could not fetch the package index from any repository.",
      "i" = "Check your network connection and {.arg repos}."
    ))
  }
  all_packages <- unname(index$packages[, "Package"])
  cli::cli_alert_warning(
    "Full mirror: {length(all_packages)} packages. This may require 20+ GB of disk space."
  )
  download_webr_packages(
    all_packages,
    pkg_dir,
    r_version,
    repos,
    include_deps = FALSE,
    reuse_existing = resume,
    progress = progress,
    index = index,
    required = character()
  )
}

#' Generate mirror HTML page with the inline viewer config
#'
#' Copies the vendored viewer (inst/viewer) into the mirror and injects the
#' ViewerConfig inline, through the same build_viewer_config() and
#' inject_viewer_config() bind() uses. `config$share_links` sets the
#' share-link mode (default `"off"`).
#' @noRd
generate_mirror_html <- function(output_dir, config) {
  copy_vendored_viewer(output_dir)

  # Patch the page title into the copied index.html.
  index_path <- fs::path(output_dir, "index.html")
  html_content <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
  page_title <- config$page_title %||% "webR"
  html_content <- gsub(
    "<title>webrarian</title>",
    sprintf("<title>%s</title>", html_escape(page_title)),
    html_content,
    fixed = TRUE
  )

  # Custom favicon (company branding), mirrored from the bind() pattern in
  # copy_ui_assets()/generate_ui_head() (R/build.R).
  if (!is.null(config$favicon) && fs::file_exists(config$favicon)) {
    assets_dir <- fs::path(output_dir, "assets", "custom")
    ensure_dir(assets_dir)
    ext <- fs::path_ext(config$favicon)
    fs::file_copy(config$favicon, fs::path(assets_dir, paste0("favicon.", ext)), overwrite = TRUE)

    mime_type <- switch(
      ext,
      "ico" = "image/x-icon",
      "svg" = "image/svg+xml",
      "png" = "image/png",
      "gif" = "image/gif",
      "image/x-icon"
    )
    favicon_link <- sprintf(
      '<link rel="icon" type="%s" href="assets/custom/favicon.%s" />',
      mime_type,
      ext
    )
    html_content <- gsub(
      "</head>",
      paste0("  ", favicon_link, "\n</head>"),
      html_content,
      fixed = TRUE
    )
  } else {
    html_content <- sub(
      "</head>",
      paste0("  ", default_favicon_link(), "\n</head>"),
      html_content,
      fixed = TRUE
    )
  }

  # Mirror config: engine bundled locally, packages pre-installed into ./repo,
  # nothing installed at boot and no bundled user files. A mirror is always
  # offline: it searches only its own ./repo and makes no request to another
  # origin. Share links default to off: a mirror runs exactly what it ships.
  version <- config$webr_version %||% default_webr_version()
  mirror_config <- apply_config_defaults(list(
    project = list(name = page_title),
    webr = list(version = version),
    repl = list(auto_open = list(), share_links = config$share_links %||% "off")
  ))
  wire <- build_viewer_config(
    mirror_config,
    files = character(),
    engine_base_url = engine_base_url_for(version, TRUE),
    packages = list(install = character(), repos = character(), repo_url = "./repo"),
    repl_files = list(auto_open = character(), auto_run = character(), startup_script = NULL),
    offline = TRUE
  )
  html_content <- gsub(
    '<script type="module"',
    paste0(
      inject_viewer_config(wire),
      '\n  ',
      viewer_url_resolver_script(),
      '\n  <script type="module"'
    ),
    html_content,
    fixed = TRUE
  )

  writeLines(html_content, index_path)

  cli::cli_alert_success("Generated {.file index.html} with inline viewer config")
}

#' Generate mirror configuration file
#'
#' @noRd
generate_mirror_config <- function(output_dir, config) {
  # Count packages in repo
  pkg_dir <- fs::path(output_dir, "repo", "bin", "emscripten", "contrib", config$r_version)
  pkg_count <- 0
  if (fs::dir_exists(pkg_dir)) {
    pkg_files <- fs::dir_ls(pkg_dir, glob = "*.tgz", fail = FALSE)
    pkg_count <- length(pkg_files)
  }

  mirror_info <- list(
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    webrarian_version = as.character(utils::packageVersion("webrarian")),
    webr_version = config$webr_version,
    r_version = config$r_version,
    mode = config$mode,
    packages = list(
      count = pkg_count,
      requested = config$packages
    ),
    repos = config$repos
  )

  jsonlite::write_json(
    mirror_info,
    fs::path(output_dir, "mirror-config.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  cli::cli_alert_success("Generated {.file mirror-config.json}")
}

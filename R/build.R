# Build orchestration

#' Bind the collection into a site
#'
#' Builds the collection into a static site with the webR engine and the
#' prebuilt packages (unless `build.bundle-engine` is `false`), the
#' collection's compiled packages and files, the exlibris viewer with its
#' configuration, and a `LICENSES/` directory. Like bookbinding, this
#' assembles every part into a finished volume. The site goes to
#' `build.output-dir` in `_webrarian.yml` (default `_site`). An existing site
#' there is replaced only once the new one is complete.
#'
#' @param path The collection, or any directory inside it.
#' @param offline If `TRUE`, the site makes no request to another origin when
#'   it runs. The webR engine and every prebuilt package are copied into it,
#'   and a share link, the Packages tab or `install.packages()` can add only
#'   the packages it bundles. If `FALSE`, the page may also install packages
#'   from repo.r-wasm.org (and `packages.repos`), so visitors can add any
#'   published package. `NULL` (the default) uses `build.offline` from
#'   `_webrarian.yml`, which defaults to `FALSE`. Whether an online site
#'   carries the engine and its prebuilt packages is the separate
#'   `build.bundle-engine` setting. It defaults to `true`, and `false` loads the
#'   engine from the webR CDN and has the page install the packages when it
#'   opens.
#' @param clean If `TRUE`, the new site replaces everything in the output
#'   directory. If `FALSE`, keep files of the previous site that the build
#'   does not write (a file added to the output directory by hand, say), but
#'   nothing under a directory the build owns, such as `vfs-files/`. Either
#'   way the site is built in a staging directory and replaces the old one
#'   only when the build succeeds. `NULL` (the default) uses `build.clean`,
#'   which defaults to `TRUE`.
#'
#' @section Share links:
#' The viewer's Share button makes a link that opens a visitor's files on the
#' same site. `repl.share-links` in `_webrarian.yml` decides what such a link
#' may do. `"open"` (the default) lets it add files and packages, `"fixed"`
#' lets it add files only, and `"off"` makes the page ignore links and hides
#' the button. A link never removes the site's packages or replaces its startup
#' script, so a shared file at the startup script's path is saved beside it as
#' `<name>-shared.<ext>`. In `"open"` mode any other shared file with the same
#' path as a bundled file replaces it for that visitor, and in `"fixed"` mode
#' it is saved beside it. On a site built with `build.offline: true` a link can add
#' only packages the site bundles.
#' See `vignette("customization", package = "webrarian")`.
#'
#' @return Invisibly, a `webrarian_bind_result` with `output` (the site's path),
#'   `size_by_part` (bytes of the `engine`, `packages`, `files` and `viewer`,
#'   the last being everything else, where `engine` is 0 when the page loads the
#'   engine from the webR CDN), `packages` (the packages the page installs)
#'   and `offline`. Printing it shows the same summary.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-bind-demo")
#' catalog(collection)
#' writeLines("mean(1:10)", file.path(collection, "analysis.R"))
#' acquire_file("analysis.R", path = collection)
#'
#' # Load the webR engine from its CDN instead of copying the ~40 MB engine
#' # into the site, so this build downloads nothing
#' settings_set(collection, "build.bundle-engine" = FALSE)
#' site <- bind(collection)
#' site
#' list.files(site$output)
#'
#' # Build somewhere else by changing build.output-dir
#' settings_set(collection, "build.output-dir" = "public")
#' bind(collection)
#'
#' \dontrun{
#' # By default the webR engine is copied into the site; it is downloaded
#' # once into the webrarian cache (see webr_cache_info()).
#' settings_set(collection, "build.bundle-engine" = TRUE)
#' bind(collection)
#'
#' # An offline site: the engine and every prebuilt package are copied in,
#' # and the page makes no request to another origin
#' bind(collection, offline = TRUE)
#'
#' # GitHub and local packages are compiled to WebAssembly in the versioned
#' # ghcr.io/r-wasm/webr Docker image, so Docker must be running.
#' acquire_package("r-lib/cli", path = collection, source = "github")
#' bind(collection)
#' }
#'
#' unlink(collection, recursive = TRUE)
bind <- function(path = ".", offline = NULL, clean = NULL) {
  check_collection(path)
  config <- collection_settings(path)
  root <- collection_root(path)
  engine <- resolve_webr_version(config$webr$version)

  # brand: in _webrarian.yml (a path or an inline mapping), else _brand.yml
  brand <- load_brand(config, root)

  if (!is.null(brand)) {
    config$ui <- apply_brand_to_ui(brand, config$ui)
    config$.brand <- brand
  }

  output_dir <- config$build$output_dir
  clean <- clean %||% config$build$clean
  offline <- offline %||% config$build$offline %||% FALSE
  if (!isTRUE(offline) && !isFALSE(offline)) {
    cli::cli_abort("{.arg offline} must be TRUE or FALSE.")
  }
  # An offline site makes no request to another origin, so it always carries
  # the engine and its packages; otherwise build.bundle-engine decides.
  bundle <- offline || !isFALSE(config$build$bundle_engine)
  if (offline && isFALSE(config$build$bundle_engine)) {
    cli::cli_alert_info(
      "{.field build.offline} is true, so the site carries the webR engine and its packages; {.field build.bundle-engine}: false is ignored."
    )
  }
  # The steps below read what the page is built for (web fonts).
  config$build$offline <- offline

  # Nothing below may touch a directory bind() is not allowed to replace.
  output_path <- resolve_output_dir(root, output_dir)
  check_output_dir_replaceable(output_path)
  files <- select_collection_files(config, root, output_path)
  # Watch mode's manifest, taken before any file is copied: an edit saved while
  # this build runs is still a change afterwards.
  build_manifest <- create_build_manifest(config, root)

  # Bad auto-open / auto-run / startup-script entries fail here, before any
  # work, rather than in a visitor's browser console.
  repl_files <- resolve_repl_files(config, files)

  cli::cli_h1("Building webrarian bundle")
  cli::cli_text("Project: {.val {config$project$name}}")
  cli::cli_text("webR version: {.val {engine$version}}")
  if (!is.null(brand)) {
    cli::cli_text("Brand: {.path {brand$.path}}")
  }
  cli::cli_text("Output: {.path {output_path}}")

  has_packages <- any(
    length(config$packages$prebuilt) > 0,
    length(config$packages$github) > 0,
    length(config$packages$local) > 0
  )
  if (length(config$packages$github) > 0 || length(config$packages$local) > 0) {
    check_requirements(path)
  }

  # The site is always assembled in a staging directory and swapped in only
  # after every step succeeded, so a failed or interrupted build leaves the
  # previous site exactly as it was, whatever build.clean says.
  target <- begin_staged_build(root)
  on.exit(discard_staged_build(target), add = TRUE)
  ensure_dir(target)
  build_id <- new_build_id()

  if (bundle) {
    cli::cli_progress_step("Installing webR engine...")
    install_webr_engine(target, engine$version)
  } else {
    cli::cli_alert_info("Using webR CDN engine (v{engine$version})")
  }

  pkgs <- list(
    install = character(),
    compiled = character(),
    has_local_repo = FALSE,
    sources = character()
  )
  if (has_packages) {
    cli::cli_progress_step("Configuring packages...")
    # output_path is where this build ends up (build.output-dir): never copy
    # it into a package build.
    pkgs <- configure_packages(config, root, target, engine, bundle, skip = output_path)
  }

  has_files <- length(files) > 0
  if (has_files) {
    cli::cli_progress_step("Copying collection files...")
    build_files_vfs(files, root, target)
  }

  # A site that bundles its packages also ships them installed, as one image
  # the viewer mounts at start-up instead of installing each package
  # (build.library-image, on by default). repo/ stays: the viewer falls
  # back to it when the image cannot be mounted, and install.packages() keeps
  # a repository. build_library_image() writes no image when a bundled package
  # needs one the site does not bundle: the viewer skips webr::install() for a
  # package an image holds, and that is what would fetch the missing one.
  library_images <- character()
  if (isTRUE(pkgs$has_local_repo) && !isFALSE(config$build$library_image)) {
    cli::cli_progress_step("Writing the package library image...")
    pkg_dir <- fs::path(target, "repo", "bin", "emscripten", "contrib", engine$r_version)
    image <- build_library_image(pkg_dir, target)
    if (!is.null(image)) library_images <- paste0("./", image)
  }

  cli::cli_progress_step("Emitting viewer + config...")
  repos <- viewer_package_repos(
    offline,
    has_local_repo = pkgs$has_local_repo,
    config_repos = unlist(config$packages$repos)
  )
  emit_viewer(
    config,
    target,
    root,
    site = list(
      files = files,
      engine_base_url = engine_base_url_for(engine$version, bundle),
      packages = list(
        install = pkgs$install,
        repos = repos$repos,
        repo_url = repos$repo_url,
        library_images = library_images
      ),
      repl_files = repl_files,
      build_id = build_id,
      offline = offline
    )
  )

  # Runtime caching: tell the *visitor's* browser it may keep the engine.
  # `_headers` is read by Netlify and Cloudflare Pages; the (opt-in) service
  # worker is what covers GitHub Pages, which cannot set headers at all.
  emit_cache_headers(target, root)

  # LICENSES/: what the site redistributes, under which license, and where
  # its source is. What the site carries decides it: `bundle`,
  # not `offline` (the default online site carries its engine too).
  bundled <- site_package_table(target, engine$r_version, pkgs$sources)
  emit_licenses(
    target,
    engine,
    bundle,
    bundled = bundled,
    runtime = setdiff(pkgs$install, bundled$package),
    runtime_repos = setdiff(c(repos$repo_url, repos$repos), "./repo")
  )

  # packages.json and a line naming bundled packages that lag CRAN.
  report_package_drift(bundled, target)
  alert_gpl_local_packages(bundled)

  # Copy UI assets (favicon, logo, custom CSS from brand + ui config)
  copy_ui_assets(config, root, target)
  if (!is.null(brand)) {
    copy_brand_assets(brand, target)
  }
  write_build_marker(target, build_id)

  # build.clean: false keeps what the user put into the site (a CNAME, say),
  # never an old copy of anything the build writes (build_owned_path()).
  if (!isTRUE(clean)) {
    kept <- carry_over_previous_site(output_path, target)
    if (kept > 0L) {
      cli::cli_alert_info(
        "Kept {kept} file{?s} from the previous site ({.field build.clean} is false)."
      )
    }
  }
  commit_staged_build(target, output_path)

  # Save build manifest for incremental builds
  save_build_manifest(build_manifest, root)

  # Close the last progress step before the summary, or its tick prints after
  # it (and out of order in logs).
  cli::cli_progress_done()

  result <- new_bind_result(output_path, offline, pkgs$install)
  sizes <- vapply(result$size_by_part, function(b) format(fs::fs_bytes(b)), character(1))
  cli::cli_alert_success(paste0(
    "Build complete! Output size: {format(fs::fs_bytes(sum(result$size_by_part)))} ",
    "(engine {sizes[['engine']]}, packages {sizes[['packages']]}, ",
    "files {sizes[['files']]}, viewer {sizes[['viewer']]})"
  ))
  if (!isTRUE(webrarian_env$in_watch)) {
    hint <- if (identical(path, ".")) {
      "webrarian::reading_room()"
    } else {
      sprintf("webrarian::reading_room(%s)", encodeString(as.character(path), quote = "\""))
    }
    cli::cli_text("")
    cli::cli_text("Preview: {.run {hint}}")
  }

  invisible(result)
}

#' What bind() built
#' @noRd
new_bind_result <- function(output, offline, packages) {
  bytes_in <- function(dir) {
    if (!fs::dir_exists(dir)) {
      return(0)
    }
    files <- fs::dir_ls(dir, recurse = TRUE, type = "file", all = TRUE)
    sum(fs::file_info(files)$size, na.rm = TRUE)
  }
  total <- bytes_in(output)
  engine <- bytes_in(fs::path(output, "webr"))
  pkgs <- bytes_in(fs::path(output, "repo")) + bytes_in(fs::path(output, "library"))
  files <- bytes_in(fs::path(output, "vfs-files"))
  structure(
    list(
      output = fs::path(output),
      size_by_part = c(
        engine = engine,
        packages = pkgs,
        files = files,
        viewer = total - engine - pkgs - files
      ),
      packages = as.character(packages),
      offline = isTRUE(offline)
    ),
    class = "webrarian_bind_result"
  )
}

#' Print what bind() built
#'
#' @param x A `webrarian_bind_result` from [bind()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.webrarian_bind_result <- function(x, ...) {
  sizes <- vapply(x$size_by_part, function(b) format(fs::fs_bytes(b)), character(1))
  cli::cli_text("Site: {.path {x$output}}")
  cli::cli_text(paste0(
    "Size: {format(fs::fs_bytes(sum(x$size_by_part)))} (engine {sizes[['engine']]}, ",
    "packages {sizes[['packages']]}, files {sizes[['files']]}, viewer {sizes[['viewer']]})"
  ))
  cli::cli_text(
    "Engine: {if (x$size_by_part[['engine']] > 0) 'bundled' else 'loaded from the webR CDN'}"
  )
  cli::cli_text(
    "Offline: {if (x$offline) 'yes, the page makes no request to another origin' else 'no, the page may install packages from the network'}"
  )
  if (length(x$packages) > 0L) {
    cli::cli_text("Packages: {.pkg {x$packages}}")
  }
  invisible(x)
}

#' Clean the shelves
#'
#' Removes the build output directory, clearing all build artifacts.
#'
#' @param path The collection, or any directory inside it. The directory
#'   removed is its `build.output-dir`, with any staging directories an
#'   interrupted build left in `.webrarian/`.
#'
#' @return Invisibly returns the project path (`path`), called for its side
#'   effect of removing the build output directory.
#'
#' @export
#'
#' @examples
#' collection <- file.path(tempdir(), "webrarian-clean-demo")
#' catalog(collection)
#' # Load the engine from the webR CDN, so nothing is downloaded here
#' settings_set(collection, "build.bundle-engine" = FALSE)
#' bind(collection)
#'
#' # Remove the generated `_site/` directory
#' clean_shelves(collection)
#'
#' unlink(collection, recursive = TRUE)
clean_shelves <- function(path = ".") {
  check_collection(path)
  config <- collection_settings(path)
  root <- collection_root(path)

  output_dir <- config$build$output_dir
  output_path <- resolve_output_dir(root, output_dir)

  for (leftover in staging_leftovers(root)) {
    fs::dir_delete(leftover)
  }

  if (!fs::dir_exists(output_path)) {
    cli::cli_alert_info("Nothing to clean")
    return(invisible(path))
  }
  check_output_dir_replaceable(output_path)
  fs::dir_delete(output_path)
  cli::cli_alert_success("Cleaned {.path {output_path}}")

  invisible(path)
}

#' Abort unless the Docker daemon a collection needs is running
#'
#' Collections with local or GitHub packages compile them in a Docker
#' container. Aborts, saying whether Docker is missing or not running, when the
#' collection needs it; diagnose_collection() reports the same without
#' aborting.
#' @return `TRUE`, invisibly.
#' @noRd
check_requirements <- function(path = ".") {
  check_collection(path)
  config <- collection_settings(path)

  has_local_github <- length(config$packages$github) > 0 || length(config$packages$local) > 0
  if (!has_local_github) {
    return(invisible(TRUE))
  }

  status <- docker_status()
  if (identical(status, "missing")) {
    cli::cli_abort(
      c(
        "Docker is required to compile local and GitHub packages to WebAssembly.",
        "i" = "Install Docker: {.url https://docs.docker.com/get-docker/}",
        "i" = "Prebuilt packages need no Docker."
      ),
      class = "webrarian_error_docker"
    )
  }
  if (identical(status, "stopped")) {
    cli::cli_abort(
      c(
        "Docker is installed but not running.",
        "i" = "Start Docker Desktop (or the Docker daemon), then run {.fn bind} again.",
        "i" = "Compiling local and GitHub packages to WebAssembly needs it."
      ),
      class = "webrarian_error_docker"
    )
  }
  invisible(TRUE)
}

#' Configure packages for the bundle
#'
#' Compiles local and GitHub packages first, so their names and dependencies
#' are known; then, when the site carries its packages, downloads the prebuilt
#' packages and those dependencies, leaving out anything compiled; then writes
#' one index for everything in repo/.
#' @param engine A resolved webR version (`resolve_webr_version()`).
#' @param bundle Download the prebuilt packages and the compiled packages'
#'   dependencies into the site's repo/ (`build.bundle-engine`, or
#'   `build.offline`). Otherwise the page installs them when it opens.
#' @param skip Absolute paths never copied into the Docker build: bind()
#'   passes the site's resolved output directory, wherever it is.
#' @return `list(install, compiled, has_local_repo, sources)`; `sources` maps
#'   each package in repo/ to its source link (LICENSES/PACKAGES.md).
#' @noRd
configure_packages <- function(config, root, output_dir, engine, bundle, skip = character()) {
  prebuilt <- as.character(unlist(config$packages$prebuilt))
  github <- as.character(unlist(config$packages$github))
  local <- as.character(unlist(config$packages$local))
  include_deps <- isTRUE(config$packages$dependencies)
  repos <- unique(c(default_repo_url(), as.character(unlist(config$packages$repos))))
  pkg_dir <- fs::path(output_dir, "repo", "bin", "emscripten", "contrib", engine$r_version)

  compiled <- list(names = character(), deps = character(), rows = NULL)
  if (length(github) > 0L || length(local) > 0L) {
    if (!docker_available()) {
      check_requirements(root)
    }
    ensure_dir(pkg_dir)
    compiled <- build_packages_rwasm_docker(
      github,
      local,
      root,
      pkg_dir,
      engine$r_version,
      engine$version,
      skip = skip
    )
  }

  downloaded <- NULL
  wanted <- setdiff(unique(c(prebuilt, compiled$deps)), compiled$names)
  if (isTRUE(bundle) && length(wanted) > 0L) {
    # Only the configured prebuilt packages are hard requirements. A compiled
    # package's dependency that no repository has is a warning, like any
    # other missing transitive dependency: the author never asked for it.
    downloaded <- download_webr_packages(
      wanted,
      pkg_dir,
      engine$r_version,
      repos,
      include_deps,
      exclude = compiled$names,
      required = prebuilt
    )
  } else if (length(prebuilt) > 0L) {
    cli::cli_alert_info("Packages will be installed when the page opens: {.pkg {prebuilt}}")
  }

  if (length(compiled$names) > 0L) {
    rows <- if (is.null(downloaded) || nrow(downloaded$rows) == 0L) {
      compiled$rows
    } else {
      align_and_rbind(downloaded$rows, compiled$rows)
    }
    write_repo_index(pkg_dir, rows)
  }

  list(
    install = unique(c(prebuilt, compiled$names)),
    compiled = compiled$names,
    has_local_repo = fs::dir_exists(pkg_dir) && length(fs::dir_ls(pkg_dir, glob = "*.tgz")) > 0L,
    sources = c(
      compiled_sources(compiled$rows, local, github, root),
      if (!is.null(downloaded)) prebuilt_sources(downloaded$rows, downloaded$source)
    )
  )
}

#' The source directory of a configured local package
#'
#' Relative paths are relative to the collection; absolute and `~` paths are
#' used as given.
#' @noRd
resolve_local_package_path <- function(root, pkg_path, call = rlang::caller_env()) {
  src <- normalize_project_path(fs::path_expand(pkg_path), root)
  if (!fs::dir_exists(src)) {
    cli::cli_abort(
      c(
        "Local package {.path {pkg_path}} does not exist.",
        "i" = "Looked for {.path {src}}; fix {.field packages.local} in {.file _webrarian.yml}."
      ),
      call = call
    )
  }
  if (!fs::file_exists(fs::path(src, "DESCRIPTION"))) {
    cli::cli_abort(
      "Local package {.path {pkg_path}} has no {.file DESCRIPTION}.",
      call = call
    )
  }
  src
}

#' Copy a package source tree, leaving out VCS, IDE and build directories
#'
#' Also leaves out every directory a bind() built (it carries the
#' .webrarian-build marker), so a collection that is itself the package never
#' ships an old site - under any output directory name - into the build.
#' @param skip Absolute paths (and everything under them) not to copy.
#' @noRd
copy_package_source <- function(src, dest, skip = character()) {
  src_real <- as.character(fs::path_real(src))
  files <- list_collection_files(src_real)
  built <- dirname(files[basename(files) == build_marker_name()])
  skip <- c(as.character(skip), paste0(src_real, "/", built))
  for (f in files) {
    absolute <- paste0(src_real, "/", f)
    if (any(absolute == skip | startsWith(absolute, paste0(skip, "/")))) {
      next
    }
    target <- fs::path(dest, f)
    ensure_dir(fs::path_dir(target))
    fs::file_copy(absolute, target, overwrite = TRUE)
  }
  invisible(dest)
}

#' Validate a webR version string read from user configuration
#'
#' The version is interpolated into a Docker image tag and command, so it must
#' be a strict `major.minor.patch` string. Rejecting anything else neutralizes
#' shell-injection payloads from a hand-edited or cloned `_webrarian.yml`.
#' @noRd
validate_webr_version <- function(version) {
  if (
    !is.character(version) ||
      length(version) != 1L ||
      is.na(version) ||
      !grepl("^[0-9]+(\\.[0-9]+){1,3}$", version)
  ) {
    cli::cli_abort(
      c(
        "Invalid webR version: {.val {version}}.",
        "i" = "Expected a version like {.val 0.6.0} in {.file _webrarian.yml} under {.code webr: version:}."
      ),
      class = "webrarian_error_invalid_version"
    )
  }
  invisible(version)
}

#' major.minor of a version string ("0.6.0" -> "0.6", "4.5.1" -> "4.5")
#' @noRd
version_line <- function(version) sub("^([0-9]+\\.[0-9]+).*$", "\\1", version)

#' Check a webR version against the versions verified with the viewer
#'
#' @return `list(version, r_version, tested)`: `r_version` is the major.minor
#'   R line of the webR package repository.
#' @noRd
resolve_webr_version <- function(version, call = rlang::caller_env()) {
  if (is.numeric(version)) {
    cli::cli_abort(
      c(
        "{.field webr.version} must be a quoted version string.",
        "x" = "YAML read {.val {version}} as a number.",
        "i" = "Write it with quotes, e.g. {.code version: \"{default_webr_version()}\"}."
      ),
      call = call,
      class = "webrarian_error_invalid_version"
    )
  }
  validate_webr_version(version)

  versions <- webr_versions_table()$versions
  listed <- vapply(versions, function(v) v$version, character(1))
  r_lines <- vapply(versions, function(v) version_line(v$r_version), character(1))

  hit <- match(version, listed)
  if (!is.na(hit)) {
    return(list(version = version, r_version = r_lines[[hit]], tested = TRUE))
  }
  # Only a release can be an untested patch. "0.6" or "0.6.0.1" shares the
  # client's line but names no engine on the CDN and no Docker image.
  is_release <- grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version)
  same_line <- which(version_line(listed) == version_line(version))
  if (is_release && length(same_line) > 0L) {
    r_line <- r_lines[[same_line[[1]]]]
    cli::cli_warn(
      c(
        "webR {version} has not been tested with this webrarian's viewer.",
        "i" = "Tested versions: {.val {listed}}. Using the R {r_line} package repository, as for the rest of webR {version_line(version)}."
      ),
      call = call
    )
    return(list(version = version, r_version = r_line, tested = FALSE))
  }
  client <- webr_versions_table()$verified_with_client
  cli::cli_abort(
    c(
      "webR {version} is not supported by this webrarian's viewer.",
      if (!is_release) {
        c(
          "x" = "A webR version names one release: {.code major.minor.patch}, such as {.val {client}}."
        )
      },
      "i" = "The viewer embeds the webR {client} client, which only runs webR {version_line(client)}.x engines.",
      "i" = "Tested versions: {.val {listed}}.",
      "i" = "Set {.field webr.version} in {.file _webrarian.yml} to one of them."
    ),
    call = call,
    class = "webrarian_error_invalid_version"
  )
}

#' Get R version for a webR version (major.minor format for repo)
#' @noRd
get_r_version_for_webr <- function(webr_version) {
  resolve_webr_version(webr_version)$r_version
}

#' A fresh directory for one Docker build (a function so tests can observe it)
#' @noRd
docker_build_dir <- function() {
  fs::path(tempfile("webrarian-docker-"))
}

#' Build packages using rwasm inside Docker
#' @noRd
build_packages_rwasm_docker <- function(
  github_pkgs,
  local_pkgs,
  root,
  pkg_dir,
  r_version,
  webr_version,
  skip = character()
) {
  # Create temp directory for the build
  build_dir <- docker_build_dir()
  fs::dir_create(build_dir)
  # Removed however this function exits: success, compile error or interrupt.
  on.exit(unlink(build_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Copy local packages to build directory
  pkg_refs <- character()

  for (pkg_path in local_pkgs) {
    src_path <- resolve_local_package_path(root, pkg_path)
    desc <- read.dcf(fs::path(src_path, "DESCRIPTION"), fields = "Package")
    pkg_name <- if (nrow(desc) > 0L) unname(desc[1, "Package"]) else NA_character_
    # The name becomes a host path under build_dir, so it is checked before
    # anything is copied: "Package: ../../x" would otherwise write outside it.
    if (!isTRUE(is_valid_package_name(pkg_name))) {
      cli::cli_abort(c(
        "Local package {.path {pkg_path}} does not give a valid R package name in its {.file DESCRIPTION}.",
        "x" = if (is.na(pkg_name)) {
          "It has no {.field Package} field."
        } else {
          "Its {.field Package} field is {.val {pkg_name}}."
        },
        "i" = "A package name has only letters, digits and dots, starts with a letter and does not end with a dot."
      ))
    }
    copy_package_source(src_path, fs::path(build_dir, "packages", pkg_name), skip = skip)
    pkg_refs <- c(pkg_refs, sprintf("local::/build/packages/%s", pkg_name))
    cli::cli_alert_info("Preparing {.pkg {pkg_name}} for compilation...")
  }

  # Add GitHub packages
  for (gh_ref in github_pkgs) {
    pkg_refs <- c(pkg_refs, gh_ref)
    cli::cli_alert_info("Preparing {.pkg {gh_ref}} for compilation...")
  }

  if (length(pkg_refs) == 0) {
    return(list(names = character(), deps = character(), rows = NULL))
  }

  # Create directories for Docker build with open permissions
  repo_build_dir <- fs::path(build_dir, "repo")
  cache_dir <- fs::path(build_dir, ".cache")
  fs::dir_create(repo_build_dir)
  fs::dir_create(cache_dir)
  # Make writable by Docker container (which runs as root)
  # use_umask = FALSE, or the umask quietly turns 0777 into 0755.
  Sys.chmod(c(build_dir, repo_build_dir, cache_dir), "0777", use_umask = FALSE)

  # Create R script for Docker to run
  # Patch fs::dir_create to avoid permission issues on macOS Docker mounts
  r_script <- sprintf(
    '
    # Set up cache directories
    Sys.setenv(
      HOME = "/build",
      R_USER_CACHE_DIR = "/build/.cache",
      XDG_CACHE_HOME = "/build/.cache"
    )
    dir.create("/build/.cache/R/pkgcache/pkg", recursive = TRUE, showWarnings = FALSE)

    # Patch fs to skip chmod (fails on macOS Docker mounts)
    if (requireNamespace("fs", quietly = TRUE)) {
      assignInNamespace("dir_create", function(path, ..., mode = "u=rwx,go=rx", recurse = FALSE, recursive = recurse) {
        path <- as.character(path)
        for (p in path) {
          if (!dir.exists(p)) {
            dir.create(p, recursive = TRUE, showWarnings = FALSE)
          }
        }
        invisible(fs::path(path))
      }, ns = "fs")
    }

    library(rwasm)
    packages <- %s
    # remotes = NULL builds exactly the named packages. The default (NA) first
    # resolves the list of webR forks built into rwasm, which aborts
    # ("`nrow(out)` must equal `1`", r-lib/pkgdepends#462) under the
    # pkgdepends 0.9.1 in the webR 0.6.0 image, before anything is compiled.
    add_pkg(
      packages = packages,
      repo_dir = "/build/repo",
      remotes = NULL,
      dependencies = FALSE
    )
  ',
    paste(deparse(pkg_refs), collapse = "")
  )

  writeLines(r_script, fs::path(build_dir, "build.R"))

  # Run Docker. system2() joins its arguments into one shell command line, so
  # anything that could hold a space or a shell metacharacter is quoted: the
  # mount path is shQuote()d, and webr_version passed validate_webr_version(),
  # so the image tag is a plain semantic version.
  # --platform for ARM Macs; HOME for cache dirs; :delegated for macOS perf.
  docker_image <- sprintf("ghcr.io/r-wasm/webr:v%s", webr_version)
  cli::cli_alert_info("Compiling packages in Docker container ({.val {docker_image}})...")

  docker_args <- c(
    "run",
    "--rm",
    "--platform",
    "linux/amd64",
    "-e",
    "HOME=/build",
    "-e",
    "WEBR_SKIP_CHMOD=1",
    # system2() pastes arguments into one command line, so a path with a
    # space (a Windows user name, say) has to be quoted.
    "-v",
    shQuote(sprintf("%s:/build:delegated", build_dir)),
    "-w",
    "/build",
    docker_image,
    "Rscript",
    "/build/build.R"
  )

  result <- system2("docker", docker_args, stdout = "", stderr = "")

  if (result != 0) {
    # 125: docker itself failed (no daemon, image pull); 126/127: the command
    # in the container could not run. Neither is a compilation error.
    if (result %in% c(125L, 126L, 127L)) {
      cli::cli_abort(c(
        "Docker could not start the webR build container (exit code {result}).",
        "i" = "Check that Docker is running and can pull {.val {docker_image}}; the Docker output above says why."
      ))
    }
    cli::cli_abort(c(
      "Compiling packages to WebAssembly failed inside the webR container (exit code {result}).",
      "i" = "See the compiler output above."
    ))
  }

  # Copy built packages to output
  built_dir <- fs::path(build_dir, "repo", "bin", "emscripten", "contrib", r_version)
  built_pkgs <- character()
  if (fs::dir_exists(built_dir)) {
    built_pkgs <- fs::dir_ls(built_dir, glob = "*.tgz", fail = FALSE)
  }

  if (length(built_pkgs) == 0) {
    cli::cli_abort(c(
      "Docker ran but produced no WebAssembly packages.",
      "i" = "Expected compiled {.file .tgz} files in {.path {built_dir}}."
    ))
  }

  rows <- index_rows_from_tgz(built_pkgs)
  names <- unname(rows[, "Package"])
  # These names reach the page's install list, the index and the deletion
  # pattern below, so every built tarball must carry a plain package name.
  bad <- names[!is_valid_package_name(names)]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "The webR container built {.val {bad}}, which {?is not a valid R package name/are not valid R package names}.",
      "i" = "Fix the {.field Package} field in the {.file DESCRIPTION} of the {.field packages.github} or {.field packages.local} source."
    ))
  }
  # A compiled package replaces any other build of the same package already
  # in the repo, so the index can never point at the wrong version. (bind()
  # compiles into a fresh staging repo/, which even clean = FALSE never fills
  # from the previous site; this guards any other caller.)
  for (name in names) {
    stale <- fs::dir_ls(
      pkg_dir,
      regexp = sprintf("/%s_[^/]*\\.tgz$", gsub(".", "\\.", name, fixed = TRUE))
    )
    if (length(stale) > 0L) fs::file_delete(stale)
  }
  for (pkg_file in built_pkgs) {
    fs::file_copy(pkg_file, pkg_dir, overwrite = TRUE)
  }
  cli::cli_alert_success("Built {length(built_pkgs)} package{?s} for WebAssembly")

  deps <- setdiff(
    parse_deps(c(rows[, "Depends"], rows[, "Imports"], rows[, "LinkingTo"])),
    c(base_package_names(), "R", names)
  )
  list(names = names, deps = deps, rows = rows)
}

#' Copy the selected collection files into <output>/vfs-files/
#' @noRd
build_files_vfs <- function(files, root, output_dir) {
  if (length(files) == 0L) {
    cli::cli_alert_info("No files to include in VFS")
    return(invisible(character()))
  }
  cli::cli_alert_info("Including {length(files)} file{?s} in VFS")

  files_dir <- fs::path_norm(fs::path_abs(fs::path(output_dir, "vfs-files")))
  ensure_dir(files_dir)
  for (file in files) {
    dest <- fs::path_norm(fs::path(files_dir, file))
    if (!path_strictly_inside(dest, files_dir)) {
      cli::cli_abort("Refusing to copy {.file {file}} outside {.path vfs-files}.")
    }
    ensure_dir(fs::path_dir(dest))
    fs::file_copy(fs::path(root, file), dest, overwrite = TRUE)
  }
  invisible(files)
}

#' Locate the vendored viewer assets shipped in inst/viewer
#' @noRd
viewer_source_dir <- function() {
  system.file("viewer", package = "webrarian")
}

#' First 8 hex digits of a file's MD5: the content hash in viewer file names
#' @noRd
viewer_asset_hash <- function(path) {
  substr(unname(tools::md5sum(as.character(path))), 1L, 8L)
}

#' Copy the vendored viewer (inst/viewer) into the output directory
#'
#' exlibris-r.js and exlibris-r.css are copied as exlibris-r.<hash>.js/.css,
#' and the copied index.html is rewritten to reference those names. A new
#' webrarian with a new viewer therefore gets new URLs, so the immutable
#' caching rule for them is correct.
#' @return `list(js, css)`: the file names used.
#' @noRd
copy_vendored_viewer <- function(output_dir) {
  viewer_src <- viewer_source_dir()
  if (!nzchar(viewer_src) || !fs::dir_exists(viewer_src)) {
    cli::cli_abort(c(
      "Vendored viewer not found at {.path inst/viewer}.",
      "i" = "Rebuild it with {.code tools/vendor-exlibris.sh} before calling {.fn bind}."
    ))
  }
  names <- list()
  # Only the page and the bundle go to the site root. The notices in
  # inst/viewer reach a site through LICENSES/ (emit_licenses()), and
  # PROVENANCE.json is for this package's own checks.
  shipped <- fs::path(viewer_src, c("index.html", "exlibris-r.js", "exlibris-r.css"))
  for (f in shipped) {
    rel <- as.character(fs::path_rel(f, viewer_src))
    if (rel %in% c("exlibris-r.js", "exlibris-r.css")) {
      ext <- fs::path_ext(rel)
      rel <- sprintf("exlibris-r.%s.%s", viewer_asset_hash(f), ext)
      names[[ext]] <- rel
    }
    dest <- fs::path(output_dir, rel)
    ensure_dir(fs::path_dir(dest))
    fs::file_copy(f, dest, overwrite = TRUE)
  }

  index_path <- fs::path(output_dir, "index.html")
  html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
  html <- sub('href="./exlibris-r.css"', sprintf('href="./%s"', names$css), html, fixed = TRUE)
  html <- sub('src="./exlibris-r.js"', sprintf('src="./%s"', names$js), html, fixed = TRUE)
  if (!grepl(names$js, html, fixed = TRUE) || !grepl(names$css, html, fixed = TRUE)) {
    cli::cli_abort(c(
      "The vendored host page does not reference {.file ./exlibris-r.js} and {.file ./exlibris-r.css}.",
      "i" = "Re-run {.code tools/vendor-exlibris.sh}."
    ))
  }
  writeLines(html, index_path)
  invisible(list(js = names$js, css = names$css))
}

#' Directory of the webR engine inside a site
#' @noRd
engine_dir_rel <- function(version) paste0("webr/v", version)

#' Where the page loads the webR engine from
#' @param bundled Whether the site carries the engine (bind()'s `bundle`).
#' @noRd
engine_base_url_for <- function(version, bundled) {
  if (isTRUE(bundled)) {
    paste0("./", engine_dir_rel(version), "/")
  } else {
    sprintf("https://webr.r-wasm.org/v%s/", version)
  }
}

#' Copy the cached webR engine into <output>/webr/v<version>/
#'
#' A versioned directory means a new webr.version is a new URL, so browsers
#' may cache the engine forever without ever running a stale one.
#' @return The engine base URL for the page, invisibly.
#' @noRd
install_webr_engine <- function(output_dir, version) {
  assets_dir <- webr_assets_ensure(version)
  dest_root <- fs::path(output_dir, engine_dir_rel(version))
  files <- fs::dir_ls(assets_dir, recurse = TRUE, type = "file")
  files <- files[fs::path_file(files) != ".webrarian-complete"]
  for (f in files) {
    dest <- fs::path(dest_root, fs::path_rel(f, assets_dir))
    ensure_dir(fs::path_dir(dest))
    fs::file_copy(f, dest, overwrite = TRUE)
  }
  cli::cli_alert_success("Copied the engine into {.path {engine_dir_rel(version)}}")
  invisible(engine_base_url_for(version, TRUE))
}

#' Copy the vendored viewer and write the page with its inline config
#'
#' @param site What bind() learned while building: `files` (root-relative
#'   paths of the bundled files), `engine_base_url`, `packages`
#'   (`install`, `repos`, `repo_url`), `repl_files` (resolve_repl_files()),
#'   `build_id` and `offline` (the build's resolved `build.offline`).
#' @return The wire config, invisibly.
#' @noRd
emit_viewer <- function(config, output_dir, root = ".", site) {
  copy_vendored_viewer(output_dir)

  # Opt-in, and off by default: a service worker caches hard enough that an
  # existing deployment must never grow one silently.
  service_worker <- isTRUE(config$build$service_worker)
  if (service_worker) {
    emit_service_worker(output_dir, site$build_id)
  } else {
    emit_service_worker_retirement(output_dir)
  }

  # Patch the copied index.html <head>/<body> for this project (title, favicon,
  # meta, loading overlay) — the same customization the old bundled branch did,
  # minus the repl.js injection and ?mode=/redirect.html machinery.
  index_path <- fs::path(output_dir, "index.html")
  html_content <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

  # The color scheme. Resolved once, so that a dark brand's warning is said
  # once; the config that goes to the viewer below then agrees with the page.
  config$ui$theme <- site_theme(config)
  if (!identical(config$ui$theme, "auto")) {
    # A pinned site says so in the page itself: its loading screen is drawn
    # in that scheme before any script runs.
    html_content <- sub(
      "<html lang=\"en\">",
      sprintf("<html lang=\"en\" data-theme=\"%s\">", config$ui$theme),
      html_content,
      fixed = TRUE
    )
  }

  page_title <- config$ui$meta$title %||% config$project$name
  html_content <- gsub(
    "<title>webrarian</title>",
    sprintf("<title>%s</title>", html_escape(page_title)),
    html_content,
    fixed = TRUE
  )

  # fixed = TRUE throughout: with a regex, a backslash or "\1" in any spliced
  # value (a description, a brand value) would be reinterpreted.
  ui_head <- generate_ui_head(config, root)
  if (nzchar(ui_head)) {
    html_content <- sub("</head>", sprintf("  %s\n</head>", ui_head), html_content, fixed = TRUE)
  }

  # In <head>, so a visitor's stored scheme is on <html> before <body>, and
  # with it the loading screen, is drawn.
  html_content <- sub(
    "</head>",
    sprintf("  %s\n</head>", generate_theme_init_script()),
    html_content,
    fixed = TRUE
  )

  # The loading screen follows the scheme in force, with the brand's logo for
  # that scheme when it has one for each. A screen whose contents the author
  # wrote (custom-html, made for the light screen) keeps its light colors,
  # and a brand whose one palette is dark has no other colors to change to.
  authored_loading <- !is.null(config$ui$loading$custom_html)
  # A dark logo whose file is not there leaves the brand with one logo, which
  # then shows in both schemes (copy_ui_assets() has said the file is missing).
  logo_variants <- !authored_loading &&
    !is.null(config$ui$.logo) &&
    !is.null(dark_logo_source(config$ui, root))
  loading_styles <- generate_loading_styles(
    config$ui$loading %||% list(),
    dark_variant = !authored_loading && !is_dark_palette(brand_variables(config$.brand)),
    logo_variants = logo_variants
  )
  html_content <- sub(
    "</head>",
    sprintf("  <style>%s</style>\n</head>", loading_styles),
    html_content,
    fixed = TRUE
  )

  logo_html <- ""
  if (!is.null(config$ui$.logo)) {
    width <- css_number(config$ui$.logo_width, "brand.logo.width") %||% 100
    height <- css_number(config$ui$.logo_height, "brand.logo.height") %||% 100
    logo_img <- function(source, file, class = "") {
      sprintf(
        '<img %ssrc="assets/custom/%s.%s" alt="Logo" style="width: %spx; height: %spx; margin-bottom: 20px;" />',
        class,
        file,
        html_escape(fs::path_ext(source)),
        css_number_string(width),
        css_number_string(height)
      )
    }
    logo_html <- if (logo_variants) {
      # One for each scheme; the loading styles show the one in force.
      paste0(
        logo_img(config$ui$.logo, "logo", 'class="webrarian-logo-light" '),
        logo_img(config$ui$.logo_dark, "logo-dark", 'class="webrarian-logo-dark" ')
      )
    } else {
      logo_img(config$ui$.logo, "logo")
    }
  }
  loading_html <- generate_loading_html(
    loading_title = "Opening Webrarian Project",
    loading_subtitle = config$project$name,
    logo_html = logo_html,
    loading_message = config$ui$loading$message %||% "Loading webR...",
    custom_html = config$ui$loading$custom_html
  )
  html_content <- gsub("<body>", sprintf("<body>\n  %s", loading_html), html_content, fixed = TRUE)

  # The ViewerConfig goes inline as window.__VIEWER_CONFIG__, before the module
  # <script> that reads it, so the viewer boots without a runtime fetch.
  wire <- build_viewer_config(
    config,
    site$files,
    site$engine_base_url,
    site$packages,
    site$repl_files,
    offline = isTRUE(site$offline)
  )
  registration <- if (service_worker) {
    paste0(service_worker_registration_script(), "\n  ")
  } else {
    ""
  }
  html_content <- gsub(
    '<script type="module"',
    paste0(
      inject_viewer_config(wire),
      '\n  ',
      viewer_url_resolver_script(),
      '\n  ',
      generate_loading_dismiss_script(),
      '\n  ',
      registration,
      '<script type="module"'
    ),
    html_content,
    fixed = TRUE
  )

  writeLines(html_content, index_path)

  cli::cli_alert_success("Emitted viewer with inline config")
  invisible(wire)
}

# ============================================================================
# Runtime caching (the visitor's browser, not the build machine)
# ============================================================================
#
# Two mechanisms, because no single one works everywhere:
#
#   1. `_site/_headers` - the only place a site's Cache-Control and COOP/COEP
#      rules live (a generated netlify.toml holds `[build] publish` only),
#      read by Netlify and Cloudflare Pages. GitHub Pages cannot set custom
#      response headers, so this does nothing there.
#   2. `_site/sw.js` - an opt-in service worker (`build: service-worker: true`)
#      that caches the same assets client-side, for GitHub Pages.
#
# Both follow one rule: only a URL whose bytes can never change is immutable.
# That is the engine under webr/v<version>/ and the viewer bundle named after
# its content hash. Everything rewritten at a stable URL (the page, the
# user's files, the package repo, sw.js) revalidates.

#' A unique identifier for this build
#'
#' Baked into `sw.js` as the cache name suffix so a fresh `bind()` can never be
#' served out of the previous build's cache. Deliberately time-derived rather
#' than content-derived: two builds of an unchanged project must still produce
#' different ids, because the point is to invalidate on *deploy*, not on
#' content change.
#' @noRd
new_build_id <- function() {
  now <- as.numeric(Sys.time())
  stamp <- format(as.POSIXct(now, origin = "1970-01-01", tz = "UTC"), "%Y%m%d%H%M%S")
  # Sub-second component + pid, so two builds in the same second (or in
  # parallel) still differ.
  salt <- substr(rlang::hash(paste0(sprintf("%.6f", now), "-", Sys.getpid())), 1, 8)
  paste0(stamp, "-", salt)
}

#' Locate the templates shipped in inst/templates
#' @noRd
template_path <- function(name) {
  path <- system.file("templates", name, package = "webrarian")
  if (!nzchar(path) || !fs::file_exists(path)) {
    cli::cli_abort("Missing packaged template {.file inst/templates/{name}}.")
  }
  path
}

#' Write `sw.js` into the built site
#' @noRd
emit_service_worker <- function(output_dir, build_id = new_build_id()) {
  src <- template_path("sw.js")
  js <- paste(readLines(src, warn = FALSE), collapse = "\n")
  js <- gsub("{{build_id}}", build_id, js, fixed = TRUE)

  # A leftover placeholder would ship a worker whose cache name never changes,
  # which is exactly the bug this whole mechanism exists to avoid.
  if (grepl("{{", js, fixed = TRUE)) {
    cli::cli_abort("Unsubstituted placeholder left in {.file sw.js}.")
  }

  writeLines(js, fs::path(output_dir, "sw.js"))
  cli::cli_alert_success("Emitted {.file sw.js} (cache id {.val {build_id}})")
  invisible(fs::path(output_dir, "sw.js"))
}

#' Write the retirement worker as `sw.js`
#'
#' Emitted by `bind()` whenever the service worker is off and by
#' `collection_mirror()` always, so a visitor still running a worker from an
#' earlier build is released from it on the next update check.
#' @noRd
emit_service_worker_retirement <- function(output_dir) {
  path <- fs::path(output_dir, "sw.js")
  fs::file_copy(template_path("sw-retire.js"), path, overwrite = TRUE)
  invisible(path)
}

#' The `<script>` that registers the service worker from index.html
#'
#' Registered eagerly rather than on `load`, so the worker can start caching
#' during the first visit; depending on the browser, the runtime is fully
#' cached from the second or third visit. `sw.js` is resolved relative to the
#' document, which puts the registration scope at the deployment's own
#' directory - a project site at `/repo/` gets scope `/repo/`, not `/`.
#' Registration failure is warned, never thrown: without a worker the site
#' still works, it just refetches the runtime next time.
#' @noRd
service_worker_registration_script <- function() {
  paste(
    '<script>',
    '  if ("serviceWorker" in navigator) {',
    '    navigator.serviceWorker.register("sw.js").catch(function (err) {',
    '      console.warn("webrarian: service worker registration failed", err);',
    '    });',
    '  }',
    '</script>',
    sep = "\n  "
  )
}

#' The caching and isolation rules, as data
#'
#' Rules deliberately never set the same header on overlapping patterns:
#' Netlify and Cloudflare Pages concatenate the values of a header several
#' matching rules set, so an overlap would emit nonsense such as
#' `Cache-Control: immutable, no-cache`.
#' @param include_isolation Include the COOP/COEP rule.
#' @return A list of `list(path, comment, headers)`.
#' @noRd
cache_rules <- function(include_isolation = TRUE) {
  immutable <- "public, max-age=31536000, immutable"
  revalidate <- "no-cache"
  rules <- list(
    list(
      path = "/*",
      comment = c(
        "webR needs SharedArrayBuffer, which the browser only exposes to a",
        "cross-origin-isolated page."
      ),
      headers = c(
        "Cross-Origin-Opener-Policy" = "same-origin",
        "Cross-Origin-Embedder-Policy" = "require-corp"
      )
    ),
    list(
      path = "/",
      comment = c(
        "Rewritten by every bind() at a stable URL, so these revalidate: the page",
        "carries the inline viewer config, vfs-files/ is the user's content and",
        "repo/ holds packages that can be rebuilt at the same version. no-cache",
        "still stores the response; an unchanged file costs a 304 and no bytes."
      ),
      headers = c("Cache-Control" = revalidate)
    ),
    list(path = "/index.html", headers = c("Cache-Control" = revalidate)),
    list(path = "/sw.js", headers = c("Cache-Control" = revalidate)),
    list(path = "/vfs-files/*", headers = c("Cache-Control" = revalidate)),
    list(path = "/repo/*", headers = c("Cache-Control" = revalidate)),
    list(
      path = "/LICENSES/*",
      comment = "License notices, rewritten by every bind().",
      headers = c("Cache-Control" = revalidate)
    ),
    list(
      path = "/packages.json",
      comment = "The bundled package versions (report_package_drift()), rewritten by every bind().",
      headers = c("Cache-Control" = revalidate)
    ),
    list(
      path = "/LICENSES/*.md",
      comment = "Shown in the browser as text rather than downloaded.",
      headers = c("Content-Type" = "text/plain; charset=utf-8")
    ),
    list(
      path = "/webr/*",
      comment = c(
        "The webR engine lives under webr/v<version>/: a new version is a new",
        "URL, so nothing here ever changes in place."
      ),
      headers = c("Cache-Control" = immutable)
    ),
    list(
      path = "/library/*",
      comment = "Package library images, named after a hash of their contents.",
      headers = c("Cache-Control" = immutable)
    ),
    list(
      path = "/exlibris-r.*",
      comment = "The viewer bundle, named after a hash of its contents.",
      headers = c("Cache-Control" = immutable)
    ),
    list(
      path = "/*.wasm",
      comment = "Streaming WebAssembly compilation needs the right type.",
      headers = c("Content-Type" = "application/wasm")
    ),
    list(path = "/*.mjs", headers = c("Content-Type" = "text/javascript"))
  )
  if (!isTRUE(include_isolation)) {
    rules <- rules[-1L]
  }
  rules
}

#' The cross-origin isolation headers, as cache_rules() spells them
#' @noRd
ISOLATION_HEADERS <- c("Cross-Origin-Opener-Policy", "Cross-Origin-Embedder-Policy")

#' The `_headers` file, rendered from cache_rules()
#'
#' The only place a webrarian site's response headers come from. When the
#' project has a hand-written netlify.toml, Netlify reads both files and
#' concatenates the values of a header both set for a path, so what that file
#' already sets is left out: each isolation header it sets itself (`isolation`
#' lists the ones that stay, one header at a time), and every other rule whose
#' header it also sets for the same path (`drop_paths`, from
#' netlify_duplicated_paths()).
#' @param include_isolation Emit the COOP/COEP block. `FALSE` is
#'   `isolation = character()`.
#' @param drop_paths Paths whose rules are left out (never the COOP/COEP block).
#' @param isolation The isolation headers the `/*` rule keeps, a subset of
#'   `ISOLATION_HEADERS`; the block is left out when it keeps none.
#' @noRd
cache_headers_content <- function(
  include_isolation = TRUE,
  drop_paths = character(),
  isolation = if (isTRUE(include_isolation)) ISOLATION_HEADERS else character()
) {
  header <- c(
    "# Generated by webrarian::bind() - regenerated on every build, do not edit.",
    "#",
    "# Every response header this site needs is here: cross-origin isolation",
    "# (COOP/COEP) and caching. Netlify and Cloudflare Pages read this file from",
    "# the published directory, so a CLI deploy and a drag-and-drop get the same",
    "# headers. GitHub Pages cannot set custom response headers and ignores this",
    "# file - enable `build: service-worker: true` in _webrarian.yml to get",
    "# runtime caching there.",
    "#",
    "# Only versioned (webr/v<version>/) or content-hashed (exlibris-r.<hash>.*)",
    "# paths are immutable. No two rules below set the same header, because a",
    "# header matched by several rules has its values concatenated.",
    ""
  )
  # The isolation rule keeps only the headers in `isolation`; cache_rules()
  # itself is unchanged, since other code reads it whole.
  rules <- lapply(cache_rules(include_isolation = TRUE), function(rule) {
    if (any(names(rule$headers) %in% ISOLATION_HEADERS)) {
      rule$isolation <- TRUE
      rule$headers <- rule$headers[names(rule$headers) %in% isolation]
    }
    rule
  })
  is_isolation <- vapply(rules, function(rule) isTRUE(rule$isolation), logical(1))
  empty <- vapply(rules, function(rule) length(rule$headers) == 0L, logical(1))
  dropped <- !is_isolation & vapply(rules, function(rule) rule$path %in% drop_paths, logical(1))
  left_to_toml <- setdiff(ISOLATION_HEADERS, isolation)
  note <- c(
    if (length(left_to_toml) > 0L) {
      c(
        "# Left to the project's hand-written netlify.toml, which sets these itself:",
        paste0("#   ", left_to_toml),
        "# Netlify reads both files and joins the values of a header set twice. A",
        "# merged `same-origin, same-origin` is not a valid COOP token and would",
        "# silently drop crossOriginIsolated - and with it SharedArrayBuffer.",
        ""
      )
    },
    if (any(dropped)) {
      c(
        "# Left out because the project's netlify.toml sets headers for them:",
        paste0("#   ", vapply(rules[dropped], `[[`, character(1), "path")),
        ""
      )
    }
  )
  body <- unlist(
    lapply(rules[!(empty | dropped)], function(rule) {
      c(
        if (!is.null(rule$comment)) paste("#", rule$comment),
        rule$path,
        paste0("  ", names(rule$headers), ": ", unname(rule$headers)),
        ""
      )
    }),
    use.names = FALSE
  )
  c(header, note, body)
}

#' netlify.toml for a webrarian site: the publish directory only
#'
#' Every response header a webrarian site needs, cross-origin isolation and
#' caching alike, is in the site's own _headers file, which Netlify reads on a
#' CLI deploy and on a drag-and-drop. Netlify also reads netlify.toml and
#' concatenates the values of a header both files set, and a merged
#' `same-origin, same-origin` is not a valid COOP token, so this file sets
#' none. There is no build command: Netlify's build image has no R, so the
#' site is built elsewhere and uploaded as it is.
#' @param output_dir The output directory, relative to the collection root
#'   (workflow_output_dir(), which allows only characters that need no quoting).
#' @noRd
netlify_toml_content <- function(output_dir) {
  c(
    "# Generated by webrarian::circulate_via_netlify().",
    "#",
    "# The site is built by the GitHub Actions workflow next to this file (or by",
    "# bind() on your machine) and uploaded as it is: Netlify's build image has",
    "# no R, so there is no build command. Every response header the site needs,",
    "# cross-origin isolation (COOP/COEP) and caching alike, is in the _headers",
    "# file bind() writes into the site. If you add a headers table here,",
    "# bind() leaves those headers out of _headers, so no header is set twice.",
    "",
    "[build]",
    sprintf('  publish = "%s"', output_dir)
  )
}

#' Drop TOML comments: a `#` outside a quoted string and everything after it
#' @noRd
strip_toml_comments <- function(lines) {
  vapply(
    lines,
    function(line) {
      chars <- strsplit(line, "", fixed = TRUE)[[1]]
      quote <- ""
      i <- 1L
      while (i <= length(chars)) {
        ch <- chars[[i]]
        if (nzchar(quote)) {
          # A backslash escapes the next character in a basic ("...") string.
          if (identical(quote, "\"") && identical(ch, "\\")) {
            i <- i + 2L
            next
          }
          if (identical(ch, quote)) quote <- ""
        } else if (ch %in% c("\"", "'")) {
          quote <- ch
        } else if (identical(ch, "#")) {
          return(paste(chars[seq_len(i - 1L)], collapse = ""))
        }
        i <- i + 1L
      }
      line
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' The headers each [[headers]] table of one or more netlify.toml files sets
#'
#' The file circulate_via_netlify() writes has no such table; a hand-written
#' one may. A table runs from its `[[headers]]` line to the next table header
#' other than its own `[headers.values]`. Its headers are the keys of its
#' `values`, written as a `[headers.values]` table, an inline
#' `values = { ... }` or dotted `values.<Name> = ...` keys. Comments are
#' dropped first, so a header named only in a comment does not count.
#' @param tomls Paths of netlify.toml files; missing ones are skipped.
#' @return A named list: for each `for = "<path>"`, the lowercased names of
#'   the headers its table(s) set, across every file. Empty when no file has
#'   such a table.
#' @noRd
netlify_header_blocks <- function(tomls) {
  key <- "(\"[^\"]*\"|'[^']*'|[A-Za-z0-9_-]+)"
  values_header <- "^\\s*\\[\\s*headers\\s*\\.\\s*values\\s*\\]"
  unquote <- function(x) tolower(gsub("^[\"']|[\"']$", "", x))
  out <- list()
  for (toml in tomls[fs::file_exists(tomls)]) {
    lines <- strip_toml_comments(readLines(toml, warn = FALSE))
    starts <- grep("^\\s*\\[\\[\\s*headers\\s*\\]\\]", lines)
    tables <- grep("^\\s*\\[", lines)
    ends_at <- tables[!grepl(values_header, lines[tables])]
    for (start in starts) {
      later <- ends_at[ends_at > start]
      end <- if (length(later) > 0L) later[[1]] - 1L else length(lines)
      block <- lines[start:end]
      hit <- regmatches(block, regexec("^\\s*for\\s*=\\s*[\"']([^\"']+)[\"']", block))
      path <- unlist(lapply(hit, function(m) if (length(m) == 2L) m[[2]]))
      if (length(path) == 0L) {
        next
      }
      set <- character()
      in_values <- FALSE
      for (line in block[-1L]) {
        if (grepl("^\\s*\\[", line)) {
          in_values <- grepl(values_header, line)
          next
        }
        if (in_values) {
          m <- regmatches(line, regexec(paste0("^\\s*", key, "\\s*="), line))[[1]]
          if (length(m) == 2L) {
            set <- c(set, unquote(m[[2]]))
          }
          next
        }
        m <- regmatches(line, regexec(paste0("^\\s*values\\s*\\.\\s*", key, "\\s*="), line))[[1]]
        if (length(m) == 2L) {
          set <- c(set, unquote(m[[2]]))
          next
        }
        inline <- regmatches(line, regexec("^\\s*values\\s*=\\s*\\{(.*)\\}\\s*$", line))[[1]]
        if (length(inline) == 2L) {
          # Blank out the quoted values first: a `=` inside one
          # ("max-age=600") is not a key.
          body <- gsub("=\\s*(\"([^\"\\\\]|\\\\.)*\"|'[^']*')", "= ''", inline[[2]], perl = TRUE)
          keys <- regmatches(body, gregexpr(paste0("(^|,)\\s*", key, "\\s*="), body))[[1]]
          keys <- sub(paste0("^,?\\s*", key, "\\s*=$"), "\\1", keys)
          set <- c(set, unquote(trimws(keys)))
        }
      }
      out[[path[[1]]]] <- unique(c(out[[path[[1]]]], set))
    }
  }
  out
}

#' Which of COOP and COEP one or more netlify.toml files set themselves
#'
#' _headers leaves each of these to the toml: Netlify would join the two
#' values into `same-origin, same-origin` (or `require-corp, require-corp`),
#' which is not valid, and the page would silently lose cross-origin
#' isolation. The one it does not set stays in _headers.
#' @return The subset of `ISOLATION_HEADERS`, in its spelling, that the
#'   `values` of any `[[headers]]` table set (header names are matched in any
#'   case); `character()` for none.
#' @noRd
netlify_sets_isolation <- function(tomls) {
  set <- unlist(netlify_header_blocks(tomls), use.names = FALSE)
  ISOLATION_HEADERS[tolower(ISOLATION_HEADERS) %in% set]
}

#' Paths of cache_rules() that a netlify.toml gives one of the same headers
#'
#' Netlify joins the values of a header set for the same path in both files
#' (`immutable, ..., immutable`), so _headers leaves these out. Only the exact
#' path is matched, as in pyodidarian.
#' @noRd
netlify_duplicated_paths <- function(tomls) {
  blocks <- netlify_header_blocks(tomls)
  rules <- cache_rules(include_isolation = FALSE)
  duplicated <- vapply(
    rules,
    function(rule) {
      any(tolower(names(rule$headers)) %in% blocks[[rule$path]])
    },
    logical(1)
  )
  vapply(rules[duplicated], `[[`, character(1), "path")
}

#' The netlify.toml files Netlify may read for a collection
#'
#' Its own and, for a collection (a directory holding _webrarian.yml) in a
#' subdirectory of a git repository, the repository root's: Netlify reads
#' netlify.toml from the site's base directory, which is the repository root
#' unless it is set in Netlify's UI (which no file records). Only the files
#' that exist are returned. Any other directory, such as a mirror's, has only
#' its own read.
#' @noRd
netlify_tomls <- function(root) {
  dirs <- fs::path_real(root)
  # Only a collection is deployed from its repository. collection_mirror() passes
  # its own directory, and a mirror is self-contained: no other netlify.toml changes it.
  repo <- if (is_collection(root)) repo_root(root)
  if (!is.null(repo) && !identical(as.character(repo), as.character(dirs))) {
    dirs <- c(dirs, repo)
  }
  tomls <- fs::path(dirs, "netlify.toml")
  as.character(tomls[fs::file_exists(tomls)])
}

#' Write the `_headers` file into the built site
#' @noRd
emit_cache_headers <- function(output_dir, root = ".") {
  # _headers carries every response header the site needs: Netlify reads it
  # on a CLI deploy and on a drag-and-drop alike, and circulate_via_netlify()'s
  # netlify.toml sets none. A hand-written netlify.toml may set some, and
  # Netlify concatenates the values of a header both files set for a path (a
  # merged `Cross-Origin-Opener-Policy: same-origin, same-origin` is not a
  # valid token: the browser falls back to `unsafe-none`, quietly killing
  # crossOriginIsolated and SharedArrayBuffer). So what such a file sets is
  # read from its content and left out here, one header at a time: each of
  # COOP and COEP that it sets itself (the other stays), and every other rule
  # whose header it also sets for the same path. Both the collection's own
  # netlify.toml and its git repository root's are read (netlify_tomls()); a
  # mirror reads only its own.
  tomls <- netlify_tomls(root)
  in_toml <- netlify_sets_isolation(tomls)
  toml_paths <- netlify_duplicated_paths(tomls)

  path <- fs::path(output_dir, "_headers")
  writeLines(
    cache_headers_content(
      drop_paths = toml_paths,
      isolation = setdiff(ISOLATION_HEADERS, in_toml)
    ),
    path
  )

  if (length(in_toml) > 0L || length(toml_paths) > 0L) {
    cli::cli_alert_success(
      "Wrote {.file _headers}, leaving out what the project's {.file netlify.toml} sets"
    )
  } else {
    cli::cli_alert_success("Wrote {.file _headers}")
  }
  invisible(path)
}

# ============================================================================
# UI Asset Functions
# ============================================================================

#' Copy UI assets to output directory
#' @noRd
copy_ui_assets <- function(config, root, output_dir) {
  ui <- config$ui
  assets_dir <- fs::path(output_dir, "assets", "custom")

  # Track if we copied anything
  copied <- FALSE

  # Copy favicon (check brand source first, then relative path)
  if (!is.null(ui$.favicon)) {
    # Brand source (absolute path from _brand.yml)
    src <- ui$.brand_favicon
    if (is.null(src) || !fs::file_exists(src)) {
      # Fallback to relative path from project root
      src <- fs::path(root, ui$.favicon)
    }
    if (fs::file_exists(src)) {
      ensure_dir(assets_dir)
      ext <- tolower(fs::path_ext(src))
      fs::file_copy(src, fs::path(assets_dir, paste0("favicon.", ext)), overwrite = TRUE)
      copied <- TRUE
    } else {
      cli::cli_alert_warning("Favicon not found: {.path {ui$.favicon}}")
    }
  }

  # Copy logo (check brand source first, then relative path)
  if (!is.null(ui$.logo)) {
    # Brand source (absolute path from _brand.yml)
    src <- ui$.brand_logo
    if (is.null(src) || !fs::file_exists(src)) {
      # Fallback to relative path from project root
      src <- fs::path(root, ui$.logo)
    }
    if (fs::file_exists(src)) {
      ensure_dir(assets_dir)
      ext <- fs::path_ext(src)
      fs::file_copy(src, fs::path(assets_dir, paste0("logo.", ext)), overwrite = TRUE)
      copied <- TRUE
    } else {
      cli::cli_alert_warning("Logo not found: {.path {ui$.logo}}")
    }
  }

  # The brand's logo for the dark scheme, when it has another one
  if (!is.null(ui$.logo_dark)) {
    src <- dark_logo_source(ui, root)
    if (!is.null(src)) {
      ensure_dir(assets_dir)
      ext <- fs::path_ext(src)
      fs::file_copy(src, fs::path(assets_dir, paste0("logo-dark.", ext)), overwrite = TRUE)
      copied <- TRUE
    } else {
      cli::cli_alert_warning("Logo not found: {.path {ui$.logo_dark}}")
    }
  }

  # Copy custom CSS
  if (!is.null(ui$custom_css)) {
    src <- fs::path(root, ui$custom_css)
    if (fs::file_exists(src)) {
      ensure_dir(assets_dir)
      fs::file_copy(src, fs::path(assets_dir, "custom.css"), overwrite = TRUE)
      copied <- TRUE
    } else {
      cli::cli_alert_warning("Custom CSS not found: {.path {ui$custom_css}}")
    }
  }

  # Copy OG image
  if (!is.null(ui$meta$og_image)) {
    src <- fs::path(root, ui$meta$og_image)
    if (fs::file_exists(src)) {
      ensure_dir(assets_dir)
      ext <- tolower(fs::path_ext(src))
      fs::file_copy(src, fs::path(assets_dir, paste0("og-image.", ext)), overwrite = TRUE)
      copied <- TRUE
    }
  }

  if (copied) {
    cli::cli_alert_success("Copied UI assets")
  }

  invisible(copied)
}

#' Generate UI head tags
#'
#' Text and attributes are HTML-escaped; anything placed in CSS is validated
#' as a CSS token (R/css.R).
#' @noRd
generate_ui_head <- function(config, root = ".") {
  ui <- config$ui
  tags <- character()

  if (!is.null(ui$.favicon)) {
    ext <- tolower(fs::path_ext(ui$.favicon))
    mime_type <- switch(
      ext,
      "ico" = "image/x-icon",
      "svg" = "image/svg+xml",
      "png" = "image/png",
      "gif" = "image/gif",
      "image/x-icon"
    )
    tags <- c(
      tags,
      sprintf(
        '<link rel="icon" type="%s" href="assets/custom/favicon.%s" />',
        mime_type,
        html_escape(ext)
      )
    )
  } else {
    # Without one, every browser requests /favicon.ico and logs a 404.
    tags <- c(tags, default_favicon_link())
  }

  if (!is.null(ui$custom_css)) {
    tags <- c(tags, '<link rel="stylesheet" href="assets/custom/custom.css" />')
  }

  if (!is.null(config$.brand)) {
    brand_fonts <- generate_brand_fonts_head(config$.brand, offline = isTRUE(config$build$offline))
    if (nzchar(brand_fonts)) {
      tags <- c(tags, brand_fonts)
    }
  }
  brand_vars <- brand_css_variables(config$.brand)
  if (nzchar(brand_vars)) {
    tags <- c(tags, brand_vars)
  }

  font_family <- css_font_family(ui$.font_family, "brand.typography.monospace.family")
  font_size <- css_number(ui$.font_size, "brand.typography.monospace.size")
  font_styles <- c(
    if (!is.null(font_family)) {
      sprintf("font-family: %s;", css_font_stack(font_family, "monospace"))
    },
    if (!is.null(font_size)) sprintf("font-size: %spx;", css_number_string(font_size))
  )
  if (length(font_styles) > 0) {
    tags <- c(
      tags,
      sprintf(
        "<style>.cm-editor { %s }</style>",
        paste(font_styles, collapse = " ")
      )
    )
  }

  title <- ui$meta$title %||% config$project$name
  description <- ui$meta$description %||% config$project$description %||% ""
  tags <- c(tags, sprintf('<meta property="og:title" content="%s" />', html_escape(title)))
  if (nzchar(description)) {
    tags <- c(tags, sprintf('<meta name="description" content="%s" />', html_escape(description)))
    tags <- c(
      tags,
      sprintf('<meta property="og:description" content="%s" />', html_escape(description))
    )
  }
  tags <- c(
    tags,
    sprintf(
      '<meta property="og:type" content="%s" />',
      html_escape(ui$meta$og_type %||% "website")
    )
  )

  site_url <- site_url_or_null(ui$meta$site_url)
  if (!is.null(site_url)) {
    tags <- c(tags, sprintf('<meta property="og:url" content="%s/" />', html_escape(site_url)))
  }
  if (!is.null(ui$meta$og_image)) {
    ext <- tolower(fs::path_ext(ui$meta$og_image))
    if (!fs::file_exists(fs::path(root, ui$meta$og_image))) {
      cli::cli_warn(
        "{.field ui.meta.og-image} {.file {ui$meta$og_image}} does not exist; no og:image tag written."
      )
    } else if (is.null(site_url)) {
      cli::cli_warn(c(
        "{.field ui.meta.og-image} needs {.field ui.meta.site-url}; no og:image tag written.",
        "i" = "Link previews only load absolute image URLs."
      ))
    } else {
      tags <- c(
        tags,
        sprintf(
          '<meta property="og:image" content="%s/assets/custom/og-image.%s" />',
          html_escape(site_url),
          html_escape(ext)
        )
      )
    }
  }

  tags <- c(
    tags,
    sprintf(
      '<meta name="twitter:card" content="%s" />',
      html_escape(ui$meta$twitter_card %||% "summary")
    )
  )

  paste(tags, collapse = "\n  ")
}

#' The file of the brand's logo for the dark scheme, or NULL
#'
#' The brand's own path first, then the path relative to the collection, as
#' the light logo is looked for. NULL when the brand has no second logo, or
#' its file is not there: the site then has one logo, for both schemes.
#' @param ui Settings with the brand applied (`.logo_dark`, `.brand_logo_dark`).
#' @param root The collection's directory.
#' @noRd
dark_logo_source <- function(ui, root) {
  if (is.null(ui$.logo_dark)) {
    return(NULL)
  }
  candidates <- c(ui$.brand_logo_dark, fs::path(root, ui$.logo_dark))
  found <- candidates[fs::file_exists(candidates)]
  if (length(found) == 0L) NULL else found[[1]]
}

# Note: generate_loading_styles() and generate_loading_html() are now in R/html.R

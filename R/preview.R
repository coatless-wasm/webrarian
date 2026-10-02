# Local preview server
#
# Files are served by httpuv's staticPaths, which run on httpuv's own I/O
# thread: the preview keeps answering while the R session is busy (a bind(),
# a long computation) or blocked (a test waiting on a browser). HEAD gets
# headers only, paths are URL-decoded, `..` is refused and directories are not
# listed. Without watch mode nothing falls through to an R request handler,
# which could not run while R is busy. reading_room(watch = TRUE) hands pages
# and the build id to R (watch_response()), which the watch loop answers
# between polls (preview_app(), watch_serve_for()).

#' Open the reading room (preview server)
#'
#' Serves a built site, or a mirror, on a local port with the cross-origin
#' isolation headers webR needs. The server runs on httpuv's own thread, so it
#' keeps answering while R is busy. Requires the httpuv package.
#'
#' @param path A collection or any directory inside one, whose built site
#'   (`build.output-dir`) is served, or any other directory holding an
#'   `index.html`, such as a mirror from [collection_mirror()], which is
#'   served as it is.
#' @param port Port number. `NULL` (the default) picks a free port. A port
#'   that is already in use is an error.
#' @param watch If `TRUE`, build the collection, serve it, and rebuild it
#'   whenever `_webrarian.yml` or an included file changes. A change to the
#'   included files only updates `vfs-files/` and the page's file list, and
#'   any other change runs a full [bind()]. The open page reloads itself after a
#'   successful rebuild (a small script added when the page is served, never
#'   written to the site). A failed rebuild keeps the last good site and is
#'   tried again after the next change. Needs `block = TRUE`, because the
#'   watch loop, which also answers the page's reload checks, runs in the
#'   console.
#' @param block If `TRUE` (the default), keep the console busy until you press
#'   Ctrl+C, then stop the server. If `FALSE`, return at once, and the server
#'   keeps running until [reading_room_close()] or the room's `close()`.
#' @param open_browser If `TRUE`, open the preview in the default browser.
#'   Defaults to `TRUE` in interactive sessions only.
#'
#' @return Invisibly, the reading room. Its `url` and `port` elements give the
#'   address, and its `close()` element stops it. Previewing the same site again
#'   stops the earlier preview and says so.
#'
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("httpuv", quietly = TRUE)) {
#'   collection <- file.path(tempdir(), "webrarian-preview-demo")
#'   catalog(collection)
#'   bind(collection)
#'
#'   # Serve in the background and keep the console free
#'   room <- reading_room(collection, block = FALSE)
#'   room$url
#'   room$close()
#'
#'   # Rebuild on every change, and reload the page, until you press Ctrl+C
#'   reading_room(collection, watch = TRUE)
#' }
reading_room <- function(
  path = ".",
  port = NULL,
  watch = FALSE,
  block = TRUE,
  open_browser = rlang::is_interactive()
) {
  for (arg in c("watch", "block", "open_browser")) {
    value <- get(arg, inherits = FALSE)
    if (!isTRUE(value) && !isFALSE(value)) {
      cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
    }
  }
  if (watch && !block) {
    cli::cli_abort(c(
      "{.code watch = TRUE} needs {.code block = TRUE}.",
      "i" = "The watch loop runs in the console. Use {.code block = FALSE} without {.arg watch} to keep the console free."
    ))
  }

  target <- reading_room_target(path, watch)
  if (watch) {
    return(invisible(watch_collection(target$root, port = port, open_browser = open_browser)))
  }
  if (!fs::file_exists(fs::path(target$site, "index.html"))) {
    cli::cli_abort(c(
      "No built site at {.path {target$site}}.",
      "i" = "Run {.run webrarian::bind()} first, or use {.code watch = TRUE}."
    ))
  }

  server <- serve_site(as.character(fs::path_real(target$site)), port)
  if (open_browser) {
    open_in_browser(server$url)
  }

  if (block) {
    cli::cli_text("Press {.kbd Ctrl+C} to stop the server")
    tryCatch(
      repeat {
        httpuv::service(250)
      },
      interrupt = function(cnd) NULL,
      finally = {
        if (stop_static_server(server)) {
          cli::cli_alert_info("Stopped the preview server")
        }
      }
    )
  } else {
    cli::cli_text("Run {.run webrarian::reading_room_close()} to stop")
  }

  invisible(server)
}

#' Close the reading room (stop preview server)
#'
#' @param room A reading room from [reading_room()]. If `NULL`, stops every
#'   running preview.
#'
#' @return Invisibly returns `NULL`, called for its side effect of stopping
#'   the preview server.
#'
#' @export
#'
#' @examples
#' if (interactive() && requireNamespace("httpuv", quietly = TRUE)) {
#'   collection <- file.path(tempdir(), "webrarian-close-demo")
#'   catalog(collection)
#'   bind(collection)
#'   room <- reading_room(collection, block = FALSE, open_browser = FALSE)
#'   reading_room_close(room)
#' }
reading_room_close <- function(room = NULL) {
  rooms <- if (is.null(room)) unname(webrarian_env$servers) else list(room)
  if (length(rooms) == 0L) {
    cli::cli_alert_info("No preview server is running")
    return(invisible())
  }
  for (r in rooms) {
    if (!inherits(r, "webrarian_server")) {
      cli::cli_abort("{.arg room} must be a reading room returned by {.fn reading_room}.")
    }
    if (stop_static_server(r)) {
      cli::cli_alert_success("Stopped the preview at {.url {r$url}}")
    } else {
      cli::cli_alert_info("The preview at {.url {r$url}} was already stopped")
    }
  }
  invisible()
}

#' Print a reading room
#'
#' @param x A reading room from [reading_room()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.webrarian_server <- function(x, ...) {
  state <- if (isTRUE(x$stopped)) "stopped" else "serving"
  cli::cli_text("Reading room {.url {x$url}} ({state} {.path {x$dir}})")
  invisible(x)
}

#' What reading_room() serves
#'
#' A directory with its own `_webrarian.yml` is a collection. Any other
#' directory with an `index.html` (a mirror, a built site) is served as it is,
#' except for `watch = TRUE`, which needs a collection. Otherwise `path` must
#' be inside a collection.
#' @return `list(site, root)`; `root` is `NULL` for a plain directory.
#' @noRd
reading_room_target <- function(path, watch = FALSE, call = rlang::caller_env()) {
  path <- fs::path_abs(path)
  if (!fs::dir_exists(path)) {
    cli::cli_abort("{.path {path}} does not exist.", call = call)
  }
  own_config <- fs::file_exists(fs::path(path, "_webrarian.yml"))
  has_index <- fs::file_exists(fs::path(path, "index.html"))
  if (!own_config && !watch && has_index) {
    return(list(site = as.character(fs::path_real(path)), root = NULL))
  }
  root <- collection_root(path)
  if (is.null(root) && watch && has_index) {
    cli::cli_abort(
      c(
        "{.code watch = TRUE} needs a collection (a directory with {.file _webrarian.yml}), and {.path {path}} is not in one.",
        "i" = "A built site or a mirror is previewed with {.code watch = FALSE}."
      ),
      call = call
    )
  }
  if (is.null(root)) {
    cli::cli_abort(
      c(
        "{.path {path}} is not in a webrarian collection and has no {.file index.html}.",
        "i" = "Pass a collection, a built site or a mirror directory."
      ),
      call = call
    )
  }
  config <- collection_settings(root)
  list(
    site = as.character(resolve_output_dir(root, config$build$output_dir)),
    root = as.character(root)
  )
}

#' Serve a site directory, replacing an earlier preview of the same site
#' @noRd
serve_site <- function(site, port = NULL, watch = FALSE) {
  previous <- webrarian_env$servers[[site]]
  if (!is.null(previous) && !isTRUE(previous$stopped)) {
    stop_static_server(previous)
    cli::cli_alert_info("Stopped the previous preview of this site at {.url {previous$url}}")
  }
  server <- start_static_server(site, port = port, watch = watch)
  webrarian_env$servers[[site]] <- server
  cli::cli_alert_success("Preview server running at {.url {server$url}}")
  server
}

#' Headers on every preview response: cross-origin isolation for webR
#' @noRd
preview_headers <- function() {
  list(
    "Cross-Origin-Opener-Policy" = "same-origin",
    "Cross-Origin-Embedder-Policy" = "require-corp",
    "Cross-Origin-Resource-Policy" = "same-origin"
  )
}

#' Open a URL in the browser (a function so tests can replace it)
#' @noRd
open_in_browser <- function(url) {
  utils::browseURL(url)
}

#' The httpuv app behind a preview
#'
#' Static files come from httpuv's own thread, so a preview answers while R
#' is busy. With `watch = TRUE`, directory requests (`/`, `/LICENSES/`),
#' `/index.html` and anything that is not a file (the build id) are handed to
#' R instead: watch_response() adds the reload script to pages. The watch
#' loop answers them between polls (watch_serve_for()). Static files in watch
#' mode are `no-cache`, so a reloaded page fetches edited files again.
#' @noRd
preview_app <- function(dir, watch = FALSE) {
  if (!watch) {
    return(list(
      call = function(req) {
        list(
          status = 404L,
          headers = list("Content-Type" = "text/plain; charset=utf-8"),
          body = "Not Found"
        )
      },
      staticPaths = list(
        "/" = httpuv::staticPath(
          dir,
          indexhtml = TRUE,
          fallthrough = FALSE,
          # no-store: httpuv's staticPath answers a conditional
          # If-Modified-Since from the file's mtime on its own, and without
          # this a browser could revalidate against a stale 304 when two
          # builds land within the same mtime second (the service worker
          # on/off test and a fast reading_room() rebuild both can do this).
          headers = c(preview_headers(), list("Cache-Control" = "no-store"))
        )
      )
    ))
  }
  list(
    call = function(req) watch_response(dir, req$PATH_INFO),
    staticPaths = list(
      "/" = httpuv::staticPath(
        dir,
        indexhtml = FALSE,
        fallthrough = TRUE,
        headers = c(preview_headers(), "Cache-Control" = "no-cache")
      ),
      "/index.html" = httpuv::excludeStaticPath()
    )
  )
}

#' The path the watch-mode page polls for the build id (pyodidarian's too)
#' @noRd
watch_build_id_path <- function() "/__reading_room__/build-id"

#' The script reading_room(watch = TRUE) adds to each page it serves
#'
#' It polls watch_build_id_path() once a second and reloads the page when the
#' id changes. Added at serve time, never written to disk. Character for
#' character pyodidarian's RELOAD_SCRIPT.
#' @noRd
watch_reload_script <- function() {
  paste0(
    "<script>(function () { var id = null; function poll() {",
    " fetch(\"",
    watch_build_id_path(),
    "\", { cache: \"no-store\" })",
    ".then(function (r) { return r.text(); }).then(function (t) {",
    " if (id === null) { id = t; } else if (t !== id) { location.reload(); return; }",
    " setTimeout(poll, 1000); }).catch(function () { setTimeout(poll, 1000); }); }",
    " poll(); })();</script>"
  )
}

#' The build id the watch-mode page polls
#'
#' The number of successful rebuilds in this watch session, as text ("0"
#' before any), which pyodidarian's server counts the same way. watch_step()
#' adds one when a rebuild succeeds and watch_collection() records it here
#' after every poll (watch_record_builds()); a failed rebuild leaves it alone.
#' A hash of the page would not do: new contents in an included file leave
#' index.html byte for byte the same, since its file list holds only names
#' and paths.
#' @noRd
watch_build_id <- function(site) {
  as.character(webrarian_env$watch_builds[[site]] %||% 0L)
}

#' Record the rebuild count watch_build_id() answers for a site
#'
#' `builds = NULL` removes the site's entry (when watching stops).
#' @noRd
watch_record_builds <- function(site, builds) {
  webrarian_env$watch_builds[[site]] <- builds
  invisible(builds)
}

#' Answer a request the watch-mode preview hands to R
#'
#' The build id, or an HTML page inside the site with the reload script added
#' before `</body>`. Anything else that reaches R (a file that does not
#' exist, or a path outside the site) is a 404.
#' @noRd
watch_response <- function(site, path_info) {
  respond <- function(status, type, body) {
    list(
      status = status,
      headers = c(preview_headers(), list("Content-Type" = type, "Cache-Control" = "no-store")),
      body = body
    )
  }
  not_found <- respond(404L, "text/plain; charset=utf-8", "Not Found")
  path <- tryCatch(
    utils::URLdecode(path_info %||% "/"),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  if (is.null(path)) {
    return(not_found)
  }
  if (identical(path, watch_build_id_path())) {
    return(respond(200L, "text/plain; charset=utf-8", watch_build_id(site)))
  }
  if (endsWith(path, "/")) {
    path <- paste0(path, "index.html")
  }
  file <- fs::path(site, sub("^/+", "", path))
  if (
    !grepl("\\.html$", path) ||
      !fs::file_exists(file) ||
      fs::dir_exists(file) ||
      !path_strictly_inside(fs::path_real(file), site)
  ) {
    return(not_found)
  }
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  script <- watch_reload_script()
  html <- if (grepl("</body>", html, fixed = TRUE)) {
    sub("</body>", paste0(script, "</body>"), html, fixed = TRUE)
  } else {
    paste0(html, script)
  }
  respond(200L, "text/html; charset=utf-8", enc2utf8(html))
}

#' Serve a directory of static files
#' @return A `webrarian_server` environment: `url`, `port`, `host`, `dir`,
#'   `stopped`, `close()`, and the httpuv handle `server`.
#' @noRd
start_static_server <- function(
  dir,
  port = NULL,
  host = "127.0.0.1",
  call = rlang::caller_env(),
  watch = FALSE
) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "httpuv package is required for preview",
        "i" = "Install it with: {.code install.packages(\"httpuv\")}"
      ),
      call = call
    )
  }
  dir <- as.character(fs::path_real(dir))
  explicit <- !is.null(port)
  if (is.null(port)) {
    port <- httpuv::randomPort(min = 8000, max = 9000, host = host)
  }
  port <- as.integer(port)

  handle <- tryCatch(
    httpuv::startServer(host, port, preview_app(dir, watch)),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not start the preview server on {host}:{port}.",
          "x" = conditionMessage(e),
          "i" = if (explicit) {
            "Something else holds that port; use {.code port = NULL} to pick a free one."
          }
        ),
        call = call
      )
    }
  )

  server <- new.env(parent = emptyenv())
  server$server <- handle
  server$host <- host
  server$port <- port
  server$url <- sprintf("http://%s:%d", host, port)
  server$dir <- dir
  server$stopped <- FALSE
  server$close <- function() invisible(stop_static_server(server))
  class(server) <- "webrarian_server"
  server
}

#' Stop a server from start_static_server()
#' @return `TRUE` if it was running, invisibly.
#' @noRd
stop_static_server <- function(server) {
  if (isTRUE(server$stopped)) {
    return(invisible(FALSE))
  }
  tryCatch(httpuv::stopServer(server$server), error = function(e) NULL)
  server$stopped <- TRUE
  if (identical(webrarian_env$servers[[server$dir]], server)) {
    webrarian_env$servers[[server$dir]] <- NULL
  }
  invisible(TRUE)
}

# Package environment for storing server state
webrarian_env <- new.env(parent = emptyenv())

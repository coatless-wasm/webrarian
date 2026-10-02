# Serve a built site for the browser tests with the package's own preview
# server (R/preview.R). It serves files from httpuv's I/O thread, so it keeps
# answering while chromote blocks the R session waiting on the browser; the
# old out-of-process `call`-handler server (which also answered HEAD with a
# body) is no longer needed.
serve_dir_isolated <- function(dir, port) {
  start_static_server(dir, port = port)
}

# Stop a server started by serve_dir_isolated() or
# serve_dir_isolated_with_content_type_override().
stop_dir_server <- function(server) {
  if (is.null(server)) {
    return(invisible())
  }
  if (inherits(server, "webrarian_server")) {
    stop_static_server(server)
  } else if (inherits(server, "process") && server$is_alive()) {
    server$kill()
  }
  invisible()
}

# Like serve_dir_isolated(), but a 200 response for a path in `overrides`
# (a named list/vector, e.g. c("/exlibris-r.js" = "text/html; charset=utf-8"))
# is served with that Content-Type instead of the one its extension implies.
# Used to pin that a wrong-MIME-type 200 response for the viewer bundle fails
# a module script's fetch the same way a 404 does (both are terminal per the
# HTML module-script-fetch algorithm's strict MIME-type check), not just the
# 404 case test-browser-splash.R otherwise covers.
#
# httpuv's staticPath() `headers` argument only *adds* headers alongside the
# one it guesses from the file extension rather than replacing it (verified
# empirically: a Content-Type in `headers` shows up as a second Content-Type
# header, not a substitute for the first), so there is no way to force a
# single file's Content-Type through start_static_server(). This one case
# stays on the pre-Task-15 out-of-process `call` handler.
serve_dir_isolated_with_content_type_override <- function(dir, port, overrides) {
  process <- callr::r_bg(
    function(dir, port, overrides) {
      get_content_type <- function(path) {
        ext <- tolower(tools::file_ext(path))
        types <- list(
          html = "text/html; charset=utf-8",
          htm = "text/html; charset=utf-8",
          css = "text/css; charset=utf-8",
          js = "text/javascript; charset=utf-8",
          mjs = "text/javascript; charset=utf-8",
          json = "application/json",
          wasm = "application/wasm",
          png = "image/png",
          jpg = "image/jpeg",
          jpeg = "image/jpeg",
          gif = "image/gif",
          svg = "image/svg+xml",
          ico = "image/x-icon",
          txt = "text/plain; charset=utf-8",
          csv = "text/csv; charset=utf-8",
          r = "text/plain; charset=utf-8",
          rds = "application/octet-stream",
          tar = "application/x-tar",
          gz = "application/gzip",
          data = "application/octet-stream",
          map = "application/json"
        )
        ct <- types[[ext]]
        if (is.null(ct)) "application/octet-stream" else ct
      }

      httpuv::runServer(
        "127.0.0.1",
        port,
        list(
          call = function(req) {
            path <- utils::URLdecode(req$PATH_INFO)
            if (path == "/" || path == "") {
              path <- "/index.html"
            }
            file <- file.path(dir, sub("^/", "", path))
            if (!file.exists(file) || dir.exists(file)) {
              return(list(
                status = 404L,
                headers = list("Content-Type" = "text/plain"),
                body = "Not found"
              ))
            }
            content_type <- if (!is.null(overrides[[path]])) {
              overrides[[path]]
            } else {
              get_content_type(file)
            }
            list(
              status = 200L,
              headers = list(
                "Content-Type" = content_type,
                "Cross-Origin-Opener-Policy" = "same-origin",
                "Cross-Origin-Embedder-Policy" = "require-corp",
                "Cross-Origin-Resource-Policy" = "cross-origin"
              ),
              body = readBin(file, "raw", n = file.info(file)$size)
            )
          }
        )
      )
    },
    args = list(dir = dir, port = port, overrides = overrides),
    supervise = TRUE
  )

  deadline <- Sys.time() + 15
  repeat {
    ready <- tryCatch(
      suppressWarnings({
        con <- socketConnection("127.0.0.1", port, timeout = 1)
        close(con)
        TRUE
      }),
      error = function(e) FALSE
    )
    if (ready) {
      break
    }
    if (!process$is_alive()) {
      stop(
        "Background static server process exited unexpectedly:\n",
        paste(process$read_all_error(), collapse = "\n")
      )
    }
    if (Sys.time() > deadline) {
      process$kill()
      stop("Timed out waiting for background static server to start on port ", port)
    }
    Sys.sleep(0.2)
  }

  process
}

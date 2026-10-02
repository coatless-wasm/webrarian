# The preview server: static files from httpuv's I/O thread,
# isolation headers, HEAD without a body, decoded paths, no traversal, and
# clear handling of a second preview or a busy port.

skip_if_not_installed("httpuv")
skip_if_not_installed("curl")

# writeLines() writes CRLF on Windows; the served bytes are compared exactly.
write_lf <- function(text, path) {
  writeBin(charToRaw(enc2utf8(paste0(text, "\n"))), path)
}

local_site <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  writeLines("secret", fs::path(base, "secret.txt"))
  dir <- fs::path(base, "site")
  fs::dir_create(fs::path(dir, "vfs-files", "data"))
  writeLines("<html><body>site</body></html>", fs::path(dir, "index.html"))
  writeBin(as.raw(c(0x00, 0x61, 0x73, 0x6d)), fs::path(dir, "engine.wasm"))
  write_lf("a,b", fs::path(dir, "vfs-files", "data", "my survey.csv"))
  dir
}

fetch <- function(url, method = "GET") {
  h <- curl::new_handle(
    customrequest = method,
    nobody = identical(method, "HEAD"),
    path_as_is = TRUE
  )
  r <- curl::curl_fetch_memory(url, handle = h)
  list(status = r$status_code, headers = curl::parse_headers_list(r$headers), body = r$content)
}

local_preview <- function(dir, env = parent.frame(), ...) {
  server <- suppressMessages(reading_room(dir, open_browser = FALSE, block = FALSE, ...))
  withr::defer(suppressMessages(reading_room_close(server)), envir = env)
  server
}

test_that("files are served with the isolation headers and no wildcard CORS", {
  server <- local_preview(local_site())
  r <- fetch(paste0(server$url, "/"))
  expect_equal(r$status, 200L)
  expect_equal(r$headers[["cross-origin-opener-policy"]], "same-origin")
  expect_equal(r$headers[["cross-origin-embedder-policy"]], "require-corp")
  expect_equal(r$headers[["cross-origin-resource-policy"]], "same-origin")
  expect_null(r$headers[["access-control-allow-origin"]])
})

# Raw bytes off one keep-alive connection. curl's HEAD (nobody = TRUE) never
# reads a body, so it cannot see the HEAD-with-body bug: a HEAD answered with a body,
# whose bytes the browser then reads as the start of the next response on the
# same connection.
headers_end <- function(buf) {
  at <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
  if (length(at) == 0L) NA_integer_ else at[[1]] + 3L
}

read_until <- function(con, done, seconds = 5) {
  buf <- raw()
  deadline <- Sys.time() + seconds
  while (Sys.time() < deadline) {
    chunk <- readBin(con, "raw", 65536L)
    if (length(chunk) > 0L) {
      buf <- c(buf, chunk)
      if (done(buf)) break
    } else {
      Sys.sleep(0.05)
    }
  }
  buf
}

test_that("HEAD returns headers and no body, so the next response on the connection is intact", {
  server <- local_preview(local_site())
  con <- socketConnection(
    server$host,
    port = server$port,
    open = "r+b",
    blocking = FALSE,
    timeout = 5
  )
  withr::defer(close(con))

  writeBin(charToRaw("HEAD /index.html HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"), con)
  flush(con)
  head <- read_until(con, function(b) !is.na(headers_end(b)))
  # Give a body that should not exist time to arrive, then keep whatever came.
  head <- c(head, read_until(con, function(b) FALSE, seconds = 0.5))
  end <- headers_end(head)
  expect_false(is.na(end))
  head_text <- rawToChar(head[seq_len(end)])
  expect_match(head_text, "^HTTP/1\\.1 200")
  expect_match(head_text, "Content-Length: [0-9]+", ignore.case = TRUE)
  # Nothing follows the HEAD response's headers.
  expect_equal(length(head), end)

  writeBin(
    charToRaw("GET /index.html HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"),
    con
  )
  flush(con)
  rest <- read_until(con, function(b) {
    e <- headers_end(b)
    if (is.na(e)) {
      return(FALSE)
    }
    len <- as.integer(sub(
      "(?is).*content-length:\\s*(\\d+).*",
      "\\1",
      rawToChar(b[seq_len(e)]),
      perl = TRUE
    ))
    length(b) >= e + len
  })
  # The second response starts exactly where the first one's headers ended.
  expect_match(rawToChar(rest), "^HTTP/1\\.1 200")
  expect_match(rawToChar(rest[-seq_len(headers_end(rest))]), "<body>site</body>", fixed = TRUE)
})

test_that(".wasm is served as application/wasm", {
  server <- local_preview(local_site())
  expect_equal(
    fetch(paste0(server$url, "/engine.wasm"))$headers[["content-type"]],
    "application/wasm"
  )
})

test_that("percent-encoded paths are decoded", {
  server <- local_preview(local_site())
  r <- fetch(paste0(server$url, "/vfs-files/data/my%20survey.csv"))
  expect_equal(r$status, 200L)
  expect_equal(rawToChar(r$body), "a,b\n")
})

test_that("paths outside the site and directories are not served", {
  server <- local_preview(local_site())
  expect_false(fetch(paste0(server$url, "/../secret.txt"))$status == 200L)
  expect_false(fetch(paste0(server$url, "/%2e%2e/secret.txt"))$status == 200L)
  expect_equal(fetch(paste0(server$url, "/vfs-files"))$status, 404L)
})

test_that("the server answers while the R session is busy", {
  skip_if_not_installed("callr")
  server <- local_preview(local_site())
  bg <- callr::r_bg(
    function(url) {
      t0 <- Sys.time()
      r <- curl::curl_fetch_memory(url)
      c(r$status_code, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    },
    args = list(url = paste0(server$url, "/index.html"))
  )
  Sys.sleep(3) # R is busy; an R request handler could not run now
  bg$wait(10000)
  res <- bg$get_result()
  expect_equal(res[[1]], 200)
  expect_lt(res[[2]], 2.5)
})

test_that("a second preview of the same site replaces the first, and says so", {
  dir <- local_site()
  first <- local_preview(dir)
  expect_message(
    second <- reading_room(dir, open_browser = FALSE, block = FALSE),
    "Stopped the previous preview"
  )
  withr::defer(suppressMessages(reading_room_close(second)))
  expect_true(first$stopped)
  expect_false(second$stopped)
  # Only the second preview is still registered. (Fetching the first URL is no
  # proof: the second server may have been given the port the first freed.)
  live <- Filter(function(s) !isTRUE(s$stopped), webrarian_env$servers)
  expect_length(live, 1L)
  expect_identical(live[[1]], second)
  expect_equal(fetch(paste0(second$url, "/"))$status, 200L)
})

test_that("an explicit busy port is a clear error", {
  first <- local_preview(local_site())
  expect_error(
    suppressMessages(reading_room(
      local_site(),
      port = first$port,
      open_browser = FALSE,
      block = FALSE
    )),
    "port = NULL"
  )
})

test_that("closing a stopped preview says so", {
  server <- suppressMessages(reading_room(local_site(), open_browser = FALSE, block = FALSE))
  suppressMessages(reading_room_close(server))
  expect_message(reading_room_close(server), "already stopped")
})

test_that("reading_room() of a collection serves its built output", {
  root <- local_collection()
  suppressMessages(bind(root))
  server <- local_preview(root)
  expect_match(rawToChar(fetch(paste0(server$url, "/"))$body), "__VIEWER_CONFIG__", fixed = TRUE)
})

test_that("rebuilding while previewing serves the new build", {
  root <- local_collection()
  suppressMessages(bind(root))
  server <- local_preview(root)
  build_id <- function() {
    jsonlite::fromJSON(rawToChar(fetch(paste0(server$url, "/.webrarian-build"))$body))$build_id
  }
  first <- build_id()

  suppressMessages(bind(root))

  expect_equal(fetch(paste0(server$url, "/"))$status, 200L)
  expect_false(identical(build_id(), first))
})

test_that("bundled files with spaces, #, % and non-ASCII names are served at their fetch paths", {
  root <- local_collection()
  names <- c("data/my survey.csv", "data/hash#1.csv", "data/pct%41.csv", "données/été.R")
  for (n in names) {
    fs::dir_create(fs::path_dir(fs::path(root, n)))
    write_lf(n, fs::path(root, n))
  }
  suppressMessages(settings_set(root, "files.include" = list("data/", "données/")))
  suppressMessages(bind(root))
  server <- local_preview(root)

  wire <- read_site_config(fs::path(root, "_site"))
  expect_length(wire$files, 4L)
  for (f in wire$files) {
    r <- fetch(paste0(server$url, "/", f[["fetch-path"]]))
    expect_equal(r$status, 200L, info = f$name)
    expect_equal(
      rawToChar(r$body),
      paste0(sub("^/home/web_user/", "", f[["vfs-path"]]), "\n"),
      info = f$name
    )
  }
})

test_that("reading_room() takes the shared arguments", {
  expect_identical(
    formals_text(reading_room),
    c(
      path = "\".\"",
      port = "NULL",
      watch = "FALSE",
      block = "TRUE",
      open_browser = "rlang::is_interactive()"
    )
  )
  expect_identical(formals_text(reading_room_close), c(room = "NULL"))
})

test_that("the reading room closes itself", {
  room <- local_preview(local_site())
  expect_false(room$stopped)
  room$close()
  expect_true(room$stopped)
})

test_that("from a subdirectory of a collection, the collection's site is served", {
  root <- local_collection()
  suppressMessages(bind(root))
  fs::dir_create(fs::path(root, "analysis"))
  room <- local_preview(fs::path(root, "analysis"))
  expect_identical(room$dir, as.character(fs::path_real(fs::path(root, "_site"))))
})

test_that("a collection is served as a collection even when it holds an index.html", {
  root <- local_collection()
  writeLines("<html>not the site</html>", fs::path(root, "index.html"))
  suppressMessages(bind(root))
  room <- local_preview(root)
  body <- rawToChar(fetch(paste0(room$url, "/"))$body)
  expect_match(body, "__VIEWER_CONFIG__", fixed = TRUE)
  # Only watch mode adds the reload script.
  expect_false(grepl("__reading_room__", body, fixed = TRUE))
})

test_that("watch mode adds the reload script to pages, answers the build id, and serves nothing outside the site", {
  site <- as.character(fs::path_real(local_site()))
  writeLines("<html>outside</html>", fs::path(fs::path_dir(site), "outside.html"))
  page <- watch_response(site, "/")
  expect_equal(page$status, 200L)
  expect_match(page$body, paste0(watch_reload_script(), "</body>"), fixed = TRUE)
  expect_equal(page$headers[["Cross-Origin-Embedder-Policy"]], "require-corp")
  expect_equal(page$headers[["Cache-Control"]], "no-store")
  expect_identical(watch_response(site, "/index.html")$body, page$body)

  # The build id is the count of successful rebuilds watch_collection() records.
  withr::defer(watch_record_builds(site, NULL))
  expect_identical(watch_response(site, "/__reading_room__/build-id")$body, "0")
  watch_record_builds(site, 3L)
  expect_identical(watch_response(site, "/__reading_room__/build-id")$body, "3")
  expect_equal(
    watch_response(site, "/__reading_room__/build-id")$headers[["Cache-Control"]],
    "no-store"
  )

  for (bad in c(
    "/../outside.html",
    "/%2e%2e/outside.html",
    "/missing.html",
    "/engine.wasm",
    "/%zz"
  )) {
    expect_equal(watch_response(site, bad)$status, 404L, info = bad)
  }
})

test_that("a collection that was never built says to run bind()", {
  root <- local_collection()
  expect_error(
    reading_room(root, block = FALSE, open_browser = FALSE),
    "Run .*bind"
  )
})

test_that("a room prints its address", {
  room <- local_preview(local_site())
  expect_message(print(room), room$url, fixed = TRUE)
})

test_that("watch = TRUE on a built site outside any collection says watching needs a collection", {
  site <- local_site()
  err <- expect_error(reading_room(site, watch = TRUE, open_browser = FALSE))
  expect_match(conditionMessage(err), "needs a collection", fixed = TRUE)
  expect_match(conditionMessage(err), "watch = FALSE", fixed = TRUE)
  expect_no_match(conditionMessage(err), "has no", fixed = TRUE)
})

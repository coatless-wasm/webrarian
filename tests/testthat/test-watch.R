# The watch loop debounces by resetting a timer whenever a change is seen.
# Because detect_changes() is level-triggered (it diffs against a manifest that
# is only rewritten after a rebuild), the timer must reset only on a *newly
# seen* change, not on every poll of the same persistent diff — otherwise the
# debounce never elapses and the loop never rebuilds. change_signature() gives
# the loop a stable identity for "the current change set".

test_that("change_signature is order-insensitive for the file list", {
  a <- list(config_changed = FALSE, packages_changed = FALSE, files_changed = c("b.R", "a.R"))
  b <- list(config_changed = FALSE, packages_changed = FALSE, files_changed = c("a.R", "b.R"))
  expect_identical(change_signature(a), change_signature(b))
})

test_that("change_signature distinguishes different change sets", {
  base <- list(config_changed = FALSE, packages_changed = FALSE, files_changed = "a.R")
  expect_false(identical(
    change_signature(base),
    change_signature(list(config_changed = TRUE, packages_changed = FALSE, files_changed = "a.R"))
  ))
  expect_false(identical(
    change_signature(base),
    change_signature(list(
      config_changed = FALSE,
      packages_changed = FALSE,
      files_changed = c("a.R", "b.R")
    ))
  ))
})

test_that("build manifest round-trips through save/load", {
  withr::with_tempdir({
    suppressMessages(catalog())
    config <- collection_settings(".")
    m <- create_build_manifest(config, ".")
    save_build_manifest(m, ".")

    loaded <- load_build_manifest(".")
    expect_equal(loaded$config_hash, m$config_hash)
    expect_equal(names(loaded$packages), names(m$packages))
    # The functional contract (a round-tripped manifest reports no change) is
    # covered by the detect_changes test below; raw structural equality is not
    # asserted because empty character() serializes to an empty JSON array.
  })
})

test_that("detect_changes reports no change right after a manifest is saved", {
  withr::with_tempdir({
    suppressMessages(catalog())
    config <- collection_settings(".")
    save_build_manifest(create_build_manifest(config, "."), ".")

    changes <- detect_changes(config, ".")
    expect_false(changes$any_changed)
  })
})

test_that("detect_changes flags a configuration edit", {
  withr::with_tempdir({
    suppressMessages(catalog())
    config <- collection_settings(".")
    save_build_manifest(create_build_manifest(config, "."), ".")

    suppressMessages(settings_set("project/name" = "changed-name"))
    changes <- detect_changes(collection_settings("."), ".")

    expect_true(changes$config_changed)
    expect_true(changes$any_changed)
  })
})

# --- the watch loop ---------------------------------------------

local_watched_collection <- function(env = parent.frame()) {
  root <- local_collection(env = env)
  writeLines("a <- 1", fs::path(root, "a.R"))
  suppressMessages(settings_set(root, "files.include" = list("*.R")))
  suppressMessages(bind(root))
  root
}

step <- function(state, at, ...) {
  suppressMessages(watch_step(state, at, debounce_delay = 0.5, ...))
}

test_that("an added or deleted file reaches the page's file list and vfs-files", {
  root <- local_watched_collection()
  state <- new_watch_state(root)
  t0 <- Sys.time()
  writeLines("b <- 2", fs::path(root, "b.R"))
  fs::file_delete(fs::path(root, "a.R"))

  state <- step(state, t0)
  expect_true(state$pending)
  state <- step(state, t0 + 1)

  site <- fs::path(root, "_site")
  names <- vapply(read_site_config(site)$files, function(f) f$name, character(1))
  expect_true("b.R" %in% names)
  expect_false("a.R" %in% names)
  expect_true(fs::file_exists(fs::path(site, "vfs-files", "b.R")))
  expect_false(fs::file_exists(fs::path(site, "vfs-files", "a.R")))
  expect_false(detect_changes(collection_settings(root), root)$any_changed)
})

test_that("a failing rebuild is not retried until something changes again", {
  root <- local_watched_collection()
  calls <- 0L
  local_mocked_bindings(bind = function(...) {
    calls <<- calls + 1L
    stop("boom")
  })
  state <- new_watch_state(root)
  t0 <- Sys.time()

  suppressMessages(settings_set(root, "project.name" = "renamed"))
  for (i in 0:5) {
    state <- step(state, t0 + i)
  }
  expect_equal(calls, 1L)

  suppressMessages(settings_set(root, "project.name" = "renamed-again"))
  for (i in 6:8) {
    state <- step(state, t0 + i)
  }
  expect_equal(calls, 2L)
})

test_that("a half-written config is reported once and does not stop the watch", {
  root <- local_watched_collection()
  state <- new_watch_state(root)
  cat("repl: [unclosed\n", file = fs::path(root, "_webrarian.yml"), append = TRUE)

  first <- testthat::capture_messages(state <- watch_step(state, Sys.time()))
  expect_true(any(grepl("Config error", first)))
  second <- testthat::capture_messages(state <- watch_step(state, Sys.time()))
  expect_false(any(grepl("Config error", second)))
})

test_that("a config warning is shown once per version of the file, not on every poll", {
  root <- local_watched_collection()
  state <- new_watch_state(root)
  cat("zzzzzz: 1\n", file = fs::path(root, "_webrarian.yml"), append = TRUE)

  # A long debounce: these polls only look, they never rebuild.
  poll <- function(state) watch_step(state, Sys.time(), debounce_delay = 60)
  expect_no_warning(first <- testthat::capture_messages(state <- poll(state)))
  expect_true(any(grepl("zzzzzz", first, fixed = TRUE)))
  expect_no_warning(second <- testthat::capture_messages(state <- poll(state)))
  expect_false(any(grepl("zzzzzz", second, fixed = TRUE)))

  cat("yyyyyy: 1\n", file = fs::path(root, "_webrarian.yml"), append = TRUE)
  third <- testthat::capture_messages(state <- poll(state))
  expect_true(any(grepl("yyyyyy", third, fixed = TRUE)))
})

# Every request httpuv answers on its own thread (a static file) queues one
# callback on later's global loop, which runs the next time R services that
# loop. Test files that fetch from their servers without servicing it leave
# these behind: a whole-suite run reaches this file with about 320 queued.
# This queues `n` the same way, so the test below meets that backlog even
# when it runs alone.
queue_served_callbacks <- function(n) {
  dir <- withr::local_tempdir()
  writeLines("x", fs::path(dir, "x.txt"))
  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1",
    port,
    list(
      call = function(req) {
        list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not Found")
      },
      staticPaths = list("/" = httpuv::staticPath(dir, fallthrough = FALSE))
    )
  )
  on.exit(httpuv::stopServer(server), add = TRUE)
  handle <- curl::new_handle()
  url <- sprintf("http://127.0.0.1:%d/x.txt", port)
  for (i in seq_len(n)) {
    curl::curl_fetch_memory(url, handle = handle)
  }
  invisible(n)
}

# Fetches a URL that R itself answers in watch mode (the page and its build
# id). httpuv hands such a request to R as callbacks on later's global loop,
# queued behind any already there, and httpuv::service() runs one callback
# per call. So the loop waits in httpuv::service() and only polls curl
# (`timeout = 0`): curl::multi_run() measures its timeout in whole seconds,
# and even `timeout = 0.05` blocks until the clock's second changes, which
# ran one callback a second and let a whole-suite backlog outlast the
# deadline.
fetch_serviced <- function(url, timeout = 10) {
  result <- NULL
  pool <- curl::new_pool()
  curl::curl_fetch_multi(
    url,
    done = function(res) result <<- res,
    fail = function(msg) result <<- simpleError(msg),
    pool = pool
  )
  deadline <- Sys.time() + timeout
  while (is.null(result) && Sys.time() < deadline) {
    # Runs one due callback, or waits up to 10 ms for one.
    httpuv::service(10)
    # Moves the transfer along without waiting.
    curl::multi_run(timeout = 0, pool = pool)
  }
  if (is.null(result)) {
    stop("no answer from ", url)
  }
  if (inherits(result, "error")) {
    stop(result)
  }
  result
}

test_that("reading_room(watch = TRUE) builds, serves the page with the reload script and runs the watch loop", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("curl")
  root <- local_watched_collection()
  opened <- NULL
  local_mocked_bindings(
    open_in_browser = function(url) opened <<- url,
    watch_poll_interval = function() 0.01
  )
  seen <- list()
  calls <- 0L
  local_mocked_bindings(watch_step = function(state, ...) {
    calls <<- calls + 1L
    id_url <- paste0(state$server_url, "/__reading_room__/build-id")
    if (calls == 1L) {
      # A bundled file comes from httpuv's own thread, even while R waits in
      # curl; the page and the build id come from R.
      file <- curl::curl_fetch_memory(paste0(state$server_url, "/vfs-files/a.R"))
      seen$file <<- file$status_code
      seen$file_cache <<- curl::parse_headers_list(file$headers)[["cache-control"]]
      # The page's request waits behind what earlier test files left queued.
      queue_served_callbacks(400)
      seen$page <<- rawToChar(fetch_serviced(paste0(state$server_url, "/"))$content)
      seen$id <<- rawToChar(fetch_serviced(id_url)$content)
      # A successful rebuild, as watch_step() counts it.
      state$builds <- state$builds + 1L
      return(state)
    }
    # watch_collection() recorded the count after the first poll.
    seen$id_after <<- rawToChar(fetch_serviced(id_url)$content)
    rlang::interrupt()
  })

  room <- suppressMessages(reading_room(root, watch = TRUE, open_browser = TRUE))

  index <- fs::path(root, "_site", "index.html")
  expect_equal(seen$file, 200L)
  # The reloaded page fetches edited files again.
  expect_identical(seen$file_cache, "no-cache")
  expect_match(seen$page, "__VIEWER_CONFIG__", fixed = TRUE)
  expect_match(seen$page, paste0(watch_reload_script(), "</body>"), fixed = TRUE)
  expect_identical(seen$id, "0")
  expect_identical(seen$id_after, "1")
  # Added at serve time only: the built page on disk has no reload script.
  expect_false(grepl("__reading_room__", paste(readLines(index), collapse = "\n"), fixed = TRUE))
  expect_identical(opened, room$url)
  expect_true(room$stopped)
  expect_null(webrarian_env$watch_builds[[room$dir]])
  expect_false(isTRUE(webrarian_env$in_watch))
})

# The build id is a count of successful rebuilds, not a hash of the page: a
# new version of an included file leaves index.html byte for byte the same
# (its file list holds names and paths only), and the page must still reload.
test_that("new contents in an included file change the build id the page polls", {
  root <- local_watched_collection()
  site <- as.character(fs::path_real(fs::path(root, "_site")))
  withr::defer(watch_record_builds(site, NULL))
  state <- new_watch_state(root)
  watch_record_builds(site, state$builds)
  before <- watch_response(site, watch_build_id_path())$body

  t0 <- Sys.time()
  writeLines("a <- 2", fs::path(root, "a.R"))
  state <- step(state, t0)
  state <- step(state, t0 + 1)
  watch_record_builds(site, state$builds)

  expect_identical(readLines(fs::path(site, "vfs-files", "a.R")), "a <- 2")
  expect_identical(before, "0")
  expect_identical(watch_response(site, watch_build_id_path())$body, "1")
})

test_that("a failed rebuild leaves the build id the page polls alone", {
  root <- local_watched_collection()
  site <- as.character(fs::path_real(fs::path(root, "_site")))
  withr::defer(watch_record_builds(site, NULL))
  local_mocked_bindings(bind = function(...) stop("boom"))
  state <- new_watch_state(root)
  watch_record_builds(site, state$builds)
  before <- watch_response(site, watch_build_id_path())$body

  # A config change runs a full bind(), which fails.
  t0 <- Sys.time()
  suppressMessages(settings_set(root, "project.name" = "renamed"))
  state <- step(state, t0)
  state <- step(state, t0 + 1)
  watch_record_builds(site, state$builds)

  expect_false(is.null(state$failed_signature))
  expect_identical(watch_response(site, watch_build_id_path())$body, before)
})

test_that("the reload script is pyodidarian's, character for character", {
  expect_identical(
    watch_reload_script(),
    paste0(
      "<script>(function () { var id = null; function poll() {",
      " fetch(\"/__reading_room__/build-id\", { cache: \"no-store\" })",
      ".then(function (r) { return r.text(); }).then(function (t) {",
      " if (id === null) { id = t; } else if (t !== id) { location.reload(); return; }",
      " setTimeout(poll, 1000); }).catch(function () { setTimeout(poll, 1000); }); }",
      " poll(); })();</script>"
    )
  )
})

test_that("watch = TRUE needs block = TRUE", {
  root <- local_collection()
  expect_error(reading_room(root, watch = TRUE, block = FALSE), "block = TRUE")
})

test_that("watch_circulation() is gone", {
  expect_false(exists("watch_circulation", envir = asNamespace("webrarian"), inherits = FALSE))
  expect_false("watch_circulation" %in% namespace_exports())
})

test_that("a files-only rebuild keeps the page's package sources and its offline flag", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  writeLines("a <- 1", fs::path(root, "a.R"))
  suppressMessages(settings_set(root, "files.include" = list("*.R"), "build.offline" = TRUE))
  suppressMessages(bind(root))
  before <- read_site_config(fs::path(root, "_site"))

  writeLines("b <- 2", fs::path(root, "b.R"))
  suppressMessages(rebuild_files_only(root))

  after <- read_site_config(fs::path(root, "_site"))
  expect_true("b.R" %in% vapply(after$files, function(f) f$name, character(1)))
  expect_true(after$offline)
  expect_false("repo-url" %in% names(after$packages))
  expect_identical(after$packages, before$packages)
})

test_that("a files-only rebuild that fails part-way keeps the last good site", {
  # The watch loop promises that a failed rebuild keeps serving the last good
  # build, so the live vfs-files/ must not be emptied before the new copy is
  # complete.
  root <- local_watched_collection()
  site <- fs::path(root, "_site")
  before <- read_site_config(site)
  local_mocked_bindings(build_files_vfs = function(files, root, output_dir) {
    # Copy one file, then fail, as a file deleted mid-copy would.
    dest <- fs::path(output_dir, "vfs-files")
    fs::dir_create(dest)
    fs::file_copy(fs::path(root, files[[1]]), fs::path(dest, files[[1]]))
    stop("disk full")
  })

  writeLines("b <- 2", fs::path(root, "b.R"))
  fs::file_delete(fs::path(root, "a.R"))
  expect_error(suppressMessages(rebuild_files_only(root)), "disk full")

  expect_true(fs::file_exists(fs::path(site, "vfs-files", "a.R")))
  expect_false(fs::file_exists(fs::path(site, "vfs-files", "b.R")))
  expect_identical(read_site_config(site), before)
  # Neither the staging directory nor the temporary page is left in the site.
  expect_length(fs::dir_ls(site, all = TRUE, regexp = "vfs-files\\.new-|\\.tmp-"), 0L)
})

test_that("manifest timestamps are UTC", {
  root <- local_watched_collection()
  m <- create_build_manifest(collection_settings(root), root)
  stamp <- as.POSIXct(m$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_lt(abs(as.numeric(difftime(stamp, Sys.time(), units = "mins"))), 5)
})

test_that("an edit saved while a files-only rebuild copies the files is picked up by the next poll", {
  root <- local_watched_collection()
  real_build_files_vfs <- build_files_vfs
  local_mocked_bindings(build_files_vfs = function(files, root, output_dir) {
    out <- real_build_files_vfs(files, root, output_dir)
    writeLines("a <- 3", fs::path(root, "a.R"))
    out
  })
  writeLines("a <- 2", fs::path(root, "a.R"))

  state <- new_watch_state(root)
  t0 <- Sys.time()
  state <- step(state, t0)
  state <- step(state, t0 + 1)

  expect_equal(readLines(fs::path(root, "_site", "vfs-files", "a.R")), "a <- 2")
  expect_true(detect_changes(collection_settings(root), root)$any_changed)
})

test_that("an edit saved while bind() copies the files is not recorded as built", {
  root <- local_watched_collection()
  real_build_files_vfs <- build_files_vfs
  local_mocked_bindings(build_files_vfs = function(files, root, output_dir) {
    out <- real_build_files_vfs(files, root, output_dir)
    writeLines("a <- 3", fs::path(root, "a.R"))
    out
  })
  writeLines("a <- 2", fs::path(root, "a.R"))

  suppressMessages(bind(root))

  expect_equal(readLines(fs::path(root, "_site", "vfs-files", "a.R")), "a <- 2")
  expect_true(detect_changes(collection_settings(root), root)$any_changed)
})

# End-to-end: the vendored exlibris-r viewer boots a real webR engine, hands off
# webrarian's pre-boot splash to exlibris's UI, and loads a bundled binary .rds
# from the VFS (proving the arrayBuffer path). Heavy and network-bound: gated off
# CRAN and offline runs, and requires a usable headless Chrome.
#
# exlibris-r.js does NOT expose a global webR handle (the engine lives inside the
# React tree), so boot/readiness is detected via DOM signals from exlibris's own
# shell rather than globalThis.webR:
#   - .cm-editor present         => the React shell mounted
#   - .loading-overlay detached  => exlibris finished booting *successfully*
#                                   (the overlay is only cleared on a good boot)
#   - #webrarian-loading .hidden => webrarian's host splash correctly handed off
#                                   (the splash's dismiss observer)
# Console/page errors are collected by a global collector injected before
# navigation. The bundled .rds is read by an auto-run check.R whose sentinel
# output is scraped from the xterm terminal.

test_that("vendored exlibris viewer boots webR, dismisses the splash, and reads a bundled .rds", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if_not_installed("yaml")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  # Build the basic example, bundling the engine locally, with:
  #  - a binary .rds in the VFS (exercises arrayBuffer() loading), and
  #  - an auto-run check.R that reads it and prints a sentinel to the terminal.
  proj <- withr::local_tempdir()
  file.copy(
    list.files(collection_example("basic"), full.names = TRUE),
    proj,
    recursive = TRUE
  )
  data_dir <- file.path(proj, "data")
  dir.create(data_dir, showWarnings = FALSE)
  saveRDS(mtcars, file.path(data_dir, "mtcars.rds"))
  writeLines(
    'cat("RDS_NROW:", nrow(readRDS("/home/web_user/data/mtcars.rds")), "\\n")',
    file.path(proj, "check.R")
  )
  # Auto-run check.R at boot so its output streams to the terminal.
  yml_path <- file.path(proj, "_webrarian.yml")
  # Edited as raw YAML, so the key is the file's hyphenated spelling, not the snake_case one
  # the settings object uses. Assigning the R-side name here would add a second key that
  # nothing reads and leave the file with both.
  yml <- yaml::read_yaml(yml_path)
  yml$repl[["auto-run"]] <- list("check.R")
  yaml::write_yaml(yml, yml_path)

  suppressMessages(bind(proj))
  site <- file.path(proj, "_site")

  # Config is inlined into index.html as window.__VIEWER_CONFIG__ (no separate
  # webrarian-config.json). Confirm it lists the bundled .rds via a fetch path.
  index_html <- paste(readLines(file.path(site, "index.html"), warn = FALSE), collapse = "\n")
  script_match <- regmatches(
    index_html,
    regexpr("<script>window\\.__VIEWER_CONFIG__ = (.*?);</script>", index_html, perl = TRUE)
  )
  config_json <- sub(
    "^<script>window\\.__VIEWER_CONFIG__ = (.*);</script>$",
    "\\1",
    script_match
  )
  cfg <- jsonlite::fromJSON(config_json, simplifyVector = FALSE)
  paths <- vapply(cfg$files, function(f) f[["fetch-path"]], character(1))
  # A skip here would let the only runtime test pass having checked nothing.
  expect_true(any(grepl("mtcars\\.rds$", paths)), info = "the example must bundle data/mtcars.rds")

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(site, port)
  withr::defer(stop_dir_server(server))

  b <- chromote::ChromoteSession$new()
  withr::defer(b$close())

  # Collect console.error / uncaught errors / unhandled rejections BEFORE any page
  # script runs, so a boot failure surfaces as recorded text rather than a blank
  # timeout. addScriptToEvaluateOnNewDocument runs before the page's own scripts.
  b$Page$enable()
  b$Page$addScriptToEvaluateOnNewDocument(
    source = paste0(
      "window.__consoleErrors__ = [];",
      "window.addEventListener('error', function(e){ window.__consoleErrors__.push(String(e.message)); });",
      "window.addEventListener('unhandledrejection', function(e){ window.__consoleErrors__.push('unhandledrejection: ' + String(e.reason)); });",
      "(function(){ var o = console.error; console.error = function(){ window.__consoleErrors__.push(Array.prototype.map.call(arguments, String).join(' ')); return o.apply(console, arguments); }; })();"
    )
  )

  load_fired <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", port), wait_ = FALSE)
  b$wait_for(load_fired)

  wait_for <- function(expr, timeout = 120) {
    deadline <- Sys.time() + timeout
    repeat {
      val <- b$Runtime$evaluate(expr, awaitPromise = TRUE)$result$value
      if (isTRUE(val)) {
        return(TRUE)
      }
      if (Sys.time() > deadline) {
        return(FALSE)
      }
      Sys.sleep(1)
    }
  }

  # 1. Cross-origin isolation active (COOP/COEP headers => webR SAB tier).
  isolated <- b$Runtime$evaluate("crossOriginIsolated")$result$value
  expect_true(isTRUE(isolated))

  # 2. exlibris React shell mounted (its editor pane rendered).
  expect_true(wait_for("!!document.querySelector('.cm-editor')"))

  # 3. exlibris finished booting webR: its loading overlay is removed only on a
  #    successful boot, so its detachment is a real engine-ready signal.
  booted <- wait_for("document.querySelector('.loading-overlay') === null")
  expect_true(booted)

  # 4. webrarian's host splash handed off to exlibris (the splash's dismiss observer).
  splash_hidden <- b$Runtime$evaluate(
    "(function(){ var el = document.getElementById('webrarian-loading'); return !el || el.classList.contains('hidden'); })()"
  )$result$value
  expect_true(isTRUE(splash_hidden))

  # 5. The auto-run check.R read the bundled binary .rds and printed the sentinel
  #    to the terminal (32 rows => the binary loaded intact through the VFS).
  saw_rows <- wait_for(
    "/RDS_NROW:\\s*32/.test((document.querySelector('.xterm-rows') || document.querySelector('.terminal-container') || document.body).innerText || '')"
  )
  expect_true(saw_rows)

  # 6. No console errors or uncaught exceptions during boot + auto-run.
  errs <- b$Runtime$evaluate("JSON.stringify(window.__consoleErrors__ || [])")$result$value
  errs_vec <- jsonlite::fromJSON(errs)
  expect_equal(length(errs_vec), 0L, info = paste(errs_vec, collapse = " | "))
})

test_that("every tested webR version boots with the vendored viewer (CDN engine)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  for (entry in webr_versions_table()$versions) {
    proj <- local_collection()
    writeLines('cat("ENGINE:", R.version.string, "\\n")', fs::path(proj, "engine.R"))
    suppressMessages(settings_set(
      proj,
      "webr.version" = entry$version,
      "files.include" = list("engine.R"),
      "repl.auto-run" = list("engine.R")
    ))
    suppressMessages(bind(proj))

    port <- httpuv::randomPort()
    server <- serve_dir_isolated(fs::path(proj, "_site"), port)
    page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
    page$navigate()

    expected <- sprintf("ENGINE: R version %s", entry$r_version)
    saw <- page$wait_for(sprintf(
      "((document.querySelector('.xterm-rows') || document.body).innerText || '').indexOf(%s) >= 0",
      jsonlite::toJSON(expected, auto_unbox = TRUE)
    ))
    stop_dir_server(server)
    expect_true(saw, info = paste(entry$version, paste(page$errors(), collapse = " | ")))
  }
})

test_that("a light brand palette themes light mode only; dark mode gets exlibris's dark palette", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")

  root <- local_collection()
  writeLines(
    c(
      "color:",
      "  palette:",
      "    brick: '#B84130'",
      "    umber: '#62291F'",
      "    parchment: '#F8F1E0'",
      "  primary: brick",
      "  background: parchment",
      "  foreground: umber"
    ),
    fs::path(root, "_brand.yml")
  )
  suppressMessages(bind(root))

  port <- httpuv::randomPort()
  server <- serve_dir_isolated(fs::path(root, "_site"), port)
  withr::defer(stop_dir_server(server))
  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port))
  root_var <- function(name) {
    tolower(page$eval(sprintf(
      "getComputedStyle(document.documentElement).getPropertyValue(%s).trim()",
      jsonlite::toJSON(name, auto_unbox = TRUE)
    )))
  }
  in_scheme <- function(scheme) {
    page$session$Emulation$setEmulatedMedia(
      features = list(list(name = "prefers-color-scheme", value = scheme))
    )
    page$navigate()
  }

  in_scheme("light")
  expect_equal(root_var("--bg-primary"), "#f8f1e0")
  expect_equal(root_var("--text-primary"), "#62291f")
  expect_equal(root_var("--accent-color"), "#b84130")

  # exlibris's own dark surfaces and text; the accent stays the
  # brand's.
  in_scheme("dark")
  expect_equal(root_var("--bg-primary"), "#1e1e1e")
  expect_equal(root_var("--text-primary"), "#e0e0e0")
  expect_equal(root_var("--accent-color"), "#b84130")
})

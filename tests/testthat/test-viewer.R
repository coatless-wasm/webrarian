# emit_viewer() replaces the old download-and-patch REPL path: it copies the
# vendored inst/viewer assets into the site and injects the ViewerConfig
# inline as window.__VIEWER_CONFIG__ (the exlibris contract) - no runtime
# webrarian-config.json fetch.

fake_config <- function(...) {
  cfg <- apply_config_defaults(list(
    project = list(name = "Demo", description = "d"),
    webr = list(version = "0.6.0")
  ))
  overrides <- list(...)
  for (nm in names(overrides)) {
    cfg[[nm]] <- overrides[[nm]]
  }
  cfg
}

fake_site <- function(files = character()) {
  list(
    files = files,
    engine_base_url = "./",
    packages = list(
      install = character(),
      repos = character(),
      repo_url = "https://repo.r-wasm.org"
    ),
    repl_files = list(auto_open = character(), auto_run = character(), startup_script = NULL),
    build_id = "20260923000000-abcdef12"
  )
}

# Extract the raw JSON text of the inline window.__VIEWER_CONFIG__ script
# from a rendered index.html. Non-greedy up to the *first* "</script>" after
# the anchor - which is exactly what a real <script> tag parser does, so
# this also doubles as the "does a raw </script> truncate the tag early"
# probe: if projectName's escaping ever regressed, this capture would stop
# mid-JSON and the parse below would fail loudly.
extract_viewer_config_json <- function(html) {
  m <- regmatches(
    html,
    regexpr("<script>window\\.__VIEWER_CONFIG__ = (.*?);</script>", html, perl = TRUE)
  )
  expect_length(m, 1)
  sub("^<script>window\\.__VIEWER_CONFIG__ = (.*);</script>$", "\\1", m)
}

# Parsed back into an R list.
extract_viewer_config <- function(html) {
  jsonlite::fromJSON(extract_viewer_config_json(html), simplifyVector = FALSE)
}

test_that("emit_viewer copies the vendored viewer and injects the config inline", {
  dir <- withr::local_tempdir()
  suppressMessages(emit_viewer(fake_config(), dir, root = dir, site = fake_site()))
  expect_true(file.exists(file.path(dir, "index.html")))
  expect_length(fs::dir_ls(dir, regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"), 1L)
  expect_length(fs::dir_ls(dir, regexp = "exlibris-r\\.[0-9a-f]{8}\\.css$"), 1L)

  # No runtime config fetch: the JSON contract is dropped entirely.
  expect_false(file.exists(file.path(dir, "webrarian-config.json")))

  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")

  # The site title is patched into the copied index.html
  expect_match(html, "<title>Demo</title>", fixed = TRUE)

  # No trace of the old webR REPL script or the ?mode= redirect
  expect_false(file.exists(file.path(dir, "repl.js")))
  expect_false(file.exists(file.path(dir, "redirect.html")))

  # The inline config script exists and lands before the module <script>.
  config_pos <- regexpr("<script>window\\.__VIEWER_CONFIG__ = ", html, perl = TRUE)
  module_pos <- regexpr('<script type="module"', html, fixed = TRUE)
  expect_true(config_pos > 0)
  expect_true(module_pos > 0)
  expect_true(config_pos < module_pos)

  # The injected config carries the project's values (v2 shape).
  cfg <- extract_viewer_config(html)
  expect_equal(cfg[["schema-version"]], 1)
  expect_equal(cfg$engine, "webr")
  expect_equal(cfg[["project-name"]], "Demo")
  expect_equal(cfg[["engine-version"]], "0.6.0")
  expect_equal(cfg[["mount-point"]], "/home/web_user")
})

test_that("a </script> in project-name does not break out of the inline <script> tag", {
  dir <- withr::local_tempdir()
  suppressMessages(
    emit_viewer(
      fake_config(
        project = list(
          name = "evil</script><script>alert(1)</script>",
          description = "d"
        )
      ),
      dir,
      root = dir,
      site = fake_site()
    )
  )

  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")

  # jsonlite escapes "/" so the raw byte sequence "</script>" never appears
  # inside the config JSON itself - only as the tag's own closing delimiter.
  json <- extract_viewer_config_json(html)
  expect_false(grepl("</script>", json, fixed = TRUE))
  expect_false(grepl("<", json, fixed = TRUE))

  # And the capture wasn't truncated early: it round-trips to the full,
  # untruncated evil string (a real early truncation would either fail to
  # parse as JSON or yield a partial value).
  cfg <- extract_viewer_config(html)
  expect_equal(cfg[["project-name"]], "evil</script><script>alert(1)</script>")

  # The module <script> that boots the app still loads, right after.
  expect_match(html, '<script type="module" src="./exlibris-r\\.[0-9a-f]{8}\\.js">')
})

test_that("emit_viewer injects the loading-dismiss observer before the module script", {
  dir <- withr::local_tempdir()
  suppressMessages(emit_viewer(fake_config(), dir, root = dir, site = fake_site()))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")

  expect_match(html, "webrarian-loading") # branded splash still present
  expect_match(html, "MutationObserver") # dismiss observer present
  # the classic dismiss script must precede the deferred module bundle
  expect_lt(
    regexpr("MutationObserver", html, fixed = TRUE),
    regexpr('<script type="module"', html, fixed = TRUE)
  )
})

test_that("emit_viewer carries the share-links mode and never the old override global", {
  dir <- withr::local_tempdir()
  suppressMessages(emit_viewer(fake_config(), dir, root = dir, site = fake_site()))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_equal(extract_viewer_config(html)[["share-links"]], "open")
  expect_false(grepl("__VIEWER_ALLOW_URL_OVERRIDE__", html, fixed = TRUE))
})

test_that("repl$share_links = 'off' reaches the page", {
  dir <- withr::local_tempdir()
  cfg <- fake_config()
  cfg$repl$share_links <- "off"
  suppressMessages(emit_viewer(cfg, dir, root = dir, site = fake_site()))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_equal(extract_viewer_config(html)[["share-links"]], "off")
})

test_that("copy_vendored_viewer aborts when inst/viewer is missing", {
  # Point system.file at an empty package dir by faking an absent viewer:
  dir <- withr::local_tempdir()
  local_mocked_bindings(
    viewer_source_dir = function() file.path(dir, "does-not-exist")
  )
  expect_error(copy_vendored_viewer(dir), "Vendored viewer not found")
})

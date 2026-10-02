# The mirror emits the same vendored viewer as bind(), with an empty package
# list (packages are pre-installed into ./repo) and the engine bundled
# locally. Like emit_viewer(), the mirror injects the ViewerConfig inline
# as window.__VIEWER_CONFIG__ - no separate webrarian-config.json is written
# or fetched at runtime.

# Extract the inline window.__VIEWER_CONFIG__ script from a rendered
# index.html and parse it back into an R list (mirrors the helper in
# test-viewer.R).
extract_mirror_viewer_config <- function(html) {
  m <- regmatches(
    html,
    regexpr("<script>window\\.__VIEWER_CONFIG__ = (.*?);</script>", html, perl = TRUE)
  )
  expect_length(m, 1)
  json <- sub("^<script>window\\.__VIEWER_CONFIG__ = (.*);</script>$", "\\1", m)
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

test_that("generate_mirror_html copies the viewer and injects the config inline", {
  dir <- withr::local_tempdir()
  suppressMessages(
    generate_mirror_html(
      dir,
      list(
        webr_version = "0.6.0",
        page_title = "My Mirror"
      )
    )
  )
  expect_true(file.exists(file.path(dir, "index.html")))
  expect_length(fs::dir_ls(dir, regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"), 1L)

  # No runtime config fetch: the JSON contract is never written.
  expect_false(file.exists(file.path(dir, "webrarian-config.json")))

  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "<title>My Mirror</title>", fixed = TRUE)

  # No dead schemaVersion:1 config anywhere in the emitted page.
  expect_false(grepl("schemaVersion\":1", html, fixed = TRUE))

  # The inline config script exists and lands before the module <script>
  # that reads it (same ordering emit_viewer() guarantees).
  config_pos <- regexpr("<script>window\\.__VIEWER_CONFIG__ = ", html, perl = TRUE)
  module_pos <- regexpr('<script type="module"', html, fixed = TRUE)
  expect_true(config_pos > 0)
  expect_true(module_pos > 0)
  expect_true(config_pos < module_pos)

  # The injected config is the wire shape, with the mirror's package/engine
  # settings carried through. Keys are the wire's, hyphenated.
  cfg <- extract_mirror_viewer_config(html)
  expect_equal(cfg[["schema-version"]], 1)
  expect_equal(cfg$engine, "webr")
  expect_equal(cfg[["project-name"]], "My Mirror")
  expect_equal(cfg[["engine-version"]], "0.6.0")
  expect_equal(cfg[["engine-base-url"]], "./webr/v0.6.0/")
  expect_equal(cfg$packages[["repo-url"]], "./repo")
  expect_length(cfg$packages$install, 0)
})

test_that("a mirror ignores share links unless asked otherwise", {
  dir <- withr::local_tempdir()
  suppressMessages(
    generate_mirror_html(dir, list(webr_version = "0.6.0", page_title = "My Mirror"))
  )
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_equal(extract_mirror_viewer_config(html)[["share-links"]], "off")
  expect_false(grepl("__VIEWER_ALLOW_URL_OVERRIDE__", html, fixed = TRUE))

  open_dir <- withr::local_tempdir()
  suppressMessages(generate_mirror_html(
    open_dir,
    list(webr_version = "0.6.0", page_title = "M", share_links = "open")
  ))
  html <- paste(readLines(file.path(open_dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_equal(extract_mirror_viewer_config(html)[["share-links"]], "open")
})

test_that("collection_mirror() rejects an unknown share_links mode", {
  expect_error(collection_mirror(withr::local_tempdir(), share_links = "sometimes"), "share_links")
})

test_that("generate_mirror_html applies a custom favicon", {
  dir <- withr::local_tempdir()
  favicon_src <- withr::local_tempfile(fileext = ".svg")
  writeLines("<svg></svg>", favicon_src)

  suppressMessages(
    generate_mirror_html(
      dir,
      list(
        webr_version = "0.6.0",
        page_title = "M",
        favicon = favicon_src
      )
    )
  )

  expect_true(file.exists(file.path(dir, "assets", "custom", "favicon.svg")))

  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, 'rel="icon"', fixed = TRUE)
  expect_match(html, 'href="assets/custom/favicon.svg"', fixed = TRUE)
})

test_that("the mirror page is offline and resolves its engine and repository URLs before the viewer reads them", {
  dir <- withr::local_tempdir()
  suppressMessages(generate_mirror_html(dir, list(webr_version = "0.6.0", page_title = "M")))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  config_at <- regexpr("<script>window.__VIEWER_CONFIG__ = ", html, fixed = TRUE)
  resolver_at <- regexpr("document.baseURI", html, fixed = TRUE)
  module_at <- regexpr('<script type="module"', html, fixed = TRUE)
  expect_gt(resolver_at, config_at)
  expect_gt(module_at, resolver_at)
  cfg <- extract_mirror_viewer_config(html)
  expect_equal(cfg$packages[["repo-url"]], "./repo")
  expect_length(cfg$packages$repos, 0L)
  # A mirror is always offline (parent spec, "Offline sites and share links").
  expect_true(cfg$offline)
})

test_that("a mirror without a custom favicon gets the inline default", {
  dir <- withr::local_tempdir()
  suppressMessages(generate_mirror_html(dir, list(webr_version = "0.6.0", page_title = "M")))
  html <- paste(readLines(file.path(dir, "index.html"), warn = FALSE), collapse = "\n")
  expect_match(html, 'href="data:image/svg+xml,', fixed = TRUE)
})

# Everything spliced into <head> is either escaped or validated.

head_of <- function(root) {
  html <- paste(readLines(fs::path(root, "_site", "index.html"), warn = FALSE), collapse = "\n")
  sub("</head>.*$", "", html)
}

test_that("backslashes and \\1 in config text reach the page unchanged", {
  root <- local_collection()
  desc <- "Regex drills: \\d+ and \\1 backrefs in C:\\data"
  suppressMessages(settings_set(root, "project.description" = desc))
  suppressMessages(bind(root))
  expect_match(head_of(root), sprintf('content="%s"', desc), fixed = TRUE)
})

test_that("a color that would close the <style> element is dropped with a warning", {
  expect_warning(
    css <- generate_loading_styles(list(
      background_color = "red;}</style><script>alert(1)</script>"
    )),
    "not a CSS color"
  )
  expect_false(grepl("<script>", css, fixed = TRUE))
  expect_match(css, "background: #ffffff", fixed = TRUE)

  ok <- generate_loading_styles(list(
    background_color = "#1a1a2e",
    spinner_color = "rgb(74, 144, 217)"
  ))
  expect_match(ok, "#1a1a2e", fixed = TRUE)
  expect_match(ok, "rgb(74, 144, 217)", fixed = TRUE)
})

test_that("an unsafe editor font family or size never reaches the page", {
  cfg <- apply_config_defaults(list(project = list(name = "x")))
  cfg$ui$.font_family <- "x}</style><script>alert('mono')</script><style>{"
  cfg$ui$.font_size <- "14px; color: red"
  expect_warning(
    expect_warning(head <- generate_ui_head(cfg), "font family"),
    "positive number"
  )
  expect_false(grepl("<script>", head, fixed = TRUE))
  expect_false(grepl(".cm-editor", head, fixed = TRUE))
})

test_that("a favicon file extension is escaped in the attribute", {
  cfg <- apply_config_defaults(list(project = list(name = "x")))
  cfg$ui$.favicon <- 'icon."onload="alert(1)'
  head <- generate_ui_head(cfg)
  expect_false(grepl('"onload=', head, fixed = TRUE))
  expect_match(head, "&quot;onload=&quot;", fixed = TRUE)
})

test_that("og:image is absolute with site-url, and omitted with a warning without it", {
  root <- local_collection()
  writeLines("png", fs::path(root, "card.png"))
  suppressMessages(settings_set(root, "ui.meta.og-image" = "card.png"))

  expect_warning(suppressMessages(bind(root)), "site-url")
  expect_false(grepl("og:image", head_of(root), fixed = TRUE))

  suppressMessages(settings_set(root, "ui.meta.site-url" = "https://example.org/demo/"))
  suppressMessages(bind(root))
  expect_match(
    head_of(root),
    '<meta property="og:image" content="https://example.org/demo/assets/custom/og-image.png" />',
    fixed = TRUE
  )
  expect_match(
    head_of(root),
    '<meta property="og:url" content="https://example.org/demo/" />',
    fixed = TRUE
  )
})

test_that("a missing og-image file is reported, not linked", {
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "ui.meta.og-image" = "nope.png",
    "ui.meta.site-url" = "https://example.org"
  ))
  expect_warning(suppressMessages(bind(root)), "does not exist")
  expect_false(grepl("og:image", head_of(root), fixed = TRUE))
})

test_that("a site-url that is not http(s) is ignored with a warning", {
  expect_warning(expect_null(site_url_or_null("javascript:alert(1)")), "http")
  expect_equal(site_url_or_null("https://example.org/a/"), "https://example.org/a")
})

test_that("a site without a favicon gets an inline default instead of a 404", {
  root <- local_collection()
  suppressMessages(bind(root))
  expect_match(
    head_of(root),
    'rel="icon" type="image/svg+xml" href="data:image/svg+xml,',
    fixed = TRUE
  )
})

test_that("css_number_string drops needless decimals", {
  expect_equal(css_number_string(120.5), "120.5")
  expect_equal(css_number_string(100), "100")
})

test_that("an uppercase og-image extension is copied and linked lowercase", {
  root <- local_collection()
  writeLines("png", fs::path(root, "Card.PNG"))
  suppressMessages(settings_set(
    root,
    "ui.meta.og-image" = "Card.PNG",
    "ui.meta.site-url" = "https://example.org/demo/"
  ))
  suppressMessages(bind(root))

  # fs::file_exists() would pass here even with the bug present, because
  # macOS APFS is case-insensitive; compare the actual on-disk name instead.
  copied <- basename(fs::dir_ls(fs::path(root, "_site", "assets", "custom")))
  expect_true("og-image.png" %in% copied)
  expect_match(
    head_of(root),
    '<meta property="og:image" content="https://example.org/demo/assets/custom/og-image.png" />',
    fixed = TRUE
  )
})

test_that("an uppercase favicon extension is copied and linked lowercase", {
  cfg <- apply_config_defaults(list(project = list(name = "x")))
  cfg$ui$.favicon <- "Icon.ICO"

  root <- withr::local_tempdir()
  writeLines("ico", fs::path(root, "Icon.ICO"))
  out <- withr::local_tempdir()
  copy_ui_assets(cfg, root, out)

  copied <- basename(fs::dir_ls(fs::path(out, "assets", "custom")))
  expect_true("favicon.ico" %in% copied)
  expect_match(generate_ui_head(cfg, root), 'href="assets/custom/favicon.ico"', fixed = TRUE)
})

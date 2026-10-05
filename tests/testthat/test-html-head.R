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

# --- the color scheme ---

test_that("the page reads a stored scheme before the loading screen's styles", {
  root <- local_collection()
  suppressMessages(bind(root))
  head <- head_of(root)
  init <- regexpr("exlibris-theme:", head, fixed = TRUE)
  styles <- regexpr("#webrarian-loading {", head, fixed = TRUE)
  expect_gt(init, 0L)
  expect_gt(styles, init)
  expect_no_match(head, "<html[^>]*data-theme", perl = TRUE)
})

test_that("a pinned site says so on <html>, and in its config", {
  root <- local_collection()
  suppressMessages(settings_set(root, "ui.theme" = "dark"))
  suppressMessages(bind(root))
  expect_match(head_of(root), "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  expect_identical(read_site_config(fs::path(root, "_site"))$theme, "dark")
})

test_that("a dark brand pins the page to dark, and ui.theme: light is refused once", {
  root <- local_collection()
  writeLines(
    c("color:", "  background: '#101820'", "  foreground: '#f2f2f2'"),
    fs::path(root, "_brand.yml")
  )
  suppressMessages(settings_set(root, "ui.theme" = "light"))
  warnings <- capture_warnings(suppressMessages(bind(root)))
  expect_length(grep("the site is pinned to dark", warnings, fixed = TRUE), 1L)
  head <- head_of(root)
  expect_match(head, "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  expect_identical(read_site_config(fs::path(root, "_site"))$theme, "dark")
  # One palette for both schemes, so the loading screen has no dark variant.
  expect_no_match(head, "prefers-color-scheme: dark", fixed = TRUE)
})

# The loading screen follows the scheme in force, with the brand's logo for
# that scheme when it has one for each. One whose contents the author wrote
# (ui.loading.custom-html) keeps its light design, as before the switcher.
test_that("a plain site's loading screen has the dark variant", {
  root <- local_collection()
  suppressMessages(bind(root))
  expect_match(head_of(root), "@media (prefers-color-scheme: dark) { :where(:root", fixed = TRUE)
})

test_that("a loading screen with the author's own HTML keeps its light design", {
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "ui.loading.custom-html" = "<h1 style=\"color: #222222\">Hello</h1>"
  ))
  suppressMessages(bind(root))
  head <- head_of(root)
  expect_match(head, "#webrarian-loading {", fixed = TRUE)
  expect_no_match(head, "prefers-color-scheme: dark", fixed = TRUE)
})

body_of <- function(root) {
  html <- paste(readLines(fs::path(root, "_site", "index.html"), warn = FALSE), collapse = "\n")
  sub("^.*</head>", "", html)
}

test_that("a loading screen with one logo follows the scheme and shows that logo in both", {
  root <- local_collection()
  writeLines(
    c(
      "logo:",
      "  medium: logo.png",
      "color:",
      "  background: '#F8F1E0'",
      "  foreground: '#62291F'"
    ),
    fs::path(root, "_brand.yml")
  )
  writeLines("png", fs::path(root, "logo.png"))
  suppressMessages(bind(root))
  head <- head_of(root)
  expect_match(head, "background: #F8F1E0;", fixed = TRUE)
  expect_match(head, "@media (prefers-color-scheme: dark) { :where(:root", fixed = TRUE)
  body <- body_of(root)
  expect_equal(lengths(regmatches(body, gregexpr("<img [^>]*alt=\"Logo\"", body))), 1L)
  expect_match(body, 'src="assets/custom/logo.png"', fixed = TRUE)
  expect_no_match(paste(head, body), "webrarian-logo-dark", fixed = TRUE)
  expect_false(fs::file_exists(fs::path(root, "_site", "assets", "custom", "logo-dark.png")))
})

test_that("a brand with a light and a dark logo shows each in its scheme", {
  root <- local_collection()
  writeLines(
    c("logo:", "  medium:", "    light: day.png", "    dark: night.png"),
    fs::path(root, "_brand.yml")
  )
  writeLines("day", fs::path(root, "day.png"))
  writeLines("night", fs::path(root, "night.png"))
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  expect_identical(readLines(fs::path(site, "assets", "custom", "logo.png")), "day")
  expect_identical(readLines(fs::path(site, "assets", "custom", "logo-dark.png")), "night")
  body <- body_of(root)
  expect_match(body, '<img class="webrarian-logo-light" src="assets/custom/logo.png"', fixed = TRUE)
  expect_match(
    body,
    '<img class="webrarian-logo-dark" src="assets/custom/logo-dark.png"',
    fixed = TRUE
  )
  head <- head_of(root)
  # Light until the scheme in force is dark: the system's, unless light is
  # forced, or a forced dark.
  expect_match(head, ".webrarian-logo-dark { display: none; }", fixed = TRUE)
  for (dark in c(
    ":where(:root:not([data-theme=\"light\"]))",
    ":where(:root[data-theme=\"dark\"])"
  )) {
    expect_match(head, paste0(dark, " .webrarian-logo-light { display: none; }"), fixed = TRUE)
    expect_match(head, paste0(dark, " .webrarian-logo-dark { display: block; }"), fixed = TRUE)
  }
})

test_that("a dark brand with two logos shows its dark one", {
  root <- local_collection()
  writeLines(
    c(
      "logo:",
      "  medium:",
      "    light: day.png",
      "    dark: night.png",
      "color:",
      "  background: '#101820'",
      "  foreground: '#f2f2f2'"
    ),
    fs::path(root, "_brand.yml")
  )
  writeLines("day", fs::path(root, "day.png"))
  writeLines("night", fs::path(root, "night.png"))
  suppressMessages(bind(root))
  head <- head_of(root)
  # Pinned to dark; one palette, so the screen's colors have no dark variant,
  # but the logos are still told apart by the scheme the page is marked with.
  expect_match(head, "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  expect_no_match(head, "#webrarian-loading { background: #1e1e1e; }", fixed = TRUE)
  expect_match(
    head,
    ":where(:root[data-theme=\"dark\"]) .webrarian-logo-dark { display: block; }",
    fixed = TRUE
  )
})

# The maintainer's rule again: a brand with one usable logo shows it in both
# schemes. A dark logo whose file is not there leaves the brand with one.
test_that("a dark logo whose file is missing falls back to the one logo, with a warning", {
  root <- local_collection()
  writeLines(
    c("logo:", "  medium:", "    light: day.png", "    dark: night.png"),
    fs::path(root, "_brand.yml")
  )
  writeLines("day", fs::path(root, "day.png"))
  expect_message(suppressWarnings(bind(root)), "Logo not found")
  body <- body_of(root)
  expect_equal(lengths(regmatches(body, gregexpr("<img [^>]*alt=\"Logo\"", body))), 1L)
  expect_match(body, 'src="assets/custom/logo.png"', fixed = TRUE)
  expect_no_match(paste(head_of(root), body), "webrarian-logo-dark", fixed = TRUE)
})

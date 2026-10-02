# Tests for R/brand.R pure helpers

test_that("parse_font_size handles numeric input unchanged", {
  expect_equal(parse_font_size(14), 14)
  expect_equal(parse_font_size(16.5), 16.5)
})

test_that("parse_font_size converts px verbatim", {
  expect_equal(parse_font_size("16px"), 16)
  expect_equal(parse_font_size("12.5px"), 12.5)
})

test_that("parse_font_size converts pt to px via 1.333 factor", {
  # round(12 * 1.333) = round(15.996) = 16
  expect_equal(parse_font_size("12pt"), 16)
  # round(10 * 1.333) = round(13.33) = 13
  expect_equal(parse_font_size("10pt"), 13)
})

test_that("parse_font_size converts rem/em against 16px base", {
  expect_equal(parse_font_size("1.5rem"), 24) # round(1.5 * 16)
  expect_equal(parse_font_size("2em"), 32) # round(2 * 16)
  expect_equal(parse_font_size("1rem"), 16)
})

test_that("parse_font_size with a unitless numeric string returns the number", {
  expect_equal(parse_font_size("18"), 18)
})

test_that("parse_font_size returns NULL for non-numeric non-character", {
  expect_null(parse_font_size(NULL))
  expect_null(parse_font_size(list()))
})

test_that("resolve_brand_colors resolves palette-key references to hex", {
  brand <- list(
    color = list(
      palette = list(`brand-blue` = "#1a2b3c", `brand-red` = "#aa0000"),
      primary = "brand-blue",
      secondary = "brand-red"
    )
  )
  out <- resolve_brand_colors(brand)
  expect_equal(out$color$primary, "#1a2b3c")
  expect_equal(out$color$secondary, "#aa0000")
})

test_that("resolve_brand_colors passes literal hex values through unchanged", {
  brand <- list(
    color = list(
      palette = list(`brand-blue` = "#1a2b3c"),
      primary = "#ff0000" # literal, not a palette key
    )
  )
  out <- resolve_brand_colors(brand)
  expect_equal(out$color$primary, "#ff0000")
})

test_that("resolve_brand_colors returns brand unchanged when no color section", {
  brand <- list(meta = list(name = "x"))
  expect_equal(resolve_brand_colors(brand), brand)
})

test_that("build_fonts_css returns NULL when no fonts", {
  expect_null(build_fonts_css(NULL, "."))
  expect_null(build_fonts_css(list(), "."))
})

test_that("build_fonts_css google source yields a googleapis URL", {
  fonts <- list(list(source = "google", family = "Open Sans", weight = c(400, 700)))
  css <- build_fonts_css(fonts, ".")
  expect_type(css, "list")
  expect_match(css$import_url, "^https://fonts\\.googleapis\\.com/css2")
  expect_match(css$import_url, "family=Open\\+Sans:wght@400;700")
  expect_match(css$import_url, "display=swap")
})

test_that("build_fonts_css bunny source yields a fonts.bunny.net URL", {
  fonts <- list(list(source = "bunny", family = "Inter"))
  css <- build_fonts_css(fonts, ".")
  expect_match(css$import_url, "^https://fonts\\.bunny\\.net/css2")
  expect_match(css$import_url, "family=Inter:wght@400;700")
})

test_that("build_fonts_css file source yields @font-face CSS", {
  fonts <- list(list(
    source = "file",
    family = "MyFont",
    files = list(list(path = "fonts/myfont.woff2", weight = 500, style = "italic"))
  ))
  css <- build_fonts_css(fonts, ".")
  expect_match(css$font_faces, "@font-face")
  expect_match(css$font_faces, "MyFont")
  expect_match(css$font_faces, "url('assets/fonts/myfont.woff2')", fixed = TRUE)
})

test_that("a remote brand font file is skipped with a warning, not linked", {
  fonts <- list(list(
    family = "Closed Sans",
    source = "file",
    files = list(
      list(path = "https://example.com/Closed-Sans-Bold.woff2", weight = "bold"),
      list(path = "fonts/closed-sans.woff2", weight = 400)
    )
  ))
  expect_warning(css <- build_fonts_css(fonts, "."), "not a URL")
  expect_false(grepl("Closed-Sans-Bold", css$font_faces, fixed = TRUE))
  expect_match(css$font_faces, "url('assets/fonts/closed-sans.woff2')", fixed = TRUE)
})

test_that("find_brand_yml returns path when directory contains _brand.yml", {
  withr::with_tempdir({
    writeLines("meta:\n  name: Test", "_brand.yml")
    found <- find_brand_yml(".")
    expect_false(is.null(found))
    expect_match(as.character(found), "_brand\\.yml$")
  })
})

test_that("find_brand_yml returns NULL when no brand file present", {
  dir <- withr::local_tempdir()
  expect_null(find_brand_yml(dir))
})

test_that("find_brand_yml accepts a direct file path", {
  withr::with_tempdir({
    writeLines("meta:\n  name: Test", "_brand.yml")
    expect_equal(as.character(find_brand_yml("_brand.yml")), "_brand.yml")
  })
})

test_that("read_brand_yml parses a minimal _brand.yml into a webrarian_brand", {
  withr::with_tempdir({
    writeLines(
      c(
        "meta:",
        "  name: My Project",
        "color:",
        "  palette:",
        "    brand-blue: '#1a2b3c'",
        "  primary: brand-blue",
        "  background: '#ffffff'"
      ),
      "_brand.yml"
    )

    brand <- read_brand_yml(".")
    expect_s3_class(brand, "webrarian_brand")
    expect_equal(brand$meta$name, "My Project")
    # color palette reference resolved to hex
    expect_equal(brand$color$primary, "#1a2b3c")
    # .path recorded for asset resolution
    expect_false(is.null(brand$.path))
  })
})

test_that("read_brand_yml returns NULL when file is missing", {
  dir <- withr::local_tempdir()
  expect_null(read_brand_yml(dir))
})

test_that("apply_brand_to_ui returns ui unchanged for NULL brand", {
  ui <- list(existing = TRUE)
  expect_equal(apply_brand_to_ui(NULL, ui), ui)
})

test_that("apply_brand_to_ui maps colors onto loading config", {
  brand <- list(
    color = list(
      background = "#ffffff",
      foreground = "#000000",
      primary = "#1a2b3c"
    )
  )
  ui <- apply_brand_to_ui(brand)
  expect_equal(ui$loading$background_color, "#ffffff")
  expect_equal(ui$loading$text_color, "#000000")
  expect_equal(ui$loading$spinner_color, "#1a2b3c")
})

test_that("apply_brand_to_ui maps typography size and family", {
  brand <- list(
    typography = list(
      monospace = list(family = "Fira Code", size = "14px")
    )
  )
  ui <- apply_brand_to_ui(brand)
  expect_equal(ui$.font_family, "Fira Code")
  expect_equal(ui$.font_size, 14)
})

test_that("apply_brand_to_ui carries meta name into ui title", {
  brand <- list(meta = list(name = "Widget Lab"))
  ui <- apply_brand_to_ui(brand)
  expect_equal(ui$meta$title, "Widget Lab")
})

# --- brand.yml shorthands and the running UI -----------------------

local_branded_collection <- function(brand_lines, env = parent.frame()) {
  root <- local_collection(env = env)
  writeLines(brand_lines, fs::path(root, "_brand.yml"))
  root
}

page_of <- function(root) {
  paste(readLines(fs::path(root, "_site", "index.html"), warn = FALSE), collapse = "\n")
}

test_that("brand.yml shorthands from the spec build without error", {
  root <- local_branded_collection(c(
    "meta:",
    "  name:",
    "    full: Acme Analytics",
    "    short: Acme",
    "logo: logo.png",
    "typography:",
    "  base: Open Sans",
    "  monospace: Fira Code"
  ))
  writeLines("png", fs::path(root, "logo.png"))

  suppressMessages(bind(root))
  html <- page_of(root)
  expect_match(html, "<title>Acme Analytics</title>", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('property="og:title"', html, fixed = TRUE))), 1L)
  expect_match(html, 'href="assets/custom/favicon.png"', fixed = TRUE)
  expect_match(html, '--font-body: "Open Sans"', fixed = TRUE)
  expect_match(html, '--font-mono: "Fira Code"', fixed = TRUE)
  expect_no_error(utils::capture.output(print(read_brand_yml(root)), type = "message"))
})

test_that("the branded example themes the running viewer, after its stylesheet", {
  root <- fs::path(withr::local_tempdir(), "branded")
  fs::dir_copy(collection_example("branded"), root)
  suppressMessages(settings_set(root, "build.bundle-engine" = FALSE))
  suppressMessages(bind(root))
  html <- page_of(root)

  vars <- regmatches(html, regexpr("<style>:root \\{[^<]*\\}</style>", html))
  expect_length(vars, 1L)
  expect_match(vars, "--accent-color: #4A90D9;", fixed = TRUE)
  expect_match(vars, "--bg-primary: #1a1a2e;", fixed = TRUE)
  expect_match(vars, "--text-primary: #eaeaea;", fixed = TRUE)
  expect_match(vars, '--font-body: "Inter", system-ui, sans-serif;', fixed = TRUE)
  expect_match(vars, '--font-mono: "Fira Code", ui-monospace, monospace;', fixed = TRUE)
  # The rest of the contract follows the brand's dark background and light
  # text; exlibris's light defaults (--bg-secondary #f5f5f5 for headers, tab
  # bars, inputs and the editor gutter) would put #eaeaea text on near-white.
  expect_match(vars, "--bg-secondary: #2b2b3d;", fixed = TRUE)
  expect_match(vars, "--bg-tertiary: #3b3b4c;", fixed = TRUE)
  expect_match(vars, "--border-color: #545463;", fixed = TRUE)
  expect_match(vars, "--text-secondary: #acacb2;", fixed = TRUE)
  expect_match(vars, "--accent-hover: #3f7ab8;", fixed = TRUE)
  # A dark palette (background luminance 0.012, below DARK_LUMINANCE) applies
  # in both color schemes: one :root rule, no light-mode block.
  expect_false(grepl("@media", vars, fixed = TRUE))
  expect_lt(
    regexpr('rel="stylesheet" href="./exlibris-r', html, fixed = TRUE),
    regexpr("<style>:root {", html, fixed = TRUE)
  )
})

test_that("a local brand font is referenced where it is copied", {
  root <- local_branded_collection(c(
    "typography:",
    "  fonts:",
    "    - family: Open Sans",
    "      source: file",
    "      files:",
    "        - path: fonts/open-sans/OpenSans-Variable.ttf"
  ))
  fs::dir_create(fs::path(root, "fonts", "open-sans"))
  writeLines("ttf", fs::path(root, "fonts", "open-sans", "OpenSans-Variable.ttf"))

  suppressMessages(bind(root))
  expect_match(page_of(root), "url('assets/fonts/OpenSans-Variable.ttf')", fixed = TRUE)
  expect_true(fs::file_exists(fs::path(root, "_site", "assets", "fonts", "OpenSans-Variable.ttf")))
})

test_that("a non-integer logo size is written as CSS, not a crash", {
  root <- local_branded_collection(c("logo:", "  medium: logo.png", "  width: 120.5"))
  writeLines("png", fs::path(root, "logo.png"))
  suppressMessages(bind(root))
  expect_match(page_of(root), "width: 120.5px", fixed = TRUE)
})

test_that("a hostile brand font family is dropped with a warning", {
  root <- local_branded_collection(c(
    "typography:",
    "  monospace: \"x}</style><script>alert('mono')</script>\"",
    "  fonts:",
    "    - family: 'F\"><script>alert(1)</script>'",
    "      source: google"
  ))
  seen <- character()
  withCallingHandlers(
    suppressMessages(bind(root)),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("font family", seen)))
  expect_false(grepl("<script>alert", page_of(root), fixed = TRUE))
})

test_that("brand: may name a brand.yml file", {
  root <- local_collection()
  fs::dir_create(fs::path(root, "brand"))
  writeLines(c("color:", "  primary: '#123456'"), fs::path(root, "brand", "acme.yml"))
  suppressMessages(settings_set(root, "brand" = "brand/acme.yml"))
  suppressMessages(bind(root))
  expect_match(page_of(root), "--accent-color: #123456;", fixed = TRUE)
})

test_that("brand colors in other CSS forms get shades the browser mixes", {
  vars <- brand_css_variables(normalize_brand(list(
    color = list(background = "navy", foreground = "rgb(240, 240, 240)", primary = "tomato")
  )))
  expect_match(vars, "--bg-primary: navy;", fixed = TRUE)
  expect_match(
    vars,
    "--bg-secondary: color-mix(in srgb, var(--bg-primary) 92%, var(--text-primary));",
    fixed = TRUE
  )
  expect_match(
    vars,
    "--text-secondary: color-mix(in srgb, var(--bg-primary) 30%, var(--text-primary));",
    fixed = TRUE
  )
  expect_match(
    vars,
    "--accent-hover: color-mix(in srgb, var(--accent-color) 85%, black);",
    fixed = TRUE
  )
})

test_that("only a background is enough to derive the shades", {
  vars <- brand_css_variables(normalize_brand(list(color = list(background = "#ffffff"))))
  expect_match(
    vars,
    "--bg-secondary: color-mix(in srgb, var(--bg-primary) 92%, var(--text-primary));",
    fixed = TRUE
  )
  both <- brand_css_variables(normalize_brand(list(
    color = list(background = "#fff", foreground = "#000")
  )))
  expect_match(both, "--bg-secondary: #ebebeb;", fixed = TRUE)
})

test_that("a brand that sets no colors leaves exlibris's shades alone", {
  vars <- brand_css_variables(normalize_brand(list(typography = list(base = "Inter"))))
  expect_match(vars, "--font-body:", fixed = TRUE)
  expect_false(grepl("--bg-secondary", vars, fixed = TRUE))
  expect_false(grepl("--accent-hover", vars, fixed = TRUE))
})

test_that("normalize_brand expands every documented shorthand", {
  b <- normalize_brand(list(
    logo = "x.png",
    typography = list(base = "Inter", headings = "Roboto Slab", `monospace-inline` = "Fira Code"),
    meta = list(name = list(short = "Acme"))
  ))
  expect_equal(b$logo, list(small = "x.png", medium = "x.png", large = "x.png"))
  expect_equal(b$typography$base, list(family = "Inter"))
  expect_equal(b$typography[["monospace-inline"]], list(family = "Fira Code"))
  expect_equal(b$meta$name, "Acme")
})

# --- dark mode (exlibris's theming contract; the same rule as pyodidarian's brand.py) -------

rule_for <- function(color) brand_css_variables(normalize_brand(list(color = color)))

test_that("a light palette is scoped to light mode; the accent applies in both", {
  # In dark mode exlibris switches every default a producer leaves unset to
  # its dark palette and keeps oneDark's syntax colors, so a light palette
  # must not reach dark-mode visitors. Byte for byte what pyodidarian writes.
  expect_identical(
    rule_for(list(
      palette = list(brick = "#B84130", umber = "#62291F", parchment = "#F8F1E0"),
      primary = "brick",
      background = "parchment",
      foreground = "umber"
    )),
    paste0(
      "<style>:root { --accent-color: #B84130; --accent-hover: #9c3729; } ",
      "@media (prefers-color-scheme: light) { :root { --bg-primary: #F8F1E0; ",
      "--text-primary: #62291F; --bg-secondary: #ece1d1; --bg-tertiary: #e0d1c1; ",
      "--border-color: #ceb9aa; --text-secondary: #8f6559; } }</style>"
    )
  )
})

test_that("a background without a foreground is scoped to light mode, light or dark", {
  # In dark mode exlibris's #e0e0e0 text would land on this background.
  for (background in c("#F8F1E0", "#1a1a2e")) {
    rule <- rule_for(list(background = background))
    expect_true(
      startsWith(
        rule,
        paste0(
          "<style>@media (prefers-color-scheme: light) { :root { ",
          "--bg-primary: ",
          background,
          "; --bg-secondary: color-mix("
        )
      ),
      info = background
    )
    expect_equal(lengths(regmatches(rule, gregexpr(":root", rule, fixed = TRUE))), 1L)
  }
})

test_that("a dark palette applies in both schemes; a background that is not hex counts as light", {
  rule <- rule_for(list(primary = "#4A90D9", background = "#1a1a2e", foreground = "#e0e0e0"))
  expect_true(startsWith(rule, "<style>:root { "))
  expect_false(grepl("@media", rule, fixed = TRUE))
  for (name in c(
    "--bg-primary",
    "--bg-secondary",
    "--bg-tertiary",
    "--text-primary",
    "--text-secondary",
    "--border-color"
  )) {
    expect_match(rule, paste0(name, ": "), fixed = TRUE, info = name)
  }
  light <- rule_for(list(background = "navy", foreground = "white"))
  expect_match(
    light,
    "@media (prefers-color-scheme: light) { :root { --bg-primary: navy;",
    fixed = TRUE
  )
  expect_equal(relative_luminance(c(255L, 255L, 255L)), 1)
  expect_equal(relative_luminance(c(0L, 0L, 0L)), 0)
  expect_equal(DARK_LUMINANCE, 0.5)
})

test_that("no brand means no rule", {
  expect_identical(brand_css_variables(NULL), "")
  expect_identical(brand_root_rule(character()), "")
})

test_that("an offline build links no web fonts, and says so", {
  local_fake_engine()
  root <- local_branded_collection(c(
    "typography:",
    "  fonts:",
    "    - family: Inter",
    "      source: google",
    "  base: Inter"
  ))
  # A Google or Bunny stylesheet is a request to another origin.
  expect_warning(suppressMessages(bind(root, offline = TRUE)), "another origin")
  html <- page_of(root)
  expect_false(grepl("fonts.googleapis.com", html, fixed = TRUE))
  expect_match(html, '--font-body: "Inter", system-ui, sans-serif;', fixed = TRUE)

  suppressMessages(bind(root))
  expect_match(page_of(root), "https://fonts.googleapis.com/css2?family=Inter", fixed = TRUE)
})

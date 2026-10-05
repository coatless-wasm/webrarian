# Brand YAML Support
#
# Implements support for the brand.yml standard from Posit.
# See: https://posit-dev.github.io/brand-yml/brand/

#' Read the collection's `_brand.yml`, if it has one
#'
#' Internal: brand.yml's own package exports a function of this name for
#' reports; webrarian reads the brand while it builds (see load_brand()).
#' @return A normalized `webrarian_brand`, or NULL.
#' @noRd
read_brand_yml <- function(path = ".") {
  brand_file <- find_brand_yml(path)
  if (is.null(brand_file)) {
    return(NULL)
  }
  read_brand_file(brand_file)
}

#' Read one brand.yml file into a normalized webrarian_brand
#' @noRd
read_brand_file <- function(brand_file) {
  brand <- read_yaml_file(brand_file)
  if (!is.list(brand)) {
    cli::cli_warn("Failed to parse {.file {brand_file}}")
    return(NULL)
  }
  brand$.path <- fs::path_dir(brand_file)
  finish_brand(brand)
}

#' @noRd
finish_brand <- function(brand) {
  brand <- normalize_brand(brand)
  class(brand) <- c("webrarian_brand", "list")
  brand
}

#' The brand a collection uses: `brand:` in _webrarian.yml (a path or an
#' inline mapping), else _brand.yml next to it, else NULL
#' @noRd
load_brand <- function(config, root) {
  spec <- config$brand
  if (is.character(spec) && length(spec) == 1L) {
    file <- fs::path(root, spec)
    if (!fs::file_exists(file)) {
      cli::cli_abort("{.field brand} names {.file {spec}}, which does not exist.")
    }
    return(read_brand_file(file))
  }
  if (is.list(spec)) {
    spec$.path <- root
    return(finish_brand(spec))
  }
  read_brand_yml(root)
}

#' Expand brand.yml's shorthands so the rest of the code sees one shape
#'
#' `logo: x.png` means one logo at every size; `typography: {base: Inter}`
#' means `{base: {family: Inter}}`; `meta.name` may be `{full, short}`.
#' @noRd
normalize_brand <- function(brand) {
  if (is.null(brand)) {
    return(NULL)
  }
  if (is.character(brand$logo) && length(brand$logo) == 1L) {
    brand$logo <- list(small = brand$logo, medium = brand$logo, large = brand$logo)
  }
  if (is.list(brand$typography)) {
    for (role in c("base", "headings", "monospace", "monospace-inline", "monospace-block")) {
      value <- brand$typography[[role]]
      if (is.character(value) && length(value) == 1L) {
        brand$typography[[role]] <- list(family = value)
      }
    }
  }
  if (is.list(brand$meta$name)) {
    brand$meta$name <- brand$meta$name$full %||% brand$meta$name$short
  }
  resolve_brand_colors(brand)
}

#' Find brand.yml file
#' @noRd
find_brand_yml <- function(path) {
  # If path is directly to a file
  if (fs::is_file(path) && grepl("_brand\\.ya?ml$", path)) {
    return(path)
  }

  # Look for _brand.yml or _brand.yaml
  for (name in c("_brand.yml", "_brand.yaml")) {
    brand_path <- fs::path(path, name)
    if (fs::file_exists(brand_path)) {
      return(brand_path)
    }
  }

  NULL
}

#' Resolve color palette references in brand config
#' @noRd
resolve_brand_colors <- function(brand) {
  if (is.null(brand$color)) {
    return(brand)
  }

  palette <- brand$color$palette %||% list()

  # Resolve references in theme colors
  theme_colors <- c(
    "foreground",
    "background",
    "primary",
    "secondary",
    "tertiary",
    "success",
    "info",
    "warning",
    "danger",
    "light",
    "dark"
  )

  for (color_name in theme_colors) {
    value <- brand$color[[color_name]]
    if (!is.null(value) && is.character(value) && value %in% names(palette)) {
      brand$color[[color_name]] <- palette[[value]]
    }
  }

  brand
}

#' Apply brand.yml to webrarian ui config
#'
#' Converts brand.yml settings to webrarian ui configuration format.
#'
#' @param brand A brand configuration from `read_brand_yml()`.
#' @param ui Existing ui config to merge into.
#'
#' @return Updated ui configuration list.
#'
#' @noRd
apply_brand_to_ui <- function(brand, ui = list()) {
  if (is.null(brand)) {
    return(ui)
  }

  brand_path <- brand$.path %||% "."

  # --- Logo ---
  # Store paths for favicon and logo from brand config
  if (!is.null(brand$logo)) {
    logo <- brand$logo

    # Favicon from small logo
    favicon_rel <- resolve_brand_logo_relative(logo, "small")
    if (!is.null(favicon_rel)) {
      ui$.favicon <- favicon_rel
      ui$.brand_favicon <- resolve_brand_logo(logo, "small", brand_path)
    }

    # Main logo from medium or large
    main_logo_rel <- resolve_brand_logo_relative(logo, "medium") %||%
      resolve_brand_logo_relative(logo, "large")
    if (!is.null(main_logo_rel)) {
      ui$.logo <- main_logo_rel
      ui$.brand_logo <- resolve_brand_logo(logo, "medium", brand_path) %||%
        resolve_brand_logo(logo, "large", brand_path)
    }

    # The logo for the dark scheme, when the brand gives another file for it.
    dark_logo_rel <- resolve_brand_logo_relative(logo, "medium", "dark") %||%
      resolve_brand_logo_relative(logo, "large", "dark")
    if (
      !is.null(main_logo_rel) && !is.null(dark_logo_rel) && !identical(dark_logo_rel, main_logo_rel)
    ) {
      ui$.logo_dark <- dark_logo_rel
      ui$.brand_logo_dark <- resolve_brand_logo(logo, "medium", brand_path, "dark") %||%
        resolve_brand_logo(logo, "large", brand_path, "dark")
    }

    # Logo dimensions (default 100x100)
    ui$.logo_width <- css_number(brand$logo$width, "brand.logo.width") %||% 100
    ui$.logo_height <- css_number(brand$logo$height, "brand.logo.height") %||% 100
  }

  # --- Colors ---
  if (!is.null(brand$color)) {
    # Loading screen colors
    ui$loading <- ui$loading %||% list()

    # Background color
    if (!is.null(brand$color$background)) {
      ui$loading$background_color <- brand$color$background
    }

    # Text color (use foreground or secondary)
    if (!is.null(brand$color$foreground)) {
      ui$loading$text_color <- brand$color$foreground
    } else if (!is.null(brand$color$secondary)) {
      ui$loading$text_color <- brand$color$secondary
    }

    # Spinner color (use primary)
    if (!is.null(brand$color$primary)) {
      ui$loading$spinner_color <- brand$color$primary
    }
  }

  # --- Typography ---
  if (!is.null(brand$typography)) {
    typo <- brand$typography

    # Monospace font for editor
    if (!is.null(typo$monospace$family)) {
      ui$.font_family <- typo$monospace$family
    } else if (!is.null(typo$`monospace-inline`$family)) {
      ui$.font_family <- typo$`monospace-inline`$family
    }

    # Font size
    if (!is.null(typo$monospace$size)) {
      ui$.font_size <- parse_font_size(typo$monospace$size)
    } else if (!is.null(typo$base$size)) {
      ui$.font_size <- parse_font_size(typo$base$size)
    }

    # Build Google Fonts URL if fonts are specified
    fonts_css <- build_fonts_css(typo$fonts, brand_path)
    if (!is.null(fonts_css)) {
      ui$.fonts_css <- fonts_css
    }
  }

  # --- Meta ---
  if (!is.null(brand$meta)) {
    ui$meta <- ui$meta %||% list()

    if (!is.null(brand$meta$name)) {
      ui$meta$title <- ui$meta$title %||% brand$meta$name
    }
  }

  ui
}

#' The logo a brand gives for a color scheme
#'
#' brand.yml lets a logo be one image or a `light` and a `dark` one. A light
#' logo shows in the light scheme and a dark logo in the dark scheme; a brand
#' that gives only one of the two uses it in both.
#' @param value A logo size's value when it is a list of variants.
#' @param variant "light" or "dark".
#' @noRd
brand_logo_variant <- function(value, variant) {
  other <- if (identical(variant, "dark")) "light" else "dark"
  value[[variant]] %||% value[[other]]
}

#' Resolve a brand logo reference to relative path (for config)
#' @noRd
resolve_brand_logo_relative <- function(logo, size, variant = "light") {
  if (is.null(logo)) {
    return(NULL)
  }

  value <- logo[[size]]
  if (is.null(value)) {
    return(NULL)
  }

  # If it's a string reference to images
  if (is.character(value)) {
    if (!is.null(logo$images) && value %in% names(logo$images)) {
      img <- logo$images[[value]]
      if (is.character(img)) {
        return(img)
      }
      if (is.list(img) && !is.null(img$path)) return(img$path)
    }
    return(value)
  }

  # If it's a list with variants
  if (is.list(value)) {
    chosen <- brand_logo_variant(value, variant)
    if (!is.null(chosen)) {
      return(resolve_brand_logo_relative(list(images = logo$images, temp = chosen), "temp"))
    }
    if (!is.null(value$path)) return(value$path)
  }

  NULL
}

#' Resolve a brand logo reference to absolute path
#' @noRd
resolve_brand_logo <- function(logo, size, brand_path, variant = "light") {
  if (is.null(logo)) {
    return(NULL)
  }

  value <- logo[[size]]

  if (is.null(value)) {
    return(NULL)
  }

  # If it's a string, it could be a reference to images or a direct path
  if (is.character(value)) {
    # Check if it's a reference to named images
    if (!is.null(logo$images) && value %in% names(logo$images)) {
      img <- logo$images[[value]]
      if (is.character(img)) {
        return(fs::path(brand_path, img))
      } else if (is.list(img) && !is.null(img$path)) {
        return(fs::path(brand_path, img$path))
      }
    }
    # Direct path
    return(fs::path(brand_path, value))
  }

  # If it's a list with light/dark variants, use the one asked for
  if (is.list(value)) {
    chosen <- brand_logo_variant(value, variant)
    if (!is.null(chosen)) {
      return(resolve_brand_logo(list(images = logo$images, temp = chosen), "temp", brand_path))
    }
    if (!is.null(value$path)) {
      return(fs::path(brand_path, value$path))
    }
  }

  NULL
}

#' Parse font size to numeric pixels
#' @noRd
parse_font_size <- function(size) {
  if (is.numeric(size)) {
    return(size)
  }

  if (is.character(size)) {
    # Extract numeric value
    num <- as.numeric(gsub("[^0-9.]", "", size))

    # Convert based on unit
    if (grepl("px$", size)) {
      return(num)
    } else if (grepl("pt$", size)) {
      return(round(num * 1.333)) # pt to px
    } else if (grepl("em$|rem$", size)) {
      return(round(num * 16)) # Assume 16px base
    }

    return(num)
  }

  NULL
}

#' Font weights for a Google/Bunny URL: integers 1-1000, joined by ";"
#' @noRd
font_weights <- function(weight) {
  w <- suppressWarnings(as.integer(unlist(weight)))
  w <- w[!is.na(w) & w >= 1L & w <= 1000L]
  if (length(w) == 0L) {
    w <- c(400L, 700L)
  }
  paste(sort(unique(w)), collapse = ";")
}

#' Build CSS for brand fonts
#'
#' Local font files are referenced under assets/fonts/, where
#' copy_brand_assets() copies them. A font file given as a URL is skipped
#' with a warning, since it is never copied there.
#' @noRd
build_fonts_css <- function(fonts, brand_path) {
  if (is.null(fonts) || length(fonts) == 0) {
    return(NULL)
  }

  css_parts <- character()
  families <- character()
  has_bunny <- FALSE

  for (font in fonts) {
    if (!is.list(font) || is.null(font$source)) {
      next
    }
    family <- css_font_family(font$family, "brand.typography.fonts.family")
    if (is.null(family)) {
      next
    }

    if (font$source %in% c("google", "bunny")) {
      families <- c(
        families,
        sprintf(
          "%s:wght@%s",
          gsub(" ", "+", family, fixed = TRUE),
          font_weights(font$weight)
        )
      )
      has_bunny <- has_bunny || identical(font$source, "bunny")
    } else if (identical(font$source, "file")) {
      for (file_entry in font$files %||% list()) {
        path <- if (is.character(file_entry)) file_entry else file_entry$path
        if (is.null(path)) {
          next
        }
        if (grepl("^([A-Za-z][A-Za-z0-9+.-]+:|//)", path)) {
          cli::cli_warn(c(
            "Skipping font file {.url {path}}: a brand font file must be a path inside the collection, not a URL.",
            "i" = "Download it next to the brand file and list its relative path, or use {.code source: google} or {.code source: bunny}."
          ))
          next
        }
        name <- basename(path)
        if (!grepl("^[A-Za-z0-9._-]+$", name)) {
          cli::cli_warn(
            "Skipping font file {.file {path}}: use a name made of letters, digits, '.', '_' and '-'."
          )
          next
        }
        weight <- if (is.character(file_entry)) {
          400
        } else {
          css_number(file_entry$weight %||% 400, "brand.typography.fonts.files.weight") %||% 400
        }
        style <- if (is.character(file_entry)) "normal" else (file_entry$style %||% "normal")
        if (!style %in% c("normal", "italic", "oblique")) {
          style <- "normal"
        }
        css_parts <- c(
          css_parts,
          sprintf(
            "@font-face { font-family: \"%s\"; src: url('assets/fonts/%s'); font-weight: %s; font-style: %s; }",
            family,
            name,
            css_number_string(weight),
            style
          )
        )
      }
    }
  }

  result <- list()
  if (length(families) > 0) {
    base_url <- if (has_bunny) {
      "https://fonts.bunny.net/css2"
    } else {
      "https://fonts.googleapis.com/css2"
    }
    result$import_url <- sprintf(
      "%s?family=%s&display=swap",
      base_url,
      paste(families, collapse = "&family=")
    )
  }
  if (length(css_parts) > 0) {
    result$font_faces <- paste(css_parts, collapse = "\n")
  }
  if (length(result) == 0) {
    return(NULL)
  }
  result
}

#' Copy brand assets to output directory
#'
#' Copies local brand font files to <output>/assets/fonts/ (the logo and
#' favicon are copied by copy_ui_assets()).
#'
#' @param brand Brand configuration from `read_brand_yml()`.
#' @param output_dir Output directory.
#'
#' @return Invisibly returns TRUE if assets were copied.
#'
#' @noRd
copy_brand_assets <- function(brand, output_dir) {
  if (is.null(brand)) {
    return(invisible(FALSE))
  }

  brand_path <- brand$.path %||% "."
  copied <- FALSE

  # Copy local font files
  if (!is.null(brand$typography$fonts)) {
    for (font in brand$typography$fonts) {
      if (identical(font$source, "file") && !is.null(font$files)) {
        fonts_dir <- fs::path(output_dir, "assets", "fonts")

        for (file_entry in font$files) {
          path <- if (is.character(file_entry)) file_entry else file_entry$path
          if (!is.null(path)) {
            src <- fs::path(brand_path, path)
            if (fs::file_exists(src)) {
              ensure_dir(fonts_dir)
              fs::file_copy(src, fs::path(fonts_dir, fs::path_file(src)), overwrite = TRUE)
              copied <- TRUE
            }
          }
        }
      }
    }
  }

  if (copied) {
    cli::cli_alert_success("Copied brand assets")
  }

  invisible(copied)
}

#' Generate head tags for brand fonts
#'
#' Google and Bunny fonts are stylesheets on another origin, so an offline
#' build (build.offline: true, which makes no such request) does not link
#' them: it warns, and the page falls back to the next font in each stack.
#' Local `source: file` fonts are always used.
#' @param offline The build's resolved `build.offline`.
#' @noRd
generate_brand_fonts_head <- function(brand, offline = FALSE) {
  if (is.null(brand) || is.null(brand$typography$fonts)) {
    return("")
  }

  fonts_css <- build_fonts_css(brand$typography$fonts, brand$.path %||% ".")

  if (is.null(fonts_css)) {
    return("")
  }

  tags <- character()

  if (!is.null(fonts_css$import_url) && isTRUE(offline)) {
    cli::cli_warn(c(
      "Not linking the brand's Google or Bunny fonts: {.field build.offline} is true, so the site makes no request to another origin.",
      "i" = "The page uses the next font in each stack. List the font files with {.code source: file} to use them offline."
    ))
  } else if (!is.null(fonts_css$import_url)) {
    host <- if (grepl("bunny", fonts_css$import_url, fixed = TRUE)) {
      "https://fonts.bunny.net"
    } else {
      "https://fonts.googleapis.com"
    }
    tags <- c(tags, sprintf('<link rel="preconnect" href="%s">', host))
    if (identical(host, "https://fonts.googleapis.com")) {
      tags <- c(tags, '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>')
    }
    tags <- c(tags, sprintf('<link href="%s" rel="stylesheet">', html_escape(fonts_css$import_url)))
  }

  # Local font faces
  if (!is.null(fonts_css$font_faces)) {
    tags <- c(tags, sprintf('<style>%s</style>', fonts_css$font_faces))
  }

  paste(tags, collapse = "\n  ")
}

#' Print method for webrarian_brand
#'
#' @param x A `webrarian_brand` object.
#' @param ... Additional arguments passed to methods (ignored).
#' @return `x`, invisibly.
#' @export
print.webrarian_brand <- function(x, ...) {
  cli::cli_h1("Brand Configuration")
  cli::cli_text("Path: {.path {x$.path}}")

  if (!is.null(x$meta$name)) {
    cli::cli_text("Name: {.val {x$meta$name}}")
  }

  if (!is.null(x$color)) {
    cli::cli_h2("Colors")
    colors <- c("primary", "secondary", "background", "foreground")
    for (c in colors) {
      if (!is.null(x$color[[c]])) {
        cli::cli_text("{c}: {.val {x$color[[c]]}}")
      }
    }
  }

  if (!is.null(x$typography)) {
    cli::cli_h2("Typography")
    if (!is.null(x$typography$base$family)) {
      cli::cli_text("Base font: {.val {x$typography$base$family}}")
    }
    if (!is.null(x$typography$monospace$family)) {
      cli::cli_text("Monospace: {.val {x$typography$monospace$family}}")
    }
  }

  if (!is.null(x$logo)) {
    cli::cli_h2("Logos")
    for (size in c("small", "medium", "large")) {
      if (!is.null(x$logo[[size]])) {
        cli::cli_text("{size}: configured")
      }
    }
  }

  invisible(x)
}

#' A hex color as c(r, g, b), or NULL for any other form
#'
#' Only hex is read here: R's color names differ from CSS's (R's "green" is
#' #00FF00, CSS's is #008000), and rgb()/hsl() are left to the browser.
#' @noRd
hex_rgb <- function(x) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !grepl("^#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$", x, perl = TRUE)
  ) {
    return(NULL)
  }
  hex <- substring(x, 2L)
  if (nchar(hex) <= 4L) {
    hex <- paste(rep(strsplit(hex, "", fixed = TRUE)[[1]][1:3], each = 2L), collapse = "")
  }
  strtoi(substring(hex, c(1L, 3L, 5L), c(2L, 4L, 6L)), 16L)
}

#' `a * weight + b * (1 - weight)` as #rrggbb
#' @noRd
mix_rgb <- function(a, b, weight) {
  rgb <- round(a * weight + b * (1 - weight))
  sprintf("#%02x%02x%02x", rgb[[1]], rgb[[2]], rgb[[3]])
}

#' The contract's secondary shades, derived from the brand's colors
#'
#' exlibris's defaults for --bg-secondary, --bg-tertiary, --border-color,
#' --text-secondary and --accent-hover are tuned to its own light and dark
#' palettes. Once a brand sets the background or the text color, keeping them
#' puts branded text on unbranded headers, tab bars, inputs and editor gutters
#' (light text on #f5f5f5), so they are derived from the brand instead. Hex
#' colors are mixed here, which gives the terminal's canvas (it reads
#' --bg-tertiary for its selection) a plain color; any other form is mixed by
#' the browser from the variables themselves, so no user text is spliced in.
#' @param accent,bg,fg Validated colors (css_color()) or NULL.
#' @noRd
brand_derived_colors <- function(accent, bg, fg) {
  out <- character()
  if (!is.null(bg) || !is.null(fg)) {
    bg_rgb <- hex_rgb(bg)
    fg_rgb <- hex_rgb(fg)
    shades <- c(
      "--bg-secondary" = 0.92,
      "--bg-tertiary" = 0.84,
      "--border-color" = 0.72,
      "--text-secondary" = 0.30
    )
    for (name in names(shades)) {
      weight <- shades[[name]]
      out[[name]] <- if (!is.null(bg_rgb) && !is.null(fg_rgb)) {
        mix_rgb(bg_rgb, fg_rgb, weight)
      } else {
        sprintf(
          "color-mix(in srgb, var(--bg-primary) %d%%, var(--text-primary))",
          as.integer(round(weight * 100))
        )
      }
    }
  }
  if (!is.null(accent)) {
    accent_rgb <- hex_rgb(accent)
    out[["--accent-hover"]] <- if (!is.null(accent_rgb)) {
      mix_rgb(accent_rgb, c(0L, 0L, 0L), 0.85)
    } else {
      "color-mix(in srgb, var(--accent-color) 85%, black)"
    }
  }
  out
}

#' The exlibris theming variables a brand sets
#'
#' In the order of pyodidarian's `css_variables()`, so one brand gives the
#' same rule in both tools.
#' @return A named character vector, possibly empty.
#' @noRd
brand_variables <- function(brand) {
  if (is.null(brand)) {
    return(character())
  }
  accent <- css_color(brand$color$primary, "brand.color.primary")
  bg <- css_color(brand$color$background, "brand.color.background")
  fg <- css_color(brand$color$foreground, "brand.color.foreground")
  vars <- c(
    "--accent-color" = accent,
    "--bg-primary" = bg,
    "--text-primary" = fg,
    brand_derived_colors(accent, bg, fg)
  )
  base <- css_font_family(brand$typography$base$family, "brand.typography.base.family")
  mono <- css_font_family(brand$typography$monospace$family, "brand.typography.monospace.family")
  if (!is.null(base)) {
    vars[["--font-body"]] <- css_font_stack(base, "system-ui, sans-serif")
  }
  if (!is.null(mono)) {
    vars[["--font-mono"]] <- css_font_stack(mono, "ui-monospace, monospace")
  }
  vars
}

#' Background luminance below which a brand palette counts as dark
#'
#' The same threshold as pyodidarian's `brand.DARK_LUMINANCE` (the rule the two tools share).
#' @noRd
DARK_LUMINANCE <- 0.5

#' WCAG 2 relative luminance of an sRGB color: 0 for black, 1 for white
#' @param rgb Integer `c(r, g, b)`, 0-255 (hex_rgb()).
#' @noRd
relative_luminance <- function(rgb) {
  channel <- rgb / 255
  linear <- ifelse(channel <= 0.04045, channel / 12.92, ((channel + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * linear)
}

#' Does a brand palette apply in both color schemes?
#'
#' Only when the brand sets both the background and the text color and the
#' background is a hex color darker than DARK_LUMINANCE. Any other palette (a
#' background that is not hex counts as light) is scoped to light mode.
#' @param vars brand_variables()'s result.
#' @noRd
is_dark_palette <- function(vars) {
  if (!all(c("--bg-primary", "--text-primary") %in% names(vars))) {
    return(FALSE)
  }
  rgb <- hex_rgb(vars[["--bg-primary"]])
  !is.null(rgb) && relative_luminance(rgb) < DARK_LUMINANCE
}

#' The surface and text variables, which go together (exlibris's theming contract)
#' @noRd
is_surface_variable <- function(name) {
  startsWith(name, "--bg-") | startsWith(name, "--text-") | name == "--border-color"
}

#' The `<style>` element that sets the brand's variables, or ""
#'
#' exlibris styles its shell from CSS custom properties on :root (the theming
#' contract documented in exlibris). Its defaults sit in :where(:root), which
#' has zero specificity, so a plain :root rule wins wherever it is placed; it
#' goes after the viewer's stylesheet all the same. In the dark scheme exlibris
#' switches every default a producer leaves unset to its dark palette and
#' keeps oneDark's syntax colors, so the accent and the fonts go on :root for
#' both schemes while the surface and text colors are the brand's light scheme.
#' The scheme in force is the system's unless the page forces one, which it
#' carries as data-theme on <html> (a visitor's choice from the viewer's
#' Settings gear, or a pinned site), so the light colors are written twice:
#' for a light system unless the page is forced dark, and for a page forced
#' light. Unless the palette is dark: then all of them apply in both schemes,
#' in one :root rule, and the site is pinned to dark (site_theme()).
#' @param vars brand_variables()'s result.
#' @noRd
brand_root_rule <- function(vars) {
  if (length(vars) == 0L) {
    return("")
  }
  declarations <- function(v) paste0(names(v), ": ", unname(v), ";", collapse = " ")
  if (is_dark_palette(vars)) {
    both <- vars
    light <- character()
  } else {
    surface <- is_surface_variable(names(vars))
    both <- vars[!surface]
    light <- vars[surface]
  }
  rules <- c(
    if (length(both) > 0L) sprintf(":root { %s }", declarations(both)),
    if (length(light) > 0L) {
      sprintf(
        paste(
          "@media (prefers-color-scheme: light) { :root:not([data-theme=\"dark\"]) { %s } }",
          ":root[data-theme=\"light\"] { %s }"
        ),
        declarations(light),
        declarations(light)
      )
    }
  )
  sprintf("<style>%s</style>", paste(rules, collapse = " "))
}

#' The exlibris theming rule for a brand
#' @noRd
brand_css_variables <- function(brand) {
  brand_root_rule(brand_variables(brand))
}

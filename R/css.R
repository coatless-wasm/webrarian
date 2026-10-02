# Validators for values spliced into CSS and <head>
#
# _webrarian.yml and _brand.yml are author-controlled but can come from a
# cloned repository, and a value that closes a <style> element breaks the page.
# Anything that is not a plain token is dropped with a warning naming the key.

#' A CSS color, or NULL
#' @noRd
css_color <- function(x, field) {
  if (is.null(x)) {
    return(NULL)
  }
  ok <- is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    (grepl("^#(?:[0-9a-fA-F]{3,4}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$", x, perl = TRUE) ||
      grepl("^(?:rgb|rgba|hsl|hsla)\\([0-9.,%\\s/deg]+\\)$", x, perl = TRUE) ||
      grepl("^[a-zA-Z]{3,20}$", x))
  if (ok) {
    return(x)
  }
  cli::cli_warn("Ignoring {.field {field}}: {.val {x}} is not a CSS color.")
  NULL
}

#' A font family name, or NULL
#' @noRd
css_font_family <- function(x, field) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[A-Za-z0-9][A-Za-z0-9 _-]{0,62}$", x)
  ) {
    return(x)
  }
  cli::cli_warn("Ignoring {.field {field}}: {.val {x}} is not a font family name.")
  NULL
}

#' A quoted family followed by a generic fallback
#' @noRd
css_font_stack <- function(family, fallback) {
  paste0("\"", family, "\", ", fallback)
}

#' A positive number, or NULL
#' @noRd
css_number <- function(x, field) {
  if (is.null(x)) {
    return(NULL)
  }
  n <- suppressWarnings(as.numeric(x))
  if (length(n) == 1L && !is.na(n) && is.finite(n) && n > 0) {
    return(n)
  }
  cli::cli_warn("Ignoring {.field {field}}: {.val {x}} is not a positive number.")
  NULL
}

#' A number as CSS writes it: 120.5 -> "120.5", 100 -> "100"
#' @noRd
css_number_string <- function(n) {
  format(signif(n, 6), scientific = FALSE, trim = TRUE)
}

#' An http(s) site URL without its trailing slash, or NULL
#' @noRd
site_url_or_null <- function(x, field = "ui.meta.site-url") {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      grepl("^https?://[^\\s\"'<>]+$", x, perl = TRUE)
  ) {
    return(sub("/+$", "", x))
  }
  cli::cli_warn("Ignoring {.field {field}}: {.val {x}} is not an http(s) URL.")
  NULL
}

#' An inline favicon, so a site without one does not log a /favicon.ico 404
#' @noRd
default_favicon_link <- function() {
  svg <- paste0(
    "%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'%3E",
    "%3Crect width='32' height='32' rx='6' fill='%23B84130'/%3E",
    "%3Cpath d='M9 8h9a5 5 0 0 1 5 5v11H14a5 5 0 0 0-5-5z' fill='%23F8F1E0'/%3E",
    "%3C/svg%3E"
  )
  sprintf('<link rel="icon" type="image/svg+xml" href="data:image/svg+xml,%s" />', svg)
}

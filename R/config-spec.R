# The _webrarian.yml key table
#
# One entry per setting: its dotted snake_case path, type, default and a
# one-line description. apply_config_defaults() builds the defaults from it,
# validate_config() checks files and settings_set() calls against it, and the
# generated key reference is rendered from it, so the three cannot drift.
# The `brand` subtree is one entry: its keys belong to Posit's brand.yml.

#' @noRd
spec_entry <- function(key, type, default = NULL, doc, values = NULL) {
  list(key = key, type = type, default = default, doc = doc, values = values)
}

#' The table of _webrarian.yml settings
#' @noRd
config_spec <- function() {
  list(
    spec_entry(
      "project.name",
      "string",
      NULL,
      "Page title and the project name the viewer shows. Defaults to the collection's directory name."
    ),
    spec_entry("project.description", "string", "", "Description for the page's meta tags."),
    spec_entry(
      "webr.version",
      "version",
      NULL,
      "webR engine version, one of the versions tested with this webrarian. Defaults to WEBRARIAN_WEBR_VERSION, else the newest tested version."
    ),
    spec_entry(
      "packages.prebuilt",
      "strings",
      list(),
      "WebAssembly packages the page installs. They are bundled into the site's repo/ (build.bundle-engine or build.offline), and otherwise installed from the repositories when the page opens."
    ),
    spec_entry(
      "packages.repos",
      "strings",
      list(),
      "Extra WebAssembly package repositories, searched after repo.r-wasm.org when the site is built and, unless build.offline is true, by the page."
    ),
    spec_entry(
      "packages.github",
      "strings",
      list(),
      "GitHub packages (owner/repo[/subdir][@ref]), compiled to WebAssembly with Docker."
    ),
    spec_entry(
      "packages.local",
      "strings",
      list(),
      "Local package directories, compiled to WebAssembly with Docker."
    ),
    spec_entry("packages.dependencies", "flag", TRUE, "Also bundle the packages' dependencies."),
    spec_entry(
      "files.include",
      "strings",
      list(),
      "Files to bundle, as paths, directories (data/) or globs (**/*.R, *.{csv,rds})."
    ),
    spec_entry(
      "files.exclude",
      "strings",
      list("**/*.Rhistory", "**/.DS_Store"),
      "Patterns to leave out, gitignore style (*.log, data/raw/)."
    ),
    spec_entry(
      "files.mount_point",
      "string",
      "/home/web_user",
      "Directory in the browser's file system that holds the bundled files."
    ),
    spec_entry(
      "repl.startup_script",
      "string_or_null",
      NULL,
      "Script run once after the files are in place. It is part of the environment, so it also runs when a share link is opened. A shared file at the same path is written next to it as <stem>-shared<ext>."
    ),
    spec_entry(
      "repl.auto_open",
      "strings_or_null",
      NULL,
      "Files opened in the editor. When unset, the first bundled R file other than the startup script opens, and [] opens none."
    ),
    spec_entry(
      "repl.auto_run",
      "strings",
      list(),
      "Files run, in order, once the environment is ready."
    ),
    spec_entry(
      "repl.share_links",
      "choice",
      "open",
      "What a share link may do, which is \"open\" (add files and packages), \"fixed\" (add files only) or \"off\" (ignored, with no Share button). off may be written with or without quotes.",
      values = c("open", "fixed", "off")
    ),
    spec_entry(
      "repl.persist_edits",
      "flag",
      TRUE,
      "Keep each visitor's edits to the files open in the editor in their own browser, and restore them on their next visit. The viewer's Reset files button puts the site's files back. Any code that runs on the site's address can read the stored edits, including code a share link brings. With false, every visit starts from the site's files."
    ),
    spec_entry("repl.panels.editor", "flag", TRUE, "Show the editor."),
    spec_entry("repl.panels.terminal", "flag", TRUE, "Show the R console."),
    spec_entry("repl.panels.files", "flag", TRUE, "Show the file browser."),
    spec_entry("repl.panels.plot", "flag", TRUE, "Show the plot panel."),
    spec_entry("repl.panels.environment", "flag", TRUE, "Show the environment panel."),
    spec_entry("ui.loading.message", "string", "Loading webR...", "Text on the loading screen."),
    spec_entry(
      "ui.loading.custom_html",
      "string_or_null",
      NULL,
      "HTML that replaces the loading screen's contents. The screen and its status line, where a load failure and the Reload button appear, stay."
    ),
    spec_entry(
      "ui.custom_css",
      "string_or_null",
      NULL,
      "A CSS file, relative to the collection, to include in the page."
    ),
    spec_entry(
      "ui.meta.title",
      "string_or_null",
      NULL,
      "Page title. Defaults to the project name."
    ),
    spec_entry(
      "ui.meta.description",
      "string_or_null",
      NULL,
      "Meta description. Defaults to the project description."
    ),
    spec_entry(
      "ui.meta.og_image",
      "string_or_null",
      NULL,
      "Image for link previews, relative to the collection. Needs ui.meta.site-url."
    ),
    spec_entry("ui.meta.og_type", "string", "website", "Open Graph type."),
    spec_entry(
      "ui.meta.twitter_card",
      "choice",
      "summary",
      "Twitter card style, summary or summary_large_image.",
      values = c("summary", "summary_large_image")
    ),
    spec_entry(
      "ui.meta.site_url",
      "string_or_null",
      NULL,
      "The site's public URL, used to make link-preview URLs absolute."
    ),
    spec_entry(
      "brand",
      "brand",
      NULL,
      "A path to a brand.yml file, or an inline brand.yml mapping. Defaults to _brand.yml in the collection, if present."
    ),
    spec_entry(
      "build.output_dir",
      "string",
      "_site",
      "Output directory, inside the collection. bind() deletes and rewrites it."
    ),
    spec_entry(
      "build.offline",
      "flag",
      FALSE,
      "With true, the site makes no request to another origin. It carries the webR engine and its packages, and visitors (share links, the Packages tab, install.packages()) can add only bundled packages. With false, the page may also install packages from repo.r-wasm.org and packages.repos."
    ),
    spec_entry(
      "build.bundle_engine",
      "flag",
      TRUE,
      "Copy the webR engine and the prebuilt packages into the site. With false, the page loads the engine from the webR CDN and installs the packages when it opens, which makes a smaller site that needs the network. Setting build.offline to true always bundles both."
    ),
    spec_entry(
      "build.clean",
      "flag",
      TRUE,
      "Replace the whole previous site. false keeps files you added to it (a CNAME, say), never an old copy of a file the build writes. Either way the site is built in a staging directory and replaces the old one only when the build succeeds."
    ),
    spec_entry(
      "build.service_worker",
      "flag",
      FALSE,
      "Emit a service worker that caches the engine in visitors' browsers (for hosts that cannot set cache headers)."
    ),
    spec_entry(
      "build.library_image",
      "flag",
      TRUE,
      "When the site bundles packages in repo/, also ship them installed, as one library image the page mounts at start-up instead of installing each package on every visit. The packages stay in repo/ too. No image is written when a bundled package needs one the site does not bundle."
    )
  )
}

#' The table indexed by key
#' @noRd
config_spec_index <- function() {
  spec <- config_spec()
  stats::setNames(spec, vapply(spec, `[[`, character(1), "key"))
}

#' A settings path as it is spelled in the file
#' @noRd
display_key <- function(key) gsub("_", "-", key, fixed = TRUE)

#' The closest candidate, if it is close enough to be a typo
#' @noRd
nearest_key <- function(key, candidates) {
  if (length(candidates) == 0L) {
    return(NA_character_)
  }
  d <- utils::adist(key, candidates)[1, ]
  best <- which.min(d)
  if (d[[best]] <= max(2L, floor(nchar(key) / 3))) candidates[[best]] else NA_character_
}

#' Read a YAML boolean as the choice it can only mean
#'
#' The yaml package reads YAML 1.1, where an unquoted off, no or false is
#' FALSE. For a choice whose values include "off" (repl.share-links, whose
#' documented values are open|fixed|off), FALSE can only mean "off". TRUE is
#' left alone: on/yes could mean open or fixed, so it stays an error.
#' @param key Dotted snake_case key, e.g. "repl.share_links".
#' @noRd
normalize_config_value <- function(key, value) {
  entry <- config_spec_index()[[key]]
  if (
    !is.null(entry) && identical(entry$type, "choice") && "off" %in% entry$values && isFALSE(value)
  ) {
    return("off")
  }
  value
}

#' normalize_config_value() for every choice key present in a raw config
#' @noRd
normalize_config_values <- function(config) {
  if (!is.list(config) || is.null(names(config))) {
    return(config)
  }
  for (entry in config_spec()) {
    if (!identical(entry$type, "choice")) {
      next
    }
    keys <- strsplit(entry$key, ".", fixed = TRUE)[[1]]
    parent <- config
    for (k in keys[-length(keys)]) {
      parent <- if (is.list(parent) && !is.null(names(parent))) parent[[k]] else NULL
    }
    leaf <- keys[[length(keys)]]
    if (!is.list(parent) || is.null(names(parent)) || !leaf %in% names(parent)) {
      next
    }
    fixed <- normalize_config_value(entry$key, parent[[leaf]])
    if (!identical(fixed, parent[[leaf]])) {
      config <- set_nested_value(config, keys, fixed)
    }
  }
  config
}

#' Defaults as a nested list (explicit NULLs kept)
#' @noRd
config_defaults <- function() {
  out <- list()
  for (entry in config_spec()) {
    out <- set_nested_value(out, strsplit(entry$key, ".", fixed = TRUE)[[1]], entry$default)
  }
  out
}

#' Check one value against its table entry
#' @noRd
check_config_value <- function(key, value, entry, call = rlang::caller_env()) {
  is_string <- function(v) is.character(v) && length(v) == 1L && !is.na(v)
  is_strings <- function(v) {
    (is.character(v) && !anyNA(v)) ||
      (is.list(v) && is.null(names(v)) && all(vapply(v, is_string, logical(1))))
  }
  ok <- switch(
    entry$type,
    string = is_string(value),
    string_or_null = is_string(value),
    version = is_string(value),
    strings = is_strings(value),
    strings_or_null = is_strings(value),
    flag = is.logical(value) && length(value) == 1L && !is.na(value),
    choice = is_string(value) && value %in% entry$values,
    brand = is_string(value) || (is.list(value) && !is.null(names(value))),
    TRUE
  )
  if (isTRUE(ok)) {
    return(invisible(TRUE))
  }
  expected <- switch(
    entry$type,
    string = "a string",
    string_or_null = "a string or null",
    version = "a quoted version string such as \"0.6.0\"",
    strings = "a list of strings",
    strings_or_null = "a list of strings, or null",
    flag = "true or false",
    choice = paste0("one of ", paste0("\"", entry$values, "\"", collapse = ", ")),
    brand = "a path to a brand.yml file or an inline brand.yml mapping"
  )
  found <- if (is.list(value)) {
    "a mapping or list"
  } else {
    paste(class(value)[[1]], encodeString(format(value)[[1]], quote = "\""))
  }
  # YAML 1.1 turns a bare yes/no/on/off into a boolean before webrarian sees
  # it, so a boolean where text belongs is an unquoted word, in a list too
  # (the same rule as pyodidarian's bare-yaml-words-quoted-elsewhere).
  text_types <- c("string", "string_or_null", "version", "strings", "strings_or_null", "choice")
  has_boolean <- is.logical(value) ||
    (is.list(value) && any(vapply(value, is.logical, logical(1))))
  hint <- if (has_boolean && entry$type %in% text_types) {
    if (identical(entry$type, "choice")) {
      c(
        "i" = "YAML reads an unquoted yes, no, on or off as true or false; quote the value, for example {.val {entry$values[[1]]}}."
      )
    } else {
      c("i" = "YAML reads an unquoted yes, no, on or off as true or false; quote the value.")
    }
  }
  cli::cli_abort(
    c(
      "{.field {display_key(key)}} in {.file _webrarian.yml} must be {expected}.",
      "x" = "Found {found}.",
      hint
    ),
    call = call
  )
}

#' Validate a raw (snake_case) config against the key table
#'
#' Wrong types are errors. Unknown keys are warnings, or errors with
#' `strict = TRUE` (what settings_set() uses, since writing a key nothing
#' reads is never what the caller meant). NULL values are always allowed and
#' mean "use the default".
#' @return The warning messages, invisibly.
#' @noRd
validate_config <- function(config, strict = FALSE, call = rlang::caller_env()) {
  if (is.null(config)) {
    return(invisible(character()))
  }
  if (!is.list(config) || (length(config) > 0L && is.null(names(config)))) {
    cli::cli_abort("{.file _webrarian.yml} must be a mapping of settings.", call = call)
  }
  spec <- config_spec_index()
  keys <- names(spec)
  problems <- character()

  # A message embeds the key as the user typed it, so it is passed to cli as
  # data ("{message}"), never as the format string: a key such as `ui{x}`
  # would otherwise be evaluated as a glue expression.
  report <- function(message) {
    if (strict) {
      cli::cli_abort("{message}", call = call)
    }
    problems <<- c(problems, message)
  }

  walk <- function(node, prefix) {
    for (name in names(node)) {
      path <- c(prefix, name)
      key <- paste(path, collapse = ".")
      value <- node[[name]]
      if (key %in% keys) {
        if (!is.null(value)) {
          check_config_value(key, value, spec[[key]], call = call)
        }
        next
      }
      if (any(startsWith(keys, paste0(key, ".")))) {
        if (is.null(value)) {
          next
        }
        if (!is.list(value) || (length(value) > 0L && is.null(names(value)))) {
          cli::cli_abort("{.field {display_key(key)}} must be a mapping of settings.", call = call)
        }
        walk(value, path)
        next
      }
      level <- if (length(prefix) > 0L) {
        keys[startsWith(keys, paste0(paste(prefix, collapse = "."), "."))]
      } else {
        keys
      }
      siblings <- unique(vapply(
        strsplit(level, ".", fixed = TRUE),
        function(s) s[[length(prefix) + 1L]],
        character(1)
      ))
      guess <- nearest_key(name, siblings)
      report(
        if (is.na(guess)) {
          sprintf(
            if (strict) {
              "`%s` is not a webrarian setting."
            } else {
              "`%s` is not a webrarian setting and is ignored."
            },
            display_key(key)
          )
        } else {
          sprintf(
            if (strict) {
              "`%s` is not a webrarian setting. Did you mean `%s`?"
            } else {
              "`%s` is not a webrarian setting and is ignored. Did you mean `%s`?"
            },
            display_key(key),
            display_key(paste(c(prefix, guess), collapse = "."))
          )
        }
      )
    }
  }
  walk(config, character())

  for (message in problems) {
    cli::cli_warn("{message}", call = call)
  }
  invisible(problems)
}

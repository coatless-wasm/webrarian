# The color scheme in a built site, in Chrome: the loading screen and the
# brand follow the scheme in force (the site's pin, else the visitor's stored
# choice, else their system), from the first paint.

theme_page <- function(root, env = parent.frame()) {
  port <- httpuv::randomPort()
  server <- serve_dir_isolated(fs::path(root, "_site"), port)
  withr::defer(stop_dir_server(server), envir = env)
  page <- local_browser_page(sprintf("http://127.0.0.1:%d/", port), env = env)
  page$system <- function(scheme) {
    page$session$Emulation$setEmulatedMedia(
      features = list(list(name = "prefers-color-scheme", value = scheme))
    )
    invisible(TRUE)
  }
  page$root_var <- function(name) {
    tolower(page$eval(sprintf(
      "getComputedStyle(document.documentElement).getPropertyValue(%s).trim()",
      jsonlite::toJSON(name, auto_unbox = TRUE)
    )))
  }
  # The loading screen is hidden once the viewer mounts, not removed, so its
  # colors can still be read.
  page$loading_background <- function() {
    page$eval("getComputedStyle(document.getElementById('webrarian-loading')).backgroundColor")
  }
  page$stored_scheme <- function(scheme) {
    page$eval(sprintf(
      "localStorage.setItem('exlibris-theme:' + location.origin + '/', %s)",
      jsonlite::toJSON(scheme, auto_unbox = TRUE)
    ))
    page$navigate()
  }
  page$marked <- function() {
    page$eval("document.documentElement.getAttribute('data-theme') || 'none'")
  }
  page
}

# What a visitor does: the Settings gear, then a choice.
choose_from_gear <- function(page, choice) {
  page$eval("document.querySelector('button[aria-label=\"Settings\"]').click()")
  page$eval(sprintf(
    "Array.from(document.querySelectorAll('[role=\"menuitemradio\"]')).find(function (el) { return el.textContent === %s; }).click()",
    jsonlite::toJSON(choice, auto_unbox = TRUE)
  ))
}
has_gear <- "!!document.querySelector('button[aria-label=\"Settings\"]')"
# The workspace has mounted: its Files pane is on the page.
workspace_up <- "!!document.querySelector('[aria-label=\"Files Pane\"]')"

skip_unless_browser <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("httpuv")
  skip_if(is.null(chromote::find_chrome()), "No Chrome/Chromium available")
}

light_brand <- c(
  "color:",
  "  primary: '#B84130'",
  "  background: '#F8F1E0'",
  "  foreground: '#62291F'"
)

test_that("the loading screen and a light brand follow the system until the visitor chooses", {
  skip_unless_browser()
  root <- local_collection()
  writeLines(light_brand, fs::path(root, "_brand.yml"))
  suppressMessages(bind(root))
  page <- theme_page(root)

  page$system("light")
  page$navigate()
  expect_equal(page$marked(), "none")
  expect_equal(page$loading_background(), "rgb(248, 241, 224)")
  expect_equal(page$root_var("--bg-primary"), "#f8f1e0")

  page$system("dark")
  page$navigate()
  expect_equal(page$marked(), "none")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")
  expect_equal(page$root_var("--bg-primary"), "#1e1e1e")
  expect_equal(page$root_var("--accent-color"), "#b84130")
})

test_that("a stored choice beats the system, for the loading screen and the brand", {
  skip_unless_browser()
  root <- local_collection()
  writeLines(light_brand, fs::path(root, "_brand.yml"))
  suppressMessages(bind(root))
  page <- theme_page(root)

  # Light chosen on a dark system: the brand's own light palette.
  page$system("dark")
  page$navigate()
  page$stored_scheme("light")
  expect_equal(page$marked(), "light")
  expect_equal(page$loading_background(), "rgb(248, 241, 224)")
  expect_equal(page$root_var("--bg-primary"), "#f8f1e0")
  expect_equal(page$root_var("--text-primary"), "#62291f")

  # Dark chosen on a light system: exlibris's dark palette, the brand's accent.
  page$system("light")
  page$stored_scheme("dark")
  expect_equal(page$marked(), "dark")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")
  expect_equal(page$root_var("--bg-primary"), "#1e1e1e")
  expect_equal(page$root_var("--text-primary"), "#e0e0e0")
  expect_equal(page$root_var("--accent-color"), "#b84130")
})

test_that("a pinned site keeps its scheme whatever the system and the stored choice", {
  skip_unless_browser()
  root <- local_collection()
  suppressMessages(settings_set(root, "ui.theme" = "dark"))
  suppressMessages(bind(root))
  page <- theme_page(root)

  page$system("light")
  page$navigate()
  page$stored_scheme("light")
  expect_equal(page$marked(), "dark")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")
  expect_equal(page$root_var("--bg-primary"), "#1e1e1e")
})

test_that("a dark brand is dark on a light system, loading screen included", {
  skip_unless_browser()
  root <- local_collection()
  writeLines(
    c("color:", "  primary: '#18BC9C'", "  background: '#101820'", "  foreground: '#f2f2f2'"),
    fs::path(root, "_brand.yml")
  )
  suppressMessages(bind(root))
  page <- theme_page(root)

  page$system("light")
  page$navigate()
  expect_equal(page$marked(), "dark")
  expect_equal(page$loading_background(), "rgb(16, 24, 32)")
  expect_equal(page$root_var("--bg-primary"), "#101820")
  expect_equal(page$root_var("--text-primary"), "#f2f2f2")
})

test_that("the page itself applies a stored choice, before the viewer's code loads", {
  skip_unless_browser()
  root <- local_collection()
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  # Without the bundle, nothing but the page's own script can mark it.
  fs::file_delete(fs::dir_ls(site, regexp = "exlibris-r[^/]*\\.js$"))
  page <- theme_page(root)

  page$system("light")
  page$navigate()
  expect_equal(page$marked(), "none")
  expect_equal(page$loading_background(), "rgb(255, 255, 255)")

  page$stored_scheme("dark")
  expect_equal(page$marked(), "dark")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")

  # The same site by its file name: one stored choice for both addresses.
  page$navigate(paste0(page$url, "index.html"))
  expect_equal(page$marked(), "dark")

  # Anything but light or dark is not a choice.
  page$stored_scheme("purple")
  expect_equal(page$marked(), "none")
})

# The round trip a visitor makes, through the viewer itself: nothing here
# writes the stored choice by hand, so the viewer's key and the page's must
# agree for the reload to come back dark.
test_that("Dark from the gear is kept, and the reloaded page draws its loading screen dark", {
  skip_unless_browser()
  root <- local_collection()
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  page <- theme_page(root)
  page$session$Network$enable()
  page$session$Network$setCacheDisabled(cacheDisabled = TRUE)

  page$system("light")
  page$navigate()
  expect_true(page$wait_for(has_gear, timeout = 60))
  expect_equal(page$marked(), "none")
  choose_from_gear(page, "Dark")
  expect_true(page$wait_for(
    "document.documentElement.getAttribute('data-theme') === 'dark'",
    timeout = 10
  ))
  expect_equal(page$root_var("--bg-primary"), "#1e1e1e")

  # Reload with the viewer gone: only the page's own script can read the choice.
  fs::file_delete(fs::dir_ls(site, regexp = "exlibris-r[^/]*\\.js$"))
  page$navigate()
  expect_false(isTRUE(page$eval(has_gear)))
  expect_equal(page$marked(), "dark")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")
})

test_that("a pinned site offers no gear, and a site that is not pinned does", {
  skip_unless_browser()
  pinned <- local_collection()
  suppressMessages(settings_set(pinned, "ui.theme" = "dark"))
  suppressMessages(bind(pinned))
  page <- theme_page(pinned)
  page$system("light")
  page$navigate()
  expect_true(page$wait_for(workspace_up, timeout = 60))
  expect_false(isTRUE(page$eval(has_gear)))
  expect_equal(page$marked(), "dark")

  open <- local_collection()
  suppressMessages(bind(open))
  other <- theme_page(open)
  other$navigate()
  expect_true(other$wait_for(workspace_up, timeout = 60))
  expect_true(isTRUE(other$eval(has_gear)))
})

test_that("an author's own loading background holds on a dark system", {
  skip_unless_browser()
  root <- local_collection()
  writeLines("body #webrarian-loading { background: #F8F1E0; }", fs::path(root, "style.css"))
  suppressMessages(settings_set(root, "ui.custom-css" = "style.css"))
  suppressMessages(bind(root))
  page <- theme_page(root)
  page$system("dark")
  page$navigate()
  expect_equal(page$loading_background(), "rgb(248, 241, 224)")
})

test_that("the page's start-up script keeps its names to itself", {
  skip_unless_browser()
  root <- local_collection()
  suppressMessages(bind(root))
  page <- theme_page(root)
  page$navigate()
  expect_true(isTRUE(page$eval("typeof window.stored === 'undefined'")))
  expect_true(isTRUE(page$eval("window.root === document.getElementById('root')")))
})

test_that("a brand's light and dark logos each show in their scheme", {
  skip_unless_browser()
  root <- local_collection()
  writeLines(
    c("logo:", "  medium:", "    light: day.png", "    dark: night.png"),
    fs::path(root, "_brand.yml")
  )
  writeLines("day", fs::path(root, "day.png"))
  writeLines("night", fs::path(root, "night.png"))
  suppressMessages(bind(root))
  page <- theme_page(root)
  shown <- function() {
    page$eval(paste0(
      "['light', 'dark'].filter(function (v) { return getComputedStyle(",
      "document.querySelector('#webrarian-loading .webrarian-logo-' + v)).display !== 'none'; }).join(',')"
    ))
  }

  page$system("light")
  page$navigate()
  expect_equal(shown(), "light")
  expect_equal(page$loading_background(), "rgb(255, 255, 255)")

  page$system("dark")
  page$navigate()
  expect_equal(shown(), "dark")
  expect_equal(page$loading_background(), "rgb(30, 30, 30)")

  # A choice against the system takes its logo with it.
  page$stored_scheme("light")
  expect_equal(shown(), "light")
  page$system("light")
  page$stored_scheme("dark")
  expect_equal(shown(), "dark")
})

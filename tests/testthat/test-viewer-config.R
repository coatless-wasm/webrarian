# build_viewer_config() turns settings plus what bind() learned into the wire
# object exlibris reads (see the table atop R/viewer-config.R).

viewer_test_config <- function(repl = NULL, ...) {
  apply_config_defaults(list(
    project = list(name = "demo"),
    webr = list(version = "0.6.0"),
    repl = repl,
    ...
  ))
}

test_packages <- function(install = "dplyr") {
  list(install = install, repos = character(), repo_url = "./repo")
}

bundled <- c("analysis.R", "data/cars.csv", "scripts/setup.R", "init.R")

wire_for <- function(config = viewer_test_config(), files = bundled, packages = test_packages()) {
  build_viewer_config(config, files, "./", packages)
}

test_that("settings map onto the wire keys", {
  wire <- wire_for()
  expect_equal(wire[["schema-version"]], 1L)
  expect_equal(wire$engine, "webr")
  expect_equal(wire[["engine-version"]], "0.6.0")
  expect_equal(wire[["engine-base-url"]], "./")
  expect_equal(wire[["project-name"]], "demo")
  expect_equal(unlist(wire$packages$install), "dplyr")
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_equal(wire[["mount-point"]], "/home/web_user")
  expect_equal(wire[["share-links"]], "open")
  expect_equal(
    wire$panels,
    list(editor = TRUE, terminal = TRUE, files = TRUE, plot = TRUE, environment = TRUE)
  )
})

test_that("repl.panels switches individual panels", {
  wire <- wire_for(viewer_test_config(
    repl = list(panels = list(plot = FALSE, environment = FALSE))
  ))
  expect_false(wire$panels$plot)
  expect_false(wire$panels$environment)
  expect_true(wire$panels$editor)
})

test_that("repl.share-links is carried through and no override global is emitted", {
  for (mode in c("open", "fixed", "off")) {
    wire <- wire_for(viewer_test_config(repl = list(share_links = mode)))
    expect_equal(wire[["share-links"]], mode)
    expect_false(grepl("ALLOW_URL_OVERRIDE", inject_viewer_config(wire), fixed = TRUE))
  }
})

test_that("auto-open defaults to the first bundled R file", {
  wire <- wire_for(files = c("data/cars.csv", "R/b.R", "analysis.R"))
  expect_equal(unlist(wire[["auto-open"]]), "/home/web_user/R/b.R")
})

test_that("auto-open's default skips the startup script and opens the next R file", {
  cfg <- viewer_test_config(repl = list(startup_script = "init.R"))
  wire <- wire_for(cfg, files = c("data/cars.csv", "init.R", "analysis.R"))
  expect_equal(wire[["startup-script"]], "/home/web_user/init.R")
  expect_equal(unlist(wire[["auto-open"]]), "/home/web_user/analysis.R")
  # When the startup script is the only R file, nothing opens.
  only <- wire_for(cfg, files = c("data/cars.csv", "init.R"))
  expect_length(only[["auto-open"]], 0L)
})

test_that("auto-open [] opens nothing and a list opens exactly those files", {
  none <- wire_for(viewer_test_config(repl = list(auto_open = list())))
  expect_length(none[["auto-open"]], 0L)
  some <- wire_for(viewer_test_config(repl = list(auto_open = list("init.R", "data/cars.csv"))))
  expect_equal(
    unlist(some[["auto-open"]]),
    c("/home/web_user/init.R", "/home/web_user/data/cars.csv")
  )
})

test_that("auto-run and startup-script resolve by path, fall back to a unique basename, and de-duplicate", {
  cfg <- viewer_test_config(
    repl = list(
      auto_run = list("scripts/setup.R", "setup.R"),
      startup_script = "init.R"
    )
  )
  wire <- wire_for(cfg)
  expect_equal(unlist(wire[["auto-run"]]), "/home/web_user/scripts/setup.R")
  expect_equal(wire[["startup-script"]], "/home/web_user/init.R")
})

test_that("an entry that is not bundled, or is ambiguous, is an error naming the key", {
  expect_error(
    resolve_repl_files(viewer_test_config(repl = list(auto_run = list("orphan.R"))), bundled),
    "repl.auto-run"
  )
  expect_error(
    resolve_repl_files(viewer_test_config(repl = list(startup_script = "orphan.R")), bundled),
    "repl.startup-script"
  )
  expect_error(
    resolve_repl_files(
      viewer_test_config(repl = list(auto_run = list("setup.R"))),
      c("a/setup.R", "b/setup.R")
    ),
    "matches 2 bundled files"
  )
})

test_that("fetch paths are percent-encoded per segment; VFS paths are not", {
  wire <- wire_for(
    files = c(
      "data/my survey#1.csv",
      "donn\u00e9es/\u00e9t\u00e9%.R",
      "data/pct%41.csv"
    )
  )
  expect_equal(wire$files[[1]][["fetch-path"]], "vfs-files/data/my%20survey%231.csv")
  expect_equal(wire$files[[1]][["vfs-path"]], "/home/web_user/data/my survey#1.csv")
  expect_equal(wire$files[[2]][["fetch-path"]], "vfs-files/donn%C3%A9es/%C3%A9t%C3%A9%25.R")
  # name is the path relative to the mount point, not the basename.
  expect_equal(wire$files[[2]]$name, "donn\u00e9es/\u00e9t\u00e9%.R")
  expect_equal(wire$files[[1]]$name, "data/my survey#1.csv")
  # A name that already contains a %XX sequence is still encoded: the server
  # decodes pct%2541.csv to pct%41.csv; pct%41.csv would decode to pctA.csv.
  expect_equal(wire$files[[3]][["fetch-path"]], "vfs-files/data/pct%2541.csv")
  expect_equal(wire$files[[3]][["vfs-path"]], "/home/web_user/data/pct%41.csv")
})

test_that("a custom mount point is used for every VFS path", {
  wire <- wire_for(
    viewer_test_config(files = list(mount_point = "/home/web_user/course/")),
    files = "analysis.R"
  )
  expect_equal(wire[["mount-point"]], "/home/web_user/course/")
  expect_equal(wire$files[[1]][["vfs-path"]], "/home/web_user/course/analysis.R")
})

test_that("single-element arrays stay arrays and empty ones stay []", {
  json <- viewer_config_json(wire_for(files = "analysis.R"))
  expect_match(json, '"install":\\["dplyr"\\]')
  expect_match(json, '"auto-open":\\["/home/web_user/analysis\\.R"\\]')
  expect_match(json, '"files":\\[\\{')

  empty <- viewer_config_json(wire_for(
    viewer_test_config(repl = list(auto_open = list())),
    files = character(),
    packages = test_packages(character())
  ))
  expect_match(empty, '"install":\\[\\]')
  expect_match(empty, '"files":\\[\\]')
  expect_match(empty, '"auto-open":\\[\\]')
  expect_match(empty, '"auto-run":\\[\\]')
  expect_match(empty, '"startup-script":null')
})

test_that("the inline JSON has no '<' or raw line separators and still round-trips", {
  cfg <- viewer_test_config()
  cfg$project$name <- "evil</script><!--<script>\u2028x"
  json <- viewer_config_json(wire_for(cfg))
  expect_false(grepl("<", json, fixed = TRUE))
  expect_false(grepl("\u2028", json, fixed = TRUE))
  expect_equal(jsonlite::fromJSON(json)[["project-name"]], cfg$project$name)
})

test_that("an inline config can be read back and replaced in a page", {
  wire <- wire_for()
  html <- paste0(
    "<head></head><body>",
    inject_viewer_config(wire),
    '<script type="module" src="x.js"></script></body>'
  )
  expect_equal(read_inline_viewer_config(html)[["project-name"]], "demo")

  wire2 <- wire
  wire2[["project-name"]] <- "renamed \\1 C:\\data"
  html2 <- replace_inline_viewer_config(html, wire2)
  expect_equal(read_inline_viewer_config(html2)[["project-name"]], "renamed \\1 C:\\data")
  expect_match(html2, '<script type="module" src="x.js">', fixed = TRUE)
  expect_null(read_inline_viewer_config("<html></html>"))
})

test_that("bind() rejects an auto-run entry that is not bundled, before building", {
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "files.include" = list("precious.R"),
    "repl.auto-run" = list("missing.R")
  ))
  expect_error(suppressMessages(bind(root)), "repl.auto-run")
  expect_false(fs::dir_exists(fs::path(root, "_site")))
})

test_that("bind() writes the collection's repl settings into the page", {
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "files.include" = list("precious.R"),
    "repl.auto-run" = list("precious.R"),
    "repl.share-links" = "fixed",
    "repl.panels.plot" = FALSE
  ))
  suppressMessages(bind(root))
  wire <- read_site_config(fs::path(root, "_site"))
  expect_equal(unlist(wire[["auto-run"]]), "/home/web_user/precious.R")
  expect_equal(unlist(wire[["auto-open"]]), "/home/web_user/precious.R")
  expect_equal(wire[["share-links"]], "fixed")
  expect_false(wire$panels$plot)
  html <- paste(readLines(fs::path(root, "_site", "index.html")), collapse = "\n")
  expect_false(grepl("__VIEWER_ALLOW_URL_OVERRIDE__", html, fixed = TRUE))
})

# --- ui.theme: who chooses the color scheme ---
#
# auto (the default) leaves the scheme to each visitor: the viewer shows its
# Settings gear and no `theme` is written. light or dark pins the site.

dark_brand <- function() {
  normalize_brand(list(color = list(background = "#101820", foreground = "#f2f2f2")))
}

test_that("a site that leaves the scheme to visitors writes no theme", {
  expect_null(wire_for()$theme)
  expect_null(wire_for(viewer_test_config(ui = list(theme = "auto")))$theme)
})

test_that("ui.theme: light or dark pins the site", {
  for (scheme in c("light", "dark")) {
    expect_equal(wire_for(viewer_test_config(ui = list(theme = scheme)))$theme, scheme)
  }
})

test_that("a brand whose palette is dark pins the site to dark", {
  config <- viewer_test_config()
  config$.brand <- dark_brand()
  expect_equal(site_theme(config), "dark")
  expect_equal(wire_for(config)$theme, "dark")
})

test_that("a brand whose palette is light leaves the scheme to visitors", {
  config <- viewer_test_config()
  config$.brand <- normalize_brand(list(
    color = list(background = "#F8F1E0", foreground = "#62291F")
  ))
  expect_equal(site_theme(config), "auto")
  expect_null(wire_for(config)$theme)
})

test_that("ui.theme: light with a dark brand warns and stays dark", {
  config <- viewer_test_config(ui = list(theme = "light"))
  config$.brand <- dark_brand()
  expect_warning(
    theme <- site_theme(config),
    "the brand's palette is dark, so the site is pinned to dark"
  )
  expect_equal(theme, "dark")
})

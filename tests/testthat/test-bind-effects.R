# Every _webrarian.yml key changes the built site. Each entry of
# `effects` either builds a site with a non-default value, written in the
# file's hyphenated spelling, and checks the result, or names the test that
# already pins the key. local_collection() loads the engine from the webR CDN
# (build.bundle-engine: false), so these builds download nothing; the entries
# that need an engine in the site use local_fake_engine().

skip_on_cran()

effect_site <- function(settings = list(), files = list(), env = parent.frame()) {
  root <- local_collection(env = env)
  for (f in names(files)) {
    fs::dir_create(fs::path_dir(fs::path(root, f)))
    writeLines(files[[f]], fs::path(root, f))
  }
  if (length(settings) > 0L) {
    suppressMessages(do.call(settings_set, c(list(root), settings)))
  }
  suppressMessages(bind(root))
  site <- fs::path(fs::path_real(root), collection_settings(root)$build$output_dir)
  list(
    root = root,
    site = site,
    wire = read_site_config(site),
    html = paste(readLines(fs::path(site, "index.html"), warn = FALSE), collapse = "\n")
  )
}

vfs_paths <- function(wire) vapply(wire$files, function(f) f[["vfs-path"]], character(1))

panel_off <- function(panel) {
  function() {
    s <- effect_site(stats::setNames(list(FALSE), paste0("repl.panels.", panel)))
    expect_false(s$wire$panels[[panel]])
  }
}

effects <- list(
  project.name = function() {
    s <- effect_site(list("project.name" = "Effect Demo"))
    expect_equal(s$wire[["project-name"]], "Effect Demo")
    expect_match(s$html, "<title>Effect Demo</title>", fixed = TRUE)
  },
  project.description = function() {
    s <- effect_site(list("project.description" = "Described here"))
    expect_match(s$html, '<meta name="description" content="Described here" />', fixed = TRUE)
  },
  webr.version = function() {
    expect_warning(s <- effect_site(list("webr.version" = "0.6.1")), "not been tested")
    expect_equal(s$wire[["engine-version"]], "0.6.1")
    expect_equal(s$wire[["engine-base-url"]], "https://webr.r-wasm.org/v0.6.1/")
  },
  packages.prebuilt = function() {
    s <- effect_site(list("packages.prebuilt" = list("glue")))
    expect_equal(unlist(s$wire$packages$install), "glue")
  },
  packages.repos = function() {
    s <- effect_site(list("packages.repos" = list("https://example.r-universe.dev")))
    expect_true("https://example.r-universe.dev" %in% unlist(s$wire$packages$repos))
  },
  packages.github = "test-compiled-packages.R: a GitHub package is installed at boot under its package name, and ./repo is reachable online",
  packages.local = "test-compiled-packages.R: a local package is compiled, installed at boot, and its dependencies are bundled",
  packages.dependencies = function() {
    skip_if_not_installed("httpuv")
    local_fake_engine()
    local_isolated_cache()
    repo <- local_fixture_repo(list(
      alpha = c(Version = "1.0", Imports = "beta"),
      beta = c(Version = "1.0")
    ))
    local_mocked_bindings(default_repo_url = function() repo$url)
    root <- local_collection(bundle_engine = TRUE) # bundles its packages into repo/
    suppressMessages(settings_set(
      root,
      "packages.prebuilt" = list("alpha"),
      "packages.dependencies" = FALSE
    ))
    suppressMessages(bind(root))
    r_line <- resolve_webr_version(collection_settings(root)$webr$version)$r_version
    contrib <- fs::path(root, "_site", "repo", "bin", "emscripten", "contrib", r_line)
    expect_identical(unname(read.dcf(fs::path(contrib, "PACKAGES"))[, "Package"]), "alpha")
  },
  files.include = function() {
    s <- effect_site(list("files.include" = list("a.R")), files = list("a.R" = "a <- 1"))
    expect_true("/home/web_user/a.R" %in% vfs_paths(s$wire))
    expect_true(fs::file_exists(fs::path(s$site, "vfs-files", "a.R")))
  },
  files.exclude = function() {
    s <- effect_site(
      list("files.include" = list("data/"), "files.exclude" = list("data/private.csv")),
      files = list("data/public.csv" = "a", "data/private.csv" = "secret")
    )
    expect_true(fs::file_exists(fs::path(s$site, "vfs-files", "data", "public.csv")))
    expect_false(fs::file_exists(fs::path(s$site, "vfs-files", "data", "private.csv")))
  },
  files.mount_point = function() {
    s <- effect_site(
      list("files.include" = list("a.R"), "files.mount-point" = "/home/web_user/work"),
      files = list("a.R" = "a <- 1")
    )
    expect_equal(s$wire[["mount-point"]], "/home/web_user/work")
    expect_true("/home/web_user/work/a.R" %in% vfs_paths(s$wire))
  },
  repl.startup_script = function() {
    s <- effect_site(
      list("files.include" = list("init.R"), "repl.startup-script" = "init.R"),
      files = list("init.R" = "x <- 1")
    )
    expect_equal(s$wire[["startup-script"]], "/home/web_user/init.R")
  },
  repl.auto_open = function() {
    s <- effect_site(
      list("files.include" = list("a.R", "b.R"), "repl.auto-open" = list("b.R")),
      files = list("a.R" = "1", "b.R" = "2")
    )
    expect_equal(unlist(s$wire[["auto-open"]]), "/home/web_user/b.R")
  },
  repl.auto_run = function() {
    s <- effect_site(
      list("files.include" = list("a.R"), "repl.auto-run" = list("a.R")),
      files = list("a.R" = "1")
    )
    expect_equal(unlist(s$wire[["auto-run"]]), "/home/web_user/a.R")
  },
  repl.share_links = function() {
    s <- effect_site(list("repl.share-links" = "fixed"))
    expect_equal(s$wire[["share-links"]], "fixed")
  },
  repl.panels.editor = panel_off("editor"),
  repl.panels.terminal = panel_off("terminal"),
  repl.panels.files = panel_off("files"),
  repl.panels.plot = panel_off("plot"),
  repl.panels.environment = panel_off("environment"),
  ui.theme = function() {
    auto <- effect_site()
    expect_null(auto$wire$theme)
    expect_no_match(auto$html, "<html[^>]*data-theme", perl = TRUE)
    s <- effect_site(list("ui.theme" = "dark"))
    expect_identical(s$wire$theme, "dark")
    expect_match(s$html, "<html lang=\"en\" data-theme=\"dark\">", fixed = TRUE)
  },
  ui.loading.message = function() {
    s <- effect_site(list("ui.loading.message" = "Warming up the shelves"))
    expect_match(s$html, "Warming up the shelves", fixed = TRUE)
  },
  ui.loading.custom_html = function() {
    s <- effect_site(list("ui.loading.custom-html" = "<div id=\"my-splash\">Hello</div>"))
    expect_match(s$html, "id=\"my-splash\"", fixed = TRUE)
  },
  ui.custom_css = function() {
    s <- effect_site(
      list("ui.custom-css" = "style.css"),
      files = list("style.css" = "body { color: red; }")
    )
    expect_true(fs::file_exists(fs::path(s$site, "assets", "custom", "custom.css")))
    expect_match(s$html, 'href="assets/custom/custom.css"', fixed = TRUE)
  },
  ui.meta.title = function() {
    s <- effect_site(list("ui.meta.title" = "Meta Title"))
    expect_match(s$html, "<title>Meta Title</title>", fixed = TRUE)
  },
  ui.meta.description = function() {
    s <- effect_site(list("ui.meta.description" = "Meta description"))
    expect_match(
      s$html,
      '<meta property="og:description" content="Meta description" />',
      fixed = TRUE
    )
  },
  ui.meta.og_image = function() {
    s <- effect_site(
      list("ui.meta.og-image" = "card.png", "ui.meta.site-url" = "https://example.org/app"),
      files = list("card.png" = "not really a png")
    )
    expect_match(
      s$html,
      '<meta property="og:image" content="https://example.org/app/assets/custom/og-image.png" />',
      fixed = TRUE
    )
  },
  ui.meta.og_type = function() {
    s <- effect_site(list("ui.meta.og-type" = "article"))
    expect_match(s$html, '<meta property="og:type" content="article" />', fixed = TRUE)
  },
  ui.meta.twitter_card = function() {
    s <- effect_site(list("ui.meta.twitter-card" = "summary_large_image"))
    expect_match(s$html, '<meta name="twitter:card" content="summary_large_image" />', fixed = TRUE)
  },
  ui.meta.site_url = function() {
    s <- effect_site(list("ui.meta.site-url" = "https://example.org/app"))
    expect_match(
      s$html,
      '<meta property="og:url" content="https://example.org/app/" />',
      fixed = TRUE
    )
  },
  brand = function() {
    s <- effect_site(list("brand.color.primary" = "#1a2b3c"))
    expect_match(s$html, "--accent-color: #1a2b3c", fixed = TRUE)
  },
  build.output_dir = function() {
    s <- effect_site(list("build.output-dir" = "public"))
    expect_equal(as.character(fs::path_file(s$site)), "public")
    expect_true(fs::file_exists(fs::path(s$site, "index.html")))
    expect_false(fs::dir_exists(fs::path(s$root, "_site")))
  },
  build.offline = function() {
    expect_null(effect_site()$wire$offline)
    local_fake_engine()
    s <- effect_site(list("build.offline" = TRUE))
    expect_true(s$wire$offline)
    # Strict: the engine is in the site (build.bundle-engine: false is
    # overridden) and no package source is on another origin.
    expect_true(fs::dir_exists(fs::path(s$site, "webr")))
    expect_false(startsWith(s$wire[["engine-base-url"]], "https://"))
    sources <- c(s$wire$packages[["repo-url"]], unlist(s$wire$packages$repos))
    expect_false(any(grepl("^https?://", sources)))
  },
  build.bundle_engine = function() {
    expect_match(effect_site()$wire[["engine-base-url"]], "^https://webr\\.r-wasm\\.org/")
    local_fake_engine()
    s <- effect_site(list("build.bundle-engine" = TRUE))
    expect_true(fs::dir_exists(fs::path(s$site, "webr")))
    expect_false(startsWith(s$wire[["engine-base-url"]], "https://"))
    expect_null(s$wire$offline)
  },
  build.clean = function() {
    # clean: false keeps a file the build does not write, never a stale one
    # under a path the build owns: the build is still staged and swapped in
    # (the meaning pyodidarian's build.clean has).
    root <- local_collection()
    suppressMessages(bind(root))
    writeLines("stays", fs::path(root, "_site", "extra.txt"))
    fs::dir_create(fs::path(root, "_site", "vfs-files"))
    writeLines("stale", fs::path(root, "_site", "vfs-files", "stale.R"))
    suppressMessages(settings_set(root, "build.clean" = FALSE))
    suppressMessages(bind(root))
    expect_true(fs::file_exists(fs::path(root, "_site", "extra.txt")))
    expect_false(fs::file_exists(fs::path(root, "_site", "vfs-files", "stale.R")))
  },
  build.service_worker = function() {
    s <- effect_site(list("build.service-worker" = TRUE))
    expect_match(s$html, 'navigator.serviceWorker.register("sw.js")', fixed = TRUE)
  },
  repl.persist_edits = function() {
    expect_true(effect_site()$wire[["persist-edits"]])
    s <- effect_site(list("repl.persist-edits" = FALSE))
    expect_false(s$wire[["persist-edits"]])
  },
  build.library_image = "test-library-image.R: build.library-image: false ships no library image"
)

test_that("every _webrarian.yml key has an effect test", {
  keys <- vapply(config_spec(), `[[`, character(1), "key")
  expect_setequal(names(effects), keys)
})

for (key in names(effects)) {
  effect <- effects[[key]]
  if (is.character(effect)) {
    local({
      file <- sub(":.*$", "", effect)
      name <- sub("^[^:]+: ", "", effect)
      test_that(sprintf("%s is pinned by %s", key, file), {
        path <- test_path(file)
        expect_true(file.exists(path))
        expect_true(any(grepl(name, readLines(path, warn = FALSE), fixed = TRUE)))
      })
    })
  } else {
    local({
      check <- effect
      test_that(sprintf("%s changes the built site", key), check())
    })
  }
}

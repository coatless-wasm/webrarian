# Where the page installs packages from (offline sites and share links): the site's own ./repo when it bundles packages,
# then, unless the site is offline, repo.r-wasm.org and packages.repos.
# exlibris installs the boot list from c(repo-url, repos), and its webR driver
# gives the console's install.packages() the same list (webr_pkg_repos).

# Stands in for the package download: glue lands in the site's repo/.
glue_download <- function(packages, pkg_dir, ...) {
  ensure_dir(pkg_dir)
  writeLines("x", fs::path(pkg_dir, "glue_1.8.0.tgz"))
  invisible(NULL)
}

# Where a site that carries its engine keeps it: under webr/v<version>/, and
# nowhere else.
expect_bundled_engine <- function(site, wire) {
  version <- wire[["engine-version"]]
  expect_equal(wire[["engine-base-url"]], sprintf("./webr/v%s/", version))
  expect_true(fs::file_exists(fs::path(site, "webr", paste0("v", version), "R.wasm")))
  expect_false(fs::file_exists(fs::path(site, "R.wasm")))
}

test_that("viewer_package_repos covers the four cases", {
  # Nothing bundled, online: the public repository, then the configured ones.
  expect_equal(
    viewer_package_repos(FALSE, FALSE, config_repos = "https://x.r-universe.dev"),
    list(repo_url = "https://repo.r-wasm.org", repos = "https://x.r-universe.dev")
  )
  # Nothing bundled, offline: no source at all. A visitor's install ends with
  # a ✗ and a reason, never a request to another origin.
  expect_equal(
    viewer_package_repos(TRUE, FALSE, config_repos = "https://x.r-universe.dev"),
    list(repo_url = NULL, repos = character())
  )
  # A bundled repo/, offline: only it.
  expect_equal(
    viewer_package_repos(TRUE, TRUE, config_repos = "https://x.r-universe.dev"),
    list(repo_url = "./repo", repos = character())
  )
  # A bundled repo/, online: it first, so the bundled (or compiled
  # development) version is the one that installs; then the public ones.
  expect_equal(
    viewer_package_repos(FALSE, TRUE, config_repos = "https://x.r-universe.dev"),
    list(repo_url = "./repo", repos = c("https://repo.r-wasm.org", "https://x.r-universe.dev"))
  )
})

test_that("a default build carries its engine and lets the page install from the public repository", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(bind(root))

  site <- fs::path(root, "_site")
  wire <- read_site_config(site)
  expect_bundled_engine(site, wire)
  expect_equal(wire$packages[["repo-url"]], "https://repo.r-wasm.org")
  expect_null(wire$offline)
  expect_false(fs::dir_exists(fs::path(site, "repo")))
})

test_that("a default build bundles its packages and searches ./repo before the public repositories", {
  local_fake_engine()
  local_mocked_bindings(download_webr_packages = glue_download)
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("glue"),
    "packages.repos" = list("https://x.r-universe.dev")
  ))
  suppressMessages(bind(root))

  wire <- read_site_config(fs::path(root, "_site"))
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_equal(
    unlist(wire$packages$repos),
    c("https://repo.r-wasm.org", "https://x.r-universe.dev")
  )
  expect_equal(unlist(wire$packages$install), "glue")
  expect_null(wire$offline)
})

# Parent spec, "Offline sites and share links": on an offline site a link's
# ?packages=, the Packages tab and install.packages() reach bundled packages
# only; build.offline: false is the way to let visitors add any published one.
test_that("an offline build installs only from ./repo and tells the viewer it is offline", {
  local_fake_engine()
  local_mocked_bindings(download_webr_packages = glue_download)
  # build.bundle-engine is false here: an offline site carries its engine and
  # packages all the same.
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("glue"),
    "packages.repos" = list("https://x.r-universe.dev")
  ))
  msgs <- testthat::capture_messages(bind(root, offline = TRUE))
  expect_true(any(grepl("bundle-engine", msgs, fixed = TRUE)))

  site <- fs::path(root, "_site")
  wire <- read_site_config(site)
  expect_true(wire$offline)
  expect_bundled_engine(site, wire)
  expect_equal(wire$packages[["repo-url"]], "./repo")
  expect_length(wire$packages$repos, 0L)
  expect_equal(unlist(wire$packages$install), "glue")
})

test_that("an offline build with no packages names no package source at all", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(root, "build.offline" = TRUE))
  suppressMessages(bind(root))

  wire <- read_site_config(fs::path(root, "_site"))
  expect_true(wire$offline)
  # exlibris's webR driver then searches nothing: install.packages() and a
  # link's ?packages= end with a ✗ and a reason.
  expect_null(wire$packages[["repo-url"]])
  expect_length(wire$packages$repos, 0L)
})

test_that("build.bundle-engine: false loads the engine from the CDN and installs packages when the page opens", {
  # Never used when the key is read; before Step 3 it keeps the failing run
  # from downloading the real engine.
  local_fake_engine()
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.prebuilt" = list("glue"),
    "packages.repos" = list("https://x.r-universe.dev")
  ))
  suppressMessages(bind(root))

  version <- collection_settings(root)$webr$version
  wire <- read_site_config(fs::path(root, "_site"))
  expect_equal(wire[["engine-base-url"]], sprintf("https://webr.r-wasm.org/v%s/", version))
  expect_equal(wire$packages[["repo-url"]], "https://repo.r-wasm.org")
  expect_equal(unlist(wire$packages$repos), "https://x.r-universe.dev")
  expect_false(fs::dir_exists(fs::path(root, "_site", "repo")))
})

test_that("a new collection is online and carries its engine", {
  dir <- withr::local_tempdir()
  suppressMessages(catalog(dir, detect = FALSE))
  build <- collection_settings(dir)$build
  expect_false(build$offline)
  expect_true(build$bundle_engine)
})

test_that("bind() rejects a non-logical offline value", {
  root <- local_collection()
  expect_error(suppressMessages(bind(root, offline = "yes")), "TRUE or FALSE")
})

test_that("bind() takes offline and no longer takes bundle_webr", {
  # Checked on the signature rather than with a call: Step 4 rewrites the old
  # argument throughout the tests, and a call written here would become a
  # valid one.
  args <- names(formals(bind))
  expect_true("offline" %in% args)
  expect_false("bundle_webr" %in% args)
})

test_that("the page resolves webR's URLs right after the config and before the viewer", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(bind(root))
  html <- paste(readLines(fs::path(root, "_site", "index.html"), warn = FALSE), collapse = "\n")
  config_at <- regexpr("<script>window.__VIEWER_CONFIG__ = ", html, fixed = TRUE)
  resolver_at <- regexpr("document.baseURI", html, fixed = TRUE)
  module_at <- regexpr('<script type="module"', html, fixed = TRUE)
  expect_gt(config_at, 0L)
  expect_gt(resolver_at, config_at)
  expect_gt(module_at, resolver_at)
  # The config itself stays page-relative; only the browser makes it absolute.
  expect_match(read_site_config(fs::path(root, "_site"))[["engine-base-url"]], "^\\./")
})

test_that("the resolver makes relative engine and repository URLs absolute against the page", {
  skip_if(!nzchar(Sys.which("node")), "node not available")
  wire <- list(
    `engine-base-url` = "./webr/v0.6.0/",
    packages = list(
      `repo-url` = "./repo",
      repos = list("https://repo.r-wasm.org", "../shared/repo")
    )
  )
  body <- sub("</script>$", "", sub("^<script>", "", viewer_url_resolver_script()))
  js <- fs::path(withr::local_tempdir(), "resolver.js")
  writeLines(
    c(
      sprintf("var window = { __VIEWER_CONFIG__: %s };", viewer_config_json(wire)),
      "var document = { baseURI: 'https://user.github.io/course/index.html?x=1' };",
      body,
      "process.stdout.write(JSON.stringify(window.__VIEWER_CONFIG__));"
    ),
    js
  )
  out <- jsonlite::fromJSON(
    paste(system2("node", shQuote(js), stdout = TRUE), collapse = ""),
    simplifyVector = FALSE
  )
  expect_equal(out[["engine-base-url"]], "https://user.github.io/course/webr/v0.6.0/")
  expect_equal(out$packages[["repo-url"]], "https://user.github.io/course/repo")
  # An absolute URL is left exactly as written (no trailing slash added).
  expect_equal(
    unlist(out$packages$repos),
    c("https://repo.r-wasm.org", "https://user.github.io/shared/repo")
  )
})

test_that("the resolver leaves a CDN engine and a config without package sources alone", {
  skip_if(!nzchar(Sys.which("node")), "node not available")
  wire <- list(`engine-base-url` = "https://webr.r-wasm.org/v0.6.0/", offline = TRUE)
  body <- sub("</script>$", "", sub("^<script>", "", viewer_url_resolver_script()))
  js <- fs::path(withr::local_tempdir(), "resolver.js")
  writeLines(
    c(
      sprintf("var window = { __VIEWER_CONFIG__: %s };", viewer_config_json(wire)),
      "var document = { baseURI: 'http://127.0.0.1:8000/' };",
      body,
      "process.stdout.write(JSON.stringify(window.__VIEWER_CONFIG__));"
    ),
    js
  )
  out <- jsonlite::fromJSON(
    paste(system2("node", shQuote(js), stdout = TRUE), collapse = ""),
    simplifyVector = FALSE
  )
  expect_equal(out, list(`engine-base-url` = "https://webr.r-wasm.org/v0.6.0/", offline = TRUE))
})

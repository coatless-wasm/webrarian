# Every site carries LICENSES/: what it redistributes, under which license,
# and where the source is.

licenses_of <- function(root) fs::path(root, "_site", "LICENSES")
read_md <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# The default site is online (build.offline: false) and still
# carries its engine and packages (build.bundle-engine: true).
test_that("the default online site carries the engine's GPL notice and lists its bundled packages", {
  skip_if_not_installed("httpuv")
  local_fake_engine()
  local_isolated_cache()
  repo <- local_fixture_repo(list(
    glue = c(Version = "1.8.0", License = "MIT + file LICENSE"),
    gpltool = c(Version = "2.1", License = "GPL (>= 2)")
  ))
  local_mocked_bindings(default_repo_url = function() repo$url)
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue", "gpltool")))
  suppressMessages(bind(root))
  expect_false(isTRUE(read_site_config(fs::path(root, "_site"))$offline))
  lic <- licenses_of(root)

  # The names both tools share (decision 9); no LICENSES/EXCEPTION.md.
  expect_setequal(
    as.character(fs::path_file(fs::dir_ls(lic))),
    c("webrarian.md", "exlibris.md", "THIRD-PARTY-r.md", "webR.md", "PACKAGES.md", "index.html")
  )

  own <- read_md(fs::path(lic, "webrarian.md"))
  expect_match(
    own,
    "Additional permission under GNU AGPL version 3 section 7 (output exception)",
    fixed = TRUE
  )
  expect_match(own, "The files that webrarian writes into a generated site or mirror", fixed = TRUE)
  expect_match(
    own,
    sprintf("webrarian %s generated this site", as.character(utils::packageVersion("webrarian"))),
    fixed = TRUE
  )

  prov <- jsonlite::read_json(system.file("viewer", "PROVENANCE.json", package = "webrarian"))
  exlibris <- read_md(fs::path(lic, "exlibris.md"))
  expect_match(
    exlibris,
    sprintf("https://github.com/coatless-wasm/exlibris/tree/%s", prov$exlibrisCommit),
    fixed = TRUE
  )
  expect_match(exlibris, "exlibris runtime exception", fixed = TRUE)
  expect_match(exlibris, "GNU AFFERO GENERAL PUBLIC LICENSE", fixed = TRUE)
  bundle <- fs::path_file(fs::dir_ls(
    fs::path(root, "_site"),
    regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"
  ))
  expect_match(exlibris, bundle, fixed = TRUE)
  # The bundles' banners name exlibris's own notice files; the exception is
  # reproduced verbatim.
  expect_match(exlibris, "`EXCEPTION.md` is the exlibris runtime exception", fixed = TRUE)
  expect_match(exlibris, "[THIRD-PARTY-r.md](THIRD-PARTY-r.md)", fixed = TRUE)
  exception <- readLines(
    system.file("viewer", "EXCEPTION.md", package = "webrarian"),
    encoding = "UTF-8"
  )
  expect_match(exlibris, paste(exception, collapse = "\n"), fixed = TRUE)

  expect_identical(
    readLines(fs::path(lic, "THIRD-PARTY-r.md"), encoding = "UTF-8"),
    readLines(system.file("viewer", "THIRD-PARTY.md", package = "webrarian"), encoding = "UTF-8")
  )
  expect_match(
    read_md(fs::path(lic, "THIRD-PARTY-r.md")),
    "## Code nested inside bundled packages",
    fixed = TRUE
  )

  webr <- read_md(fs::path(lic, "webR.md"))
  expect_match(webr, "`webr/v0.6.0/`", fixed = TRUE)
  expect_match(webr, "https://github.com/r-wasm/webr/tree/v0.6.0", fixed = TRUE)
  expect_match(webr, "https://cran.r-project.org/src/base/R-4/R-4.6.0.tar.gz", fixed = TRUE)
  expect_match(webr, "GNU GENERAL PUBLIC LICENSE", fixed = TRUE)
  expect_match(webr, sprintf("`webr` %s", prov$webrClientVersion), fixed = TRUE)

  pkgs <- read_md(fs::path(lic, "PACKAGES.md"))
  expect_match(
    pkgs,
    sprintf("| glue | 1.8.0 | MIT + file LICENSE | <%s/src/contrib/glue_1.8.0.tar.gz> |", repo$url),
    fixed = TRUE
  )
  expect_match(
    pkgs,
    sprintf("| gpltool | 2.1 | GPL (>= 2) | <%s/src/contrib/gpltool_2.1.tar.gz> |", repo$url),
    fixed = TRUE
  )
  expect_match(pkgs, "### GPL-2", fixed = TRUE)
  expect_match(pkgs, "### GPL-3", fixed = TRUE)

  index <- read_md(fs::path(lic, "index.html"))
  for (f in c("webrarian.md", "exlibris.md", "THIRD-PARTY-r.md", "webR.md", "PACKAGES.md")) {
    expect_match(index, sprintf('href="%s"', f), fixed = TRUE)
  }
  expect_match(index, "EXCEPTION.md", fixed = TRUE)
})

test_that("a site that loads its engine from the CDN says so and carries no GPL runtime text", {
  root <- local_collection() # build.bundle-engine: false
  suppressMessages(settings_set(root, "packages.prebuilt" = list("glue")))
  suppressMessages(bind(root))
  webr <- read_md(fs::path(licenses_of(root), "webR.md"))
  expect_match(webr, "is not part of this site", fixed = TRUE)
  expect_match(webr, "https://webr.r-wasm.org/v0.6.0/", fixed = TRUE)
  expect_false(grepl("GNU GENERAL PUBLIC LICENSE", webr, fixed = TRUE))

  pkgs <- read_md(fs::path(licenses_of(root), "PACKAGES.md"))
  expect_match(pkgs, "This site bundles no R packages.", fixed = TRUE)
  expect_match(
    pkgs,
    "not redistributed by this site: glue (from <https://repo.r-wasm.org>)",
    fixed = TRUE
  )
  expect_false(grepl("| glue |", pkgs, fixed = TRUE))
})

test_that("a GitHub package from a mono-repo gets its own source link", {
  gh_src <- withr::local_tempdir()
  fs::dir_create(fs::path(gh_src, "praise"))
  writeLines(
    c("Package: praise", "Version: 1.0.0.9000", "License: MIT + file LICENSE"),
    fs::path(gh_src, "praise", "DESCRIPTION")
  )
  local_fake_docker("ok", github = gh_src)
  root <- local_collection()
  suppressMessages(settings_set(
    root,
    "packages.github" = list("someone/tools-mono/pkgs/praise@v2", "other/unrelated")
  ))
  suppressMessages(bind(root))
  pkgs <- read_md(fs::path(licenses_of(root), "PACKAGES.md"))
  expect_match(
    pkgs,
    "| praise | 1.0.0.9000 | MIT + file LICENSE | <https://github.com/someone/tools-mono/tree/v2/pkgs/praise> |",
    fixed = TRUE
  )
})

test_that("a local package is named as the collection's own directory", {
  local_fake_docker("ok")
  root <- local_collection()
  pkg <- fs::path(root, "pkgs", "demotools")
  fs::dir_create(pkg)
  writeLines(
    c("Package: demotools", "Version: 0.1.0", "License: MIT"),
    fs::path(pkg, "DESCRIPTION")
  )
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/demotools")))
  suppressMessages(expect_no_message(bind(root), class = "webrarian_message_gpl_local"))
  pkgs <- read_md(fs::path(licenses_of(root), "PACKAGES.md"))
  expect_match(
    pkgs,
    "| demotools | 0.1.0 | MIT | local package `pkgs/demotools` in the collection |",
    fixed = TRUE
  )
})

# A local directory has no public source link, and the GPL makes whoever
# distributes the binary offer its source.
test_that("a GPL local package draws a reminder that whoever publishes the site must offer its source", {
  local_fake_docker("ok")
  root <- local_collection()
  pkg <- fs::path(root, "pkgs", "gpldemo")
  fs::dir_create(pkg)
  writeLines(
    c("Package: gpldemo", "Version: 0.1.0", "License: GPL-3"),
    fs::path(pkg, "DESCRIPTION")
  )
  suppressMessages(settings_set(root, "packages.local" = list("pkgs/gpldemo")))
  suppressMessages(expect_message(
    bind(root),
    "gpldemo",
    class = "webrarian_message_gpl_local"
  ))
  pkgs <- read_md(fs::path(licenses_of(root), "PACKAGES.md"))
  expect_match(
    pkgs,
    "| gpldemo | 0.1.0 | GPL-3 | local package `pkgs/gpldemo` in the collection |",
    fixed = TRUE
  )
})

test_that("only local packages under a GPL-family license draw the source reminder", {
  bundled <- data.frame(
    package = c("gpldemo", "mitdemo", "gh", "cran"),
    version = "1.0",
    license = c("LGPL (>= 2.1)", "MIT", "GPL-3", "GPL-2"),
    source = c(
      "local package `pkgs/gpldemo` in the collection",
      "local package `pkgs/mitdemo` in the collection",
      "https://github.com/o/gh/tree/HEAD",
      "https://repo.r-wasm.org/src/contrib/cran_1.0.tar.gz"
    ),
    stringsAsFactors = FALSE
  )
  expect_message(hits <- alert_gpl_local_packages(bundled), class = "webrarian_message_gpl_local")
  expect_identical(hits, "gpldemo")
  expect_no_message(none <- alert_gpl_local_packages(bundled[2:4, ]))
  expect_identical(none, character())
})

test_that("an untested webR patch still gets a webR.md", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(settings_set(root, "webr.version" = "0.6.1"))
  expect_warning(suppressMessages(bind(root)), "not been tested")
  webr <- read_md(fs::path(licenses_of(root), "webR.md"))
  expect_match(webr, "webR 0.6.1 (R 4.6.x)", fixed = TRUE)
  expect_match(webr, "<https://cran.r-project.org/src/base/>", fixed = TRUE)
})

test_that("the site root holds the page and the bundle, and no stray notice files", {
  root <- local_collection()
  suppressMessages(bind(root))
  for (f in c("THIRD-PARTY.md", "LICENSE.webR.md", "EXCEPTION.md", "PROVENANCE.json")) {
    expect_false(fs::file_exists(fs::path(root, "_site", f)), info = f)
  }
})

test_that("_headers revalidates LICENSES/ and serves its notes as text", {
  rules <- cache_rules()
  paths <- vapply(rules, `[[`, character(1), "path")
  expect_equal(rules[[match("/LICENSES/*", paths)]]$headers[["Cache-Control"]], "no-cache")
  expect_equal(
    rules[[match("/LICENSES/*.md", paths)]]$headers[["Content-Type"]],
    "text/plain; charset=utf-8"
  )

  root <- local_collection()
  suppressMessages(bind(root))
  headers <- read_md(fs::path(root, "_site", "_headers"))
  expect_match(headers, "/LICENSES/*.md", fixed = TRUE)
  # _headers is the only source of cache rules (decision 6): a generated
  # netlify.toml sets none.
  toml <- read_md(suppressMessages(circulate_via_netlify(root))[[1]])
  expect_false(grepl("LICENSES", toml, fixed = TRUE))
  expect_false(grepl("[[headers]]", toml, fixed = TRUE))
})

test_that("GitHub specs and prebuilt contrib URLs become source links", {
  expect_equal(
    parse_github_spec("r-lib/cli"),
    list(owner = "r-lib", repo = "cli", subdir = NULL, ref = "HEAD")
  )
  expect_equal(
    github_source_url("user/mono/sub/dir@v1.2"),
    "https://github.com/user/mono/tree/v1.2/sub/dir"
  )
  expect_null(parse_github_spec("not a spec"))
  expect_equal(
    prebuilt_source_url("https://repo.r-wasm.org/bin/emscripten/contrib/4.6", "glue", "1.8.1"),
    "https://repo.r-wasm.org/src/contrib/glue_1.8.1.tar.gz"
  )
  expect_named(
    standard_license_texts(c("GPL-2 | GPL-3", "LGPL-2.1", "MIT + file LICENSE", NA)),
    c("GPL-2", "GPL-3", "LGPL-2.1")
  )
  expect_named(standard_license_texts("AGPL-3"), "AGPL-3")
  expect_named(standard_license_texts("GPL"), c("GPL-2", "GPL-3"))
  expect_named(standard_license_texts("LGPL"), c("LGPL-2", "LGPL-2.1", "LGPL-3"))
  expect_named(standard_license_texts("GPL(>= 2)"), c("GPL-2", "GPL-3"))
  expect_named(standard_license_texts("GPL (> 2)"), "GPL-3")
  expect_named(standard_license_texts("LGPL-2"), "LGPL-2")
  expect_named(standard_license_texts("LGPL (>= 2.1)"), c("LGPL-2.1", "LGPL-3"))
  expect_named(standard_license_texts("GPL-3.0-only"), c("GPL-2", "GPL-3"))
  expect_length(standard_license_texts(c("MIT + file LICENSE", NA)), 0L)
})

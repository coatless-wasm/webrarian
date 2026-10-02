# Runtime caching: what the *visitor's browser* is told it may keep.
#
# Two mechanisms, tested here at the file level; the cross-origin-isolation
# claim the service worker makes is proved for real in test-browser-sw.R.
#
#   _site/_headers   Cache-Control for hosts that read it (Netlify, Cloudflare
#                    Pages). Always emitted.
#   _site/sw.js      opt-in service worker (`build: service-worker: true`),
#                    which is the only thing that helps on GitHub Pages.
#
# The invariant that matters most in the header file is that no two rules set
# the *same* header: Netlify and Cloudflare both concatenate the values of a
# header several matching rules set, so an overlap emits nonsense such as
# `Cache-Control: immutable, no-cache`.

# Parse a _headers file into a named list: pattern -> named character vector of
# headers. Mirrors the hosts' own format (a path line in column 0, indented
# `Name: value` lines beneath it, `#` comments and blanks ignored).
parse_headers_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]

  rules <- list()
  current <- NULL
  for (line in lines) {
    if (!grepl("^\\s", line)) {
      current <- trimws(line)
      rules[[current]] <- character()
    } else {
      expect_false(is.null(current), info = "header line before any path line")
      kv <- sub("^\\s+", "", line)
      name <- sub(":.*$", "", kv)
      value <- trimws(sub("^[^:]+:", "", kv))
      rules[[current]][[name]] <- value
    }
  }
  rules
}

# A minimal collection that builds with no download: the engine comes from the
# webR CDN (build.bundle-engine: false).
local_built_site <- function(..., env = parent.frame()) {
  proj <- withr::local_tempdir(.local_envir = env)
  suppressMessages(catalog(proj))
  settings <- c(list("build.bundle-engine" = FALSE), list(...))
  suppressMessages(do.call(settings_set, c(list(proj), settings)))
  suppressMessages(bind(proj))
  list(project = proj, site = fs::path(proj, "_site"))
}

read_index <- function(site) {
  paste(readLines(fs::path(site, "index.html"), warn = FALSE), collapse = "\n")
}

# ---------------------------------------------------------------------------
# _headers
# ---------------------------------------------------------------------------

test_that("bind() emits a _headers file into the built site", {
  built <- local_built_site()
  expect_true(fs::file_exists(fs::path(built$site, "_headers")))
})

test_that("_headers marks only versioned or content-hashed paths immutable", {
  built <- local_built_site()
  rules <- parse_headers_file(fs::path(built$site, "_headers"))

  immutable <- "public, max-age=31536000, immutable"
  expect_equal(rules[["/webr/*"]][["Cache-Control"]], immutable)
  expect_equal(rules[["/exlibris-r.*"]][["Cache-Control"]], immutable)
  expect_equal(rules[["/repo/*"]][["Cache-Control"]], "no-cache")

  for (pattern in names(rules)) {
    # rules[[pattern]] is an atomic named vector (parse_headers_file()), and
    # `[["Cache-Control"]]` errors rather than returning NULL when a rule
    # (e.g. the COOP/COEP-only "/*") carries no such header, so check first.
    headers <- rules[[pattern]]
    if (
      "Cache-Control" %in%
        names(headers) &&
        identical(unname(headers[["Cache-Control"]]), immutable)
    ) {
      expect_true(pattern %in% c("/webr/*", "/exlibris-r.*", "/library/*"), info = pattern)
    }
  }
  expect_false(any(c("/R.bin.data", "/webr.mjs", "/R.js", "/repo/*.tgz") %in% names(rules)))
})

test_that("a build that carries its engine puts it under webr/v<version>/ and points the page there", {
  local_fake_engine()
  root <- local_collection(bundle_engine = TRUE)
  suppressMessages(bind(root))
  site <- fs::path(root, "_site")
  version <- collection_settings(root)$webr$version

  expect_true(fs::file_exists(fs::path(site, "webr", paste0("v", version), "R.wasm")))
  expect_false(fs::file_exists(fs::path(site, "R.wasm")))
  expect_equal(read_site_config(site)[["engine-base-url"]], sprintf("./webr/v%s/", version))
})

test_that("the viewer bundle is copied under names that carry a hash of its contents", {
  built <- local_built_site()
  html <- read_index(built$site)
  js <- fs::path_file(fs::dir_ls(built$site, regexp = "exlibris-r\\.[0-9a-f]{8}\\.js$"))
  css <- fs::path_file(fs::dir_ls(built$site, regexp = "exlibris-r\\.[0-9a-f]{8}\\.css$"))
  expect_length(js, 1L)
  expect_length(css, 1L)
  expect_false(fs::file_exists(fs::path(built$site, "exlibris-r.js")))
  expect_match(html, sprintf('src="./%s"', js), fixed = TRUE)
  expect_match(html, sprintf('href="./%s"', css), fixed = TRUE)
  expect_identical(
    unname(tools::md5sum(fs::path(built$site, js))),
    unname(tools::md5sum(system.file("viewer", "exlibris-r.js", package = "webrarian")))
  )
})

test_that("a different viewer build gets a different file name", {
  src <- fs::path(withr::local_tempdir(), "viewer")
  fs::dir_copy(system.file("viewer", package = "webrarian"), src)
  cat("\n// rebuilt\n", file = fs::path(src, "exlibris-r.js"), append = TRUE)
  out_a <- withr::local_tempdir()
  out_b <- withr::local_tempdir()

  a <- copy_vendored_viewer(out_a)
  local_mocked_bindings(viewer_source_dir = function() src)
  b <- copy_vendored_viewer(out_b)

  expect_false(identical(a$js, b$js))
  expect_identical(a$css, b$css)
})

test_that("_headers keeps index.html and the user's files revalidating", {
  built <- local_built_site()
  rules <- parse_headers_file(fs::path(built$site, "_headers"))

  # These are all rewritten by every bind() at a stable URL, so a long-lived
  # entry would hide a redeploy from returning visitors.
  for (pattern in c("/", "/index.html", "/sw.js", "/vfs-files/*")) {
    expect_equal(rules[[pattern]][["Cache-Control"]], "no-cache", info = pattern)
  }

  # And nothing anywhere in the file may hand index.html a long-lived entry.
  expect_false(any(grepl("immutable", names(rules), fixed = TRUE)))
})

test_that("_headers never sets the same header on two rules", {
  # The whole reason the rules are shaped the way they are: Netlify and
  # Cloudflare Pages concatenate rather than override, so an overlap would emit
  # `Cache-Control: public, max-age=31536000, immutable, no-cache`.
  rules <- parse_headers_file(
    fs::path(local_built_site()$site, "_headers")
  )

  expect_equal(anyDuplicated(names(rules)), 0L)

  # No rule pattern is a prefix-glob of another rule that sets the same header.
  patterns <- names(rules)
  glob_matches <- function(pattern, path) {
    rx <- paste0("^", gsub("\\*", ".*", gsub("([.+?^${}()|\\[\\]\\\\])", "\\\\\\1", pattern)), "$")
    grepl(rx, path)
  }
  for (a in patterns) {
    for (b in patterns) {
      if (identical(a, b) || grepl("*", a, fixed = TRUE)) {
        next
      }
      if (glob_matches(b, a)) {
        shared <- intersect(names(rules[[a]]), names(rules[[b]]))
        expect_equal(
          shared,
          character(),
          info = sprintf("rules %s and %s both set %s", a, b, paste(shared, collapse = ", "))
        )
      }
    }
  }
})

test_that("_headers carries COOP/COEP when there is no netlify.toml", {
  built <- local_built_site()
  rules <- parse_headers_file(fs::path(built$site, "_headers"))

  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
  expect_equal(rules[["/*"]][["Cross-Origin-Embedder-Policy"]], "require-corp")
  # The site-wide rule must not also carry Cache-Control, or every other rule
  # would concatenate against it.
  expect_false("Cache-Control" %in% names(rules[["/*"]]))
})

test_that("_headers defers COOP/COEP to a hand-written netlify.toml that sets them", {
  # Netlify reads both files. A merged `same-origin, same-origin` is not a
  # valid COOP token and would silently drop crossOriginIsolated, so when a
  # toml already owns the isolation headers, _headers must not restate them.
  proj <- local_collection()
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      "  [headers.values]",
      '    Cross-Origin-Opener-Policy = "same-origin"',
      '    Cross-Origin-Embedder-Policy = "require-corp"'
    ),
    fs::path(proj, "netlify.toml")
  )

  suppressMessages(bind(proj))
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))

  expect_false("/*" %in% names(rules))
  expect_false(any(vapply(
    rules,
    function(r) any(grepl("^Cross-Origin", names(r))),
    logical(1)
  )))
  # The rules the toml does not set are all still here.
  expect_equal(rules[["/exlibris-r.*"]][["Cache-Control"]], "public, max-age=31536000, immutable")
})

test_that("the site directory alone carries COOP/COEP", {
  # One source for the isolation headers that serves a CLI deploy and a
  # drag-and-drop of the output directory alike: the site's own _headers.
  proj <- local_collection()
  suppressMessages(circulate_via_netlify(proj))
  toml <- paste(readLines(fs::path(proj, "netlify.toml")), collapse = "\n")
  expect_false(grepl("Cross-Origin", toml, fixed = TRUE))

  suppressMessages(bind(proj))
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))
  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
  expect_equal(rules[["/*"]][["Cross-Origin-Embedder-Policy"]], "require-corp")
})

test_that("cache_headers_content() explains the missing isolation block", {
  txt <- paste(cache_headers_content(include_isolation = FALSE), collapse = "\n")
  expect_match(txt, "netlify.toml", fixed = TRUE)
  expect_match(txt, "crossOriginIsolated", fixed = TRUE)
})

test_that("generated toml and _headers never set the same path", {
  proj <- local_collection()
  suppressMessages(circulate_via_netlify(proj))
  suppressMessages(bind(proj))

  toml <- fs::path(proj, "netlify.toml")
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))
  expect_length(netlify_header_blocks(toml), 0L)
  expect_length(intersect(names(netlify_header_blocks(toml)), names(rules)), 0L)
  expect_identical(netlify_sets_isolation(toml), character())
  # Nothing was left out of _headers because of the generated file.
  expect_setequal(names(rules), vapply(cache_rules(), `[[`, character(1), "path"))
})

test_that("netlify_sets_isolation() reads each header from the values of a [[headers]] table", {
  toml <- withr::local_tempfile(fileext = ".toml")
  # A header named only in a comment is not set.
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"  # Cross-Origin-Opener-Policy is left to _headers',
      "  [headers.values]",
      "    # no Cross-Origin-Opener-Policy here",
      '    Cross-Origin-Embedder-Policy = "require-corp" # Cross-Origin-Opener-Policy = "same-origin"'
    ),
    toml
  )
  expect_identical(netlify_sets_isolation(toml), "Cross-Origin-Embedder-Policy")

  # An inline table, in any case; the comment after it holds braces too.
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      '  values = { cross-origin-opener-policy = "same-origin" } # was: { Cross-Origin-Embedder-Policy = "require-corp" }'
    ),
    toml
  )
  expect_identical(netlify_sets_isolation(toml), "Cross-Origin-Opener-Policy")

  # Dotted keys, one of them quoted.
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      '  values.Cross-Origin-Opener-Policy = "same-origin"',
      '  values."Cross-Origin-Embedder-Policy" = "require-corp"'
    ),
    toml
  )
  expect_identical(netlify_sets_isolation(toml), ISOLATION_HEADERS)

  # A `=` inside a quoted value is not a key.
  writeLines(
    c(
      "[[headers]]",
      '  for = "/webr/*"',
      '  values = { Cache-Control = "public, max-age=600", "X-A" = "b=c" }'
    ),
    toml
  )
  expect_identical(netlify_header_blocks(toml), list("/webr/*" = c("cache-control", "x-a")))

  writeLines(c("[build]", '  publish = "_site"  # Cross-Origin-Opener-Policy'), toml)
  expect_identical(netlify_sets_isolation(toml), character())
  expect_identical(netlify_sets_isolation(character()), character())
  expect_identical(netlify_sets_isolation(fs::path(tempdir(), "no-such-netlify.toml")), character())
})

test_that("a hand-written netlify.toml that sets only COOP keeps COEP in _headers", {
  # All-or-nothing on COOP would drop COEP too, and the page would lose
  # crossOriginIsolated with nothing to say so.
  proj <- local_collection()
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      "  [headers.values]",
      '    Cross-Origin-Opener-Policy = "same-origin"'
    ),
    fs::path(proj, "netlify.toml")
  )

  suppressMessages(bind(proj))
  headers <- fs::path(proj, "_site", "_headers")
  rules <- parse_headers_file(headers)
  expect_equal(rules[["/*"]][["Cross-Origin-Embedder-Policy"]], "require-corp")
  expect_false("Cross-Origin-Opener-Policy" %in% names(rules[["/*"]]))
  expect_match(
    paste(readLines(headers), collapse = "\n"),
    "#   Cross-Origin-Opener-Policy",
    fixed = TRUE
  )
})

test_that("a hand-written netlify.toml that sets only COEP keeps COOP and does not repeat COEP", {
  # Both files setting COEP would send `require-corp, require-corp`, which is
  # not a valid value.
  proj <- local_collection()
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      '  values = { Cross-Origin-Embedder-Policy = "require-corp" }'
    ),
    fs::path(proj, "netlify.toml")
  )

  suppressMessages(bind(proj))
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))
  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
  expect_false("Cross-Origin-Embedder-Policy" %in% names(rules[["/*"]]))
})

test_that("the repository root's netlify.toml is read too", {
  # Netlify reads netlify.toml from the site's base directory, which is the
  # repository root unless it is set, so a collection in base/site/ is
  # deployed with base/netlify.toml.
  proj <- local_collection(name = "site")
  base <- fs::path_dir(proj)
  fs::dir_create(fs::path(base, ".git"))
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      "  [headers.values]",
      '    Cross-Origin-Opener-Policy = "same-origin"',
      "",
      "[[headers]]",
      '  for = "/webr/*"',
      "  [headers.values]",
      '    Cache-Control = "public, max-age=600"'
    ),
    fs::path(base, "netlify.toml")
  )
  expect_equal(as.character(repo_root(proj)), as.character(fs::path_real(base)))
  expect_length(netlify_tomls(proj), 1L)

  # The collection's own file is read as well.
  writeLines(c("[build]", '  publish = "_site"'), fs::path(proj, "netlify.toml"))
  expect_length(netlify_tomls(proj), 2L)

  suppressMessages(bind(proj))
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))
  expect_false("/webr/*" %in% names(rules))
  expect_false("Cross-Origin-Opener-Policy" %in% names(rules[["/*"]]))
  expect_equal(rules[["/*"]][["Cross-Origin-Embedder-Policy"]], "require-corp")
})

test_that("a mirror inside a git repository keeps COOP/COEP when the repository root's netlify.toml sets them", {
  # A mirror is deployed on its own (drag-and-drop, Cloudflare Pages, any host
  # that reads only _headers), so a netlify.toml elsewhere in its repository
  # must not take its isolation headers away.
  base <- withr::local_tempdir()
  fs::dir_create(fs::path(base, ".git"))
  writeLines(
    c(
      "[[headers]]",
      '  for = "/*"',
      "  [headers.values]",
      '    Cross-Origin-Opener-Policy = "same-origin"',
      '    Cross-Origin-Embedder-Policy = "require-corp"',
      "",
      "[[headers]]",
      '  for = "/*.wasm"',
      '  values = { Content-Type = "application/wasm" }'
    ),
    fs::path(base, "netlify.toml")
  )

  mirror <- fs::path(base, "webr-mirror")
  fs::dir_create(mirror)
  expect_length(netlify_tomls(mirror), 0L)

  suppressMessages(emit_cache_headers(mirror, root = mirror)) # collection_mirror()'s call
  rules <- parse_headers_file(fs::path(mirror, "_headers"))
  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
  expect_equal(rules[["/*"]][["Cross-Origin-Embedder-Policy"]], "require-corp")
  expect_true("/*.wasm" %in% names(rules))
})

test_that("repo_root() finds the nearest .git, a directory or a worktree's file", {
  base <- withr::local_tempdir()
  fs::dir_create(fs::path(base, "a", "b"))
  fs::dir_create(fs::path(base, ".git"))
  expect_equal(as.character(repo_root(fs::path(base, "a", "b"))), as.character(fs::path_real(base)))
  writeLines("gitdir: ../.git/worktrees/a", fs::path(base, "a", ".git"))
  expect_equal(
    as.character(repo_root(fs::path(base, "a", "b"))),
    as.character(fs::path_real(fs::path(base, "a")))
  )
  # A collection at its repository's root has one netlify.toml to read.
  writeLines(c("[build]", '  publish = "_site"'), fs::path(base, "a", "netlify.toml"))
  expect_length(netlify_tomls(fs::path(base, "a")), 1L)
})

test_that("a rule a hand-written netlify.toml duplicates is left out of _headers", {
  proj <- local_collection()
  writeLines(
    c(
      "[build]",
      '  publish = "_site"',
      "",
      "[[headers]]",
      '  for = "/webr/*"',
      "  [headers.values]",
      '    Cache-Control = "public, max-age=600"',
      "",
      "[[headers]]",
      '  for = "/exlibris-r.*"',
      '  values = { X-Robots-Tag = "noindex" }'
    ),
    fs::path(proj, "netlify.toml")
  )
  toml <- fs::path(proj, "netlify.toml")
  expect_setequal(names(netlify_header_blocks(toml)), c("/webr/*", "/exlibris-r.*"))
  expect_equal(netlify_duplicated_paths(toml), "/webr/*")

  suppressMessages(bind(proj))
  headers <- fs::path(proj, "_site", "_headers")
  rules <- parse_headers_file(headers)
  # Both files giving /webr/* a Cache-Control would send the two joined.
  expect_false("/webr/*" %in% names(rules))
  # A different header for the same path does not clash.
  expect_equal(rules[["/exlibris-r.*"]][["Cache-Control"]], "public, max-age=31536000, immutable")
  # It sets no isolation header, so _headers keeps them.
  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
  expect_match(paste(readLines(headers), collapse = "\n"), "#   /webr/*", fixed = TRUE)
})

test_that("_headers keeps COOP/COEP when a hand-written netlify.toml does not set them", {
  proj <- local_collection()
  writeLines(c("[build]", '  publish = "_site"'), fs::path(proj, "netlify.toml"))
  suppressMessages(bind(proj))
  rules <- parse_headers_file(fs::path(proj, "_site", "_headers"))
  expect_equal(rules[["/*"]][["Cross-Origin-Opener-Policy"]], "same-origin")
})

# ---------------------------------------------------------------------------
# Service worker: off by default
# ---------------------------------------------------------------------------

test_that("service_worker defaults to FALSE", {
  proj <- withr::local_tempdir()
  suppressMessages(catalog(proj))
  config <- collection_settings(proj)
  expect_false(isTRUE(settings_get(config, "build.service-worker")))
})

test_that("bind() registers no service worker by default and ships the retire worker", {
  built <- local_built_site()

  html <- read_index(built$site)
  expect_false(grepl("serviceWorker", html, fixed = TRUE))
  expect_false(grepl("sw.js", html, fixed = TRUE))

  # Visitors whose browser still runs a worker from an earlier build fetch
  # this on their next update check; it clears that worker's caches and
  # unregisters it.
  js <- paste(readLines(fs::path(built$site, "sw.js"), warn = FALSE), collapse = "\n")
  expect_match(js, "self.registration.unregister()", fixed = TRUE)
  expect_match(js, "name.startsWith(CACHE_PREFIX)", fixed = TRUE)
  expect_false(grepl("respondWith", js, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Service worker: opted in
# ---------------------------------------------------------------------------

test_that("build: service-worker: true emits sw.js and registers it", {
  built <- local_built_site("build/service_worker" = TRUE)

  sw <- fs::path(built$site, "sw.js")
  expect_true(fs::file_exists(sw))

  html <- read_index(built$site)
  expect_match(html, 'navigator.serviceWorker.register("sw.js")', fixed = TRUE)
  # Registration must land before the module script that boots the viewer.
  expect_lt(
    regexpr("serviceWorker.register", html, fixed = TRUE),
    regexpr('<script type="module"', html, fixed = TRUE)
  )
  # Relative, so a project site at /repo/ registers a worker scoped to /repo/
  # rather than to the whole user site.
  expect_false(grepl('register("/sw.js")', html, fixed = TRUE))
})

test_that("the emitted worker has its build id substituted", {
  built <- local_built_site("build/service_worker" = TRUE)
  js <- paste(readLines(fs::path(built$site, "sw.js"), warn = FALSE), collapse = "\n")

  expect_false(grepl("{{", js, fixed = TRUE))
  id <- sub('^.*const BUILD_ID = "([^"]+)".*$', "\\1", js)
  expect_match(id, "^[0-9]{14}-[0-9a-f]{8}$")
})

test_that("a second bind() changes the cache name", {
  proj <- withr::local_tempdir()
  suppressMessages(catalog(proj))
  suppressMessages(settings_set(proj, "build.bundle-engine" = FALSE))
  settings_set(proj, "build/service_worker" = TRUE)

  build_id_of <- function() {
    suppressMessages(bind(proj))
    js <- readLines(fs::path(proj, "_site", "sw.js"), warn = FALSE)
    line <- grep("^const BUILD_ID", js, value = TRUE)
    expect_length(line, 1)
    sub('^const BUILD_ID = "([^"]+)".*$', "\\1", line)
  }

  first <- build_id_of()
  second <- build_id_of()
  expect_false(identical(first, second))
})

test_that("new_build_id() is unique per call", {
  ids <- vapply(1:20, function(i) new_build_id(), character(1))
  expect_equal(anyDuplicated(ids), 0L)
})

test_that("the worker never synthesizes a Response", {
  # The single biggest hazard: a Response the worker builds itself carries no
  # COOP/COEP, so replaying one for the document would silently disable
  # SharedArrayBuffer. Every reply must be the network response or a verbatim
  # Cache Storage replay.
  js <- paste(
    readLines(
      local_built_site("build/service_worker" = TRUE)$site |>
        fs::path("sw.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  code <- gsub("/\\*.*?\\*/", "", js) # strip block comments
  code <- gsub("(?m)//.*$", "", code, perl = TRUE) # strip line comments

  expect_false(grepl("new Response", code, fixed = TRUE))
  expect_false(grepl("Response.redirect", code, fixed = TRUE))
  # Cached copies are keyed to this build's cache only.
  expect_match(code, "cacheName: CACHE_NAME", fixed = TRUE)
})

test_that("the worker scopes cache eviction to its own registration", {
  js <- paste(
    readLines(
      local_built_site("build/service_worker" = TRUE)$site |>
        fs::path("sw.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # activate() may only delete caches belonging to this deployment, never a
  # sibling webrarian site sharing the origin.
  expect_match(js, "self.registration.scope", fixed = TRUE)
  expect_match(js, "name.startsWith(CACHE_PREFIX) && name !== CACHE_NAME", fixed = TRUE)
})

test_that("the worker leaves user files and the config page on the network", {
  js <- paste(
    readLines(
      local_built_site("build/service_worker" = TRUE)$site |>
        fs::path("sw.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # The body of `const <name> = [ ... \n];` - up to the array's own closing
  # line, not the first "]" (the patterns contain bracket expressions).
  list_body <- function(name) {
    sub(sprintf("(?s)^.*const %s = \\[(.*?)\\n\\];.*$", name), "\\1", js, perl = TRUE)
  }
  network_first <- list_body("NETWORK_FIRST")
  expect_match(network_first, "index\\.html", fixed = TRUE)
  expect_match(network_first, "vfs-files", fixed = TRUE)
  expect_match(network_first, "repo", fixed = TRUE)

  cache_first <- list_body("CACHE_FIRST")
  expect_false(grepl("vfs-files", cache_first, fixed = TRUE))
  expect_false(grepl("repo", cache_first, fixed = TRUE))
  expect_match(cache_first, "webr\\/v", fixed = TRUE)
  expect_match(cache_first, "exlibris-r", fixed = TRUE)
})

test_that("emit_service_worker() refuses to ship an unsubstituted template", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(
    template_path = function(name) {
      f <- fs::path(dir, "broken.js")
      writeLines('const BUILD_ID = "{{build_id}}"; const X = "{{oops}}";', f)
      f
    }
  )
  expect_error(
    suppressMessages(emit_service_worker(dir, "abc")),
    "Unsubstituted placeholder"
  )
})

test_that("a cache-first miss revalidates instead of copying the HTTP cache", {
  js <- paste(
    readLines(
      fs::path(local_built_site("build/service_worker" = TRUE)$site, "sw.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(js, 'fetch(request, { cache: "no-cache" })', fixed = TRUE)
  expect_false(grepl("offline reload", js, fixed = TRUE))
})

test_that("both worker templates are valid JavaScript", {
  skip_if(!nzchar(Sys.which("node")), "node not available")
  for (name in c("sw.js", "sw-retire.js")) {
    status <- system2(
      "node",
      c("--check", shQuote(template_path(name))),
      stdout = FALSE,
      stderr = FALSE
    )
    expect_equal(status, 0L, info = name)
  }
})

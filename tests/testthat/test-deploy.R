# Tests for R/deploy.R — pure/file-based deployment config generation.
# No network, no Docker. All filesystem work is confined to temp dirs.

# Helper: create a minimal collection in the current (temp) working dir.
make_collection <- function() {
  suppressMessages(
    catalog(".", template = "minimal", detect = FALSE)
  )
}

test_that("circulate_via_github writes a workflow file referencing bind()", {
  withr::with_tempdir({
    make_collection()

    path <- suppressMessages(circulate_via_github())

    expect_true(fs::file_exists(path))
    expect_match(fs::path_file(path), "deploy-webr\\.yml$")

    content <- paste(readLines(path), collapse = "\n")

    # References the real exported build entry point, not a dead template name.
    expect_match(content, "webrarian::bind\\(\\)")
    expect_false(grepl("webrarian_build", content))

    # Mentions the pages/output _site deployment surface.
    expect_match(content, "_site")
    expect_match(content, "pages", ignore.case = TRUE)
  })
})

test_that("circulate_via_github writes into .github/workflows", {
  withr::with_tempdir({
    make_collection()
    path <- suppressMessages(circulate_via_github())
    expect_true(grepl("\\.github/workflows/deploy-webr\\.yml$", as.character(path)))
  })
})

test_that("second circulate_via_github without overwrite errors", {
  withr::with_tempdir({
    make_collection()
    suppressMessages(circulate_via_github())

    expect_error(
      suppressMessages(circulate_via_github()),
      "already exists"
    )
  })
})

test_that("circulate_via_github overwrite = TRUE replaces the file", {
  withr::with_tempdir({
    make_collection()
    path <- suppressMessages(circulate_via_github())
    # Corrupt it, then overwrite.
    writeLines("stale", as.character(path))
    path2 <- suppressMessages(circulate_via_github(overwrite = TRUE))
    expect_equal(as.character(path), as.character(path2))
    content <- paste(readLines(as.character(path2)), collapse = "\n")
    expect_match(content, "webrarian::bind\\(\\)")
  })
})

test_that("circulate_via_netlify writes a netlify.toml that only names the publish directory, and a deploying workflow", {
  withr::with_tempdir({
    make_collection()
    written <- suppressMessages(circulate_via_netlify())
    expect_setequal(fs::path_file(written), c("netlify.toml", "netlify-deploy.yml"))

    toml <- readLines("netlify.toml")
    # Every response header lives in the site's _headers (see the
    # cache-runtime tests); the toml only says what to publish.
    expect_identical(toml[!grepl("^\\s*(#|$)", toml)], c("[build]", '  publish = "_site"'))

    wf <- paste(readLines(".github/workflows/netlify-deploy.yml"), collapse = "\n")
    expect_match(wf, "webrarian::bind()", fixed = TRUE)
    expect_match(wf, 'netlify-cli@27 deploy --prod --dir="_site" --no-build', fixed = TRUE)
    expect_match(wf, "secrets.NETLIFY_AUTH_TOKEN", fixed = TRUE)
    expect_match(wf, "secrets.NETLIFY_SITE_ID", fixed = TRUE)
    expect_match(wf, sprintf('pak::pak("%s")', webrarian_install_ref()), fixed = TRUE)
  })
})

test_that("the netlify.toml publishes the configured output directory", {
  withr::with_tempdir({
    make_collection()
    suppressMessages(settings_set(".", "build.output-dir" = "public"))
    suppressMessages(circulate_via_netlify())
    expect_match(
      paste(readLines("netlify.toml"), collapse = "\n"),
      'publish = "public"',
      fixed = TRUE
    )
  })
})

test_that("circulate_via_netlify refuses an output directory it cannot write safely", {
  # The name goes into a TOML string and a shell command.
  withr::with_tempdir({
    make_collection()
    suppressMessages(settings_set(".", "build.output-dir" = "out $x"))
    expect_error(suppressMessages(circulate_via_netlify()), "build.output-dir")
    expect_false(fs::file_exists("netlify.toml"))
    expect_false(fs::file_exists(".github/workflows/netlify-deploy.yml"))
  })
})

test_that("circulate_via_netlify refuses to overwrite without overwrite = TRUE", {
  withr::with_tempdir({
    make_collection()
    suppressMessages(circulate_via_netlify())
    writeLines("# mine", "netlify.toml")
    expect_error(suppressMessages(circulate_via_netlify()), "already exist")
    expect_identical(readLines("netlify.toml"), "# mine")

    suppressMessages(circulate_via_netlify(overwrite = TRUE))
    expect_match(
      paste(readLines("netlify.toml"), collapse = "\n"),
      'publish = "_site"',
      fixed = TRUE
    )
  })
})

test_that("generate_netlify_config writes the workflow even without a .github directory", {
  withr::with_tempdir({
    make_collection()
    gen <- suppressMessages(generate_netlify_config(collection_root("."), overwrite = FALSE))
    expect_true(any(grepl("netlify-deploy\\.yml$", as.character(gen))))
    expect_true(any(grepl("netlify\\.toml$", as.character(gen))))
  })
})

test_that("deploy functions require a collection", {
  withr::with_tempdir({
    # No _webrarian.yml present here.
    expect_error(suppressMessages(circulate_via_github()), "collection")
    expect_error(suppressMessages(circulate_via_netlify()), "collection")
  })
})


test_that("generate_github_actions_workflow renders every placeholder", {
  withr::with_tempdir({
    make_collection()
    content <- generate_github_actions_workflow(".")
    expect_match(content, "webrarian::bind\\(\\)")
    expect_false(grepl("\\{\\{[a-z_]+\\}\\}", content))
  })
})

test_that("every webrarian:: name in generated configs is an actual export", {
  withr::with_tempdir({
    make_collection()
    root <- collection_root(".")

    suppressMessages(circulate_via_github())
    suppressMessages(circulate_via_netlify())
    # Force the netlify workflow branch too.
    suppressMessages(generate_netlify_config(root, overwrite = TRUE))

    files <- fs::dir_ls(root, recurse = TRUE, type = "file")
    all_text <- unlist(lapply(files, function(f) {
      tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    }))
    all_text <- paste(all_text, collapse = "\n")

    # Extract every "webrarian::<fn>" reference and confirm it is exported.
    m <- gregexpr("webrarian::([A-Za-z0-9_.]+)", all_text)
    refs <- regmatches(all_text, m)[[1]]
    fns <- unique(sub("^webrarian::", "", refs))
    fns <- sub("\\(.*$", "", fns)

    exports <- getNamespaceExports("webrarian")
    expect_true(length(fns) > 0)
    for (fn in fns) {
      expect_true(
        fn %in% exports,
        info = sprintf("Generated config references non-exported webrarian::%s", fn)
      )
    }
  })
})

test_that("the workflow installs the webrarian version that generated it", {
  expect_equal(webrarian_install_ref("0.1.0"), "coatless-wasm/webrarian@v0.1.0")
  expect_equal(webrarian_install_ref("0.2.1"), "coatless-wasm/webrarian@v0.2.1")
  expect_equal(webrarian_install_ref("0.1.0.9000"), "coatless-wasm/webrarian")
  # Nothing below 0.1.0 was ever tagged.
  expect_equal(webrarian_install_ref("0.0.1"), "coatless-wasm/webrarian")
  withr::with_tempdir({
    make_collection()
    content <- paste(readLines(suppressMessages(circulate_via_github())), collapse = "\n")
    expect_match(content, sprintf('pak::pak("%s")', webrarian_install_ref()), fixed = TRUE)
    expect_false(grepl('"zip"', content, fixed = TRUE))
  })
})

test_that("only the deploy job can write to Pages, and pull requests never deploy", {
  withr::with_tempdir({
    make_collection()
    wf <- yaml::read_yaml(suppressMessages(circulate_via_github()))
    expect_equal(wf$permissions, list(contents = "read"))
    expect_null(wf$jobs$build$permissions)
    expect_equal(wf$jobs$deploy$permissions, list(pages = "write", `id-token` = "write"))
    expect_match(wf$jobs$deploy[["if"]], "pull_request", fixed = TRUE)
  })
})

test_that("the workflow uploads the configured output directory and caches downloads", {
  withr::with_tempdir({
    make_collection()
    suppressMessages(settings_set(".", "build.output-dir" = "public"))
    content <- paste(readLines(suppressMessages(circulate_via_github())), collapse = "\n")
    expect_match(content, 'path: "public"', fixed = TRUE)
    expect_false(grepl("_site", content, fixed = TRUE))
    expect_match(content, "R_USER_CACHE_DIR", fixed = TRUE)
    expect_match(content, "actions/cache@v6", fixed = TRUE)
  })
})

test_that("an output directory the workflow cannot name safely is refused", {
  # `path: "{{output_dir}}"` is a double-quoted YAML scalar: a `"` ends it,
  # and `$`, a backtick or ` #` would mean something else in the Netlify
  # step's shell command.
  withr::with_tempdir({
    make_collection()
    suppressMessages(settings_set(".", "build.output-dir" = "out $x"))
    expect_error(suppressMessages(circulate_via_github()), "build.output-dir")
    expect_false(fs::file_exists(".github/workflows/deploy-webr.yml"))
    expect_error(generate_github_actions_workflow("."), "build.output-dir")
  })
})

test_that("the workflow sets R_USER_CACHE_DIR on the build step, where GitHub allows runner", {
  withr::with_tempdir({
    make_collection()
    path <- suppressMessages(circulate_via_github())
    wf <- yaml::read_yaml(path)
    # A job's env cannot use the runner context: GitHub refuses the whole file.
    # [[ ]], not $, which would partially match a job's `environment`.
    expect_null(wf$jobs$build[["env"]])
    build <- Filter(function(s) identical(s$name, "Build the site"), wf$jobs$build$steps)
    expect_length(build, 1L)
    expect_identical(build[[1]][["env"]][["R_USER_CACHE_DIR"]], "${{ runner.temp }}/r-user-cache")
    skip_on_cran()
    skip_if(!nzchar(Sys.which("actionlint")), "actionlint is not installed")
    out <- suppressWarnings(system2(
      "actionlint",
      c("-shellcheck=", "-pyflakes=", shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    ))
    expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  })
})

test_that("the netlify workflow sets R_USER_CACHE_DIR on the build step, where GitHub allows runner", {
  withr::with_tempdir({
    make_collection()
    written <- suppressMessages(circulate_via_netlify())
    path <- as.character(written)[grepl("netlify-deploy\\.yml$", as.character(written))]
    wf <- yaml::read_yaml(path)
    # A job's env cannot use the runner context: GitHub refuses the whole file.
    # [[ ]], not $, which would partially match a job's `environment`.
    expect_null(wf$jobs$deploy[["env"]])
    build <- Filter(function(s) identical(s$name, "Build the site"), wf$jobs$deploy$steps)
    expect_length(build, 1L)
    expect_identical(build[[1]][["env"]][["R_USER_CACHE_DIR"]], "${{ runner.temp }}/r-user-cache")
    skip_on_cran()
    skip_if(!nzchar(Sys.which("actionlint")), "actionlint is not installed")
    out <- suppressWarnings(system2(
      "actionlint",
      c("-shellcheck=", "-pyflakes=", shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    ))
    expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
  })
})

# A collection in a subdirectory of a git repository, a layout
# netlify_tomls() reads too. GitHub runs only the workflows at the
# repository root; a workflow's run steps can start in the collection, but
# hashFiles() and an action's `with:` paths always start at the root.
local_repo_collection <- function(env = parent.frame()) {
  repo <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(fs::path(repo, ".git"))
  site <- fs::path(repo, "sites", "course")
  fs::dir_create(site)
  suppressMessages(catalog(site, template = "minimal", detect = FALSE))
  suppressMessages(settings_set(site, "build.output-dir" = "public"))
  list(repo = fs::path_real(repo), site = site)
}

workflow_step <- function(job, name) {
  step <- Filter(function(s) identical(s$name, name), job$steps)
  testthat::expect_length(step, 1L)
  step[[1]]
}

expect_actionlint_clean <- function(path) {
  out <- suppressWarnings(system2(
    "actionlint",
    c("-shellcheck=", "-pyflakes=", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  ))
  testthat::expect_null(attr(out, "status"), info = paste(out, collapse = "\n"))
}

test_that("a collection in a repository subdirectory gets its Pages workflow at the repository root", {
  x <- local_repo_collection()
  msgs <- testthat::capture_messages(path <- circulate_via_github(x$site))
  expect_equal(fs::path_real(path), fs::path(x$repo, ".github", "workflows", "deploy-webr.yml"))
  expect_false(fs::dir_exists(fs::path(x$site, ".github")))
  expect_false(any(grepl("not in a git repository", msgs, fixed = TRUE)))

  build <- yaml::read_yaml(path)$jobs$build
  expect_identical(build$defaults$run[["working-directory"]], "sites/course")
  expect_identical(
    workflow_step(build, "Cache webR downloads")$with$key,
    "webrarian-${{ runner.os }}-${{ hashFiles('sites/course/_webrarian.yml') }}"
  )
  expect_identical(workflow_step(build, "Upload Pages artifact")$with$path, "sites/course/public")
  expect_identical(workflow_step(build, "Build the site")$run, "webrarian::bind()")

  skip_on_cran()
  skip_if(!nzchar(Sys.which("actionlint")), "actionlint is not installed")
  expect_actionlint_clean(path)
})

test_that("a collection in a repository subdirectory gets its Netlify workflow at the repository root", {
  x <- local_repo_collection()
  written <- suppressMessages(circulate_via_netlify(x$site))
  workflow <- fs::path(x$repo, ".github", "workflows", "netlify-deploy.yml")
  # netlify.toml stays next to _webrarian.yml.
  expect_setequal(
    as.character(fs::path_real(written)),
    as.character(c(fs::path_real(fs::path(x$site, "netlify.toml")), workflow))
  )
  expect_false(fs::dir_exists(fs::path(x$site, ".github")))

  job <- yaml::read_yaml(workflow)$jobs$deploy
  expect_identical(job$defaults$run[["working-directory"]], "sites/course")
  expect_identical(
    workflow_step(job, "Cache webR downloads")$with$key,
    "webrarian-${{ runner.os }}-${{ hashFiles('sites/course/_webrarian.yml') }}"
  )
  # A run step starts in the collection, so --dir stays relative to it.
  expect_match(
    workflow_step(job, "Deploy to Netlify")$run,
    '--dir="public" --no-build',
    fixed = TRUE
  )

  # The existing-file check looks where the workflow is written.
  file.remove(fs::path(x$site, "netlify.toml"))
  expect_error(suppressMessages(circulate_via_netlify(x$site)), "netlify-deploy.yml")

  skip_on_cran()
  skip_if(!nzchar(Sys.which("actionlint")), "actionlint is not installed")
  expect_actionlint_clean(workflow)
})

test_that("a collection at its repository root runs its workflow steps there", {
  withr::with_tempdir({
    make_collection()
    fs::dir_create(".git")
    path <- suppressMessages(circulate_via_github())
    expect_equal(fs::path_real(path), fs::path_real(".github/workflows/deploy-webr.yml"))
    build <- yaml::read_yaml(path)$jobs$build
    expect_identical(build$defaults$run[["working-directory"]], ".")
    expect_match(
      workflow_step(build, "Cache webR downloads")$with$key,
      "hashFiles('_webrarian.yml')",
      fixed = TRUE
    )
    expect_identical(workflow_step(build, "Upload Pages artifact")$with$path, "_site")
  })
})

test_that("outside a git repository the workflow is written in the collection, with a note to move it", {
  local_mocked_bindings(repo_root = function(path) NULL)
  withr::with_tempdir({
    make_collection()
    msgs <- testthat::capture_messages(circulate_via_github())
    expect_true(fs::file_exists(".github/workflows/deploy-webr.yml"))
    expect_match(paste(msgs, collapse = " "), "not in a git repository", fixed = TRUE)
    msgs <- testthat::capture_messages(circulate_via_netlify())
    expect_true(fs::file_exists(".github/workflows/netlify-deploy.yml"))
    expect_match(paste(msgs, collapse = " "), "not in a git repository", fixed = TRUE)
  })
})

test_that("a collection directory the workflow cannot name safely is refused before anything is written", {
  repo <- withr::local_tempdir()
  fs::dir_create(fs::path(repo, ".git"))
  site <- fs::path(repo, "my site")
  fs::dir_create(site)
  suppressMessages(catalog(site, template = "minimal", detect = FALSE))
  expect_error(suppressMessages(circulate_via_github(site)), "my site")
  expect_error(suppressMessages(circulate_via_netlify(site)), "my site")
  expect_false(fs::dir_exists(fs::path(repo, ".github")))
  expect_false(fs::file_exists(fs::path(site, "netlify.toml")))
})

test_that("circulate_via_github() takes the shared arguments and returns every path it wrote", {
  expect_identical(formals_text(circulate_via_github), c(path = "\".\"", overwrite = "FALSE"))
  withr::with_tempdir({
    make_collection()
    written <- suppressMessages(circulate_via_github())
    expect_type(written, "character")
    expect_length(written, 1L)
    expect_true(all(fs::file_exists(written)))
    expect_match(written, "\\.github/workflows/deploy-webr\\.yml$")
    expect_error(suppressMessages(circulate_via_github()), "already exists")
    expect_identical(suppressMessages(circulate_via_github(overwrite = TRUE)), written)
  })
})

test_that("circulate_via_netlify() returns every path it wrote too", {
  withr::with_tempdir({
    make_collection()
    written <- suppressMessages(circulate_via_netlify())
    expect_type(written, "character")
    expect_true(all(fs::file_exists(written)))
    expect_setequal(as.character(fs::path_file(written)), c("netlify.toml", "netlify-deploy.yml"))
    expect_true(any(grepl("\\.github/workflows/netlify-deploy\\.yml$", written)))
  })
})

test_that("circulation_config() is gone", {
  expect_false(exists("circulation_config", envir = asNamespace("webrarian"), inherits = FALSE))
  expect_false(exists(
    "generate_github_pages_config",
    envir = asNamespace("webrarian"),
    inherits = FALSE
  ))
})

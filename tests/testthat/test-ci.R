# CI runs the suites CRAN skips. Source tree only: .github/ and
# tools/ are not in the built package.

workflow <- function(name) {
  dir <- test_path("..", "..", ".github", "workflows")
  skip_if_not(dir.exists(dir), ".github/ is not in this tree")
  path <- file.path(dir, name)
  expect_true(file.exists(path), info = name)
  yaml::read_yaml(path)
}

job_runs <- function(job) paste(unlist(lapply(job$steps, `[[`, "run")), collapse = "\n")

test_that("the integration workflow runs the NOT_CRAN, browser and real-Docker suites", {
  wf <- workflow("integration.yml")
  expect_setequal(names(wf$jobs), c("tests-not-cran", "browser", "docker"))
  for (name in names(wf$jobs)) {
    job <- wf$jobs[[name]]
    expect_true(is.numeric(job[["timeout-minutes"]]), info = name)
    expect_equal(job$env$NOT_CRAN, "true", info = name)
    expect_match(job_runs(job), "R_USER_CACHE_DIR", fixed = TRUE, info = name)
  }
  expect_match(
    job_runs(wf$jobs[["tests-not-cran"]]),
    "tools/ci-test.R --exclude 'browser|docker-real'",
    fixed = TRUE
  )
  expect_match(
    job_runs(wf$jobs$browser),
    "tools/ci-test.R --filter 'browser|examples' --no-skips",
    fixed = TRUE
  )
  expect_match(
    job_runs(wf$jobs$docker),
    "tools/ci-test.R --filter docker-real --no-skips",
    fixed = TRUE
  )
  expect_equal(wf$jobs$docker$env$WEBRARIAN_TEST_DOCKER, "true")
})

test_that("the old engine matrix, which tested an engine this webrarian no longer offers, is gone", {
  wf <- workflow("viewer.yml")
  expect_null(wf$jobs[["engine-matrix"]])
})

test_that("no workflow still points at the engine-matrix job", {
  dir <- test_path("..", "..", ".github", "workflows")
  skip_if_not(dir.exists(dir), ".github/ is not in this tree")
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    expect_false(
      any(grepl("engine-matrix", readLines(f, warn = FALSE), fixed = TRUE)),
      info = basename(f)
    )
  }
})

test_that("tools/ci-test.R exists", {
  skip_if_not(dir.exists(test_path("..", "..", "tools")), "tools/ is not in this tree")
  expect_true(file.exists(test_path("..", "..", "tools", "ci-test.R")))
})

# --- No container image nothing uses -------------------------
#
# The devcontainer prebuild failed on every run and published an image that
# .devcontainer/devcontainer.json never pulled. It is gone, with its build
# configuration; the dev container uses a maintained image.

workflow_files <- function() {
  dir <- test_path("..", "..", ".github", "workflows")
  skip_if_not(dir.exists(dir), ".github/ is not in this tree")
  sort(list.files(dir, pattern = "\\.ya?ml$"))
}

test_that("no workflow builds or publishes a container image", {
  for (name in workflow_files()) {
    text <- readLines(test_path("..", "..", ".github", "workflows", name), warn = FALSE)
    expect_false(any(grepl("devcontainers/ci", text, fixed = TRUE)), info = name)
    expect_false(any(grepl("packages: write", text, fixed = TRUE)), info = name)
  }
  expect_false(dir.exists(test_path("..", "..", ".github", ".devcontainer")))
})

test_that("the dev container names only an image, not the removed prebuild", {
  path <- test_path("..", "..", ".devcontainer", "devcontainer.json")
  skip_if_not(file.exists(path), ".devcontainer/ is not in this tree")
  text <- readLines(path, warn = FALSE)
  expect_false(any(grepl(".github", text, fixed = TRUE)))
  expect_true(any(grepl(
    '"image": "ghcr.io/coatless-devcontainer/r-pkg:latest"',
    text,
    fixed = TRUE
  )))
})

# --- Checks that need the published site ----------------
#
# The URL check and CRAN's incoming checks can only pass once the repositories
# are public and the pkgdown site is deployed, so they run after each
# successful pkgdown run on main instead of racing it on every push. A
# workflow_run also follows pull-request runs, a fork's included (whose head
# branch is often also called main), and runs in this repository's context:
# only a push or manual run of pkgdown in this repository may start them.

# yaml::read_yaml() reads the key `on` as "TRUE" (YAML 1.1).
workflow_triggers <- function(wf) wf[["on"]] %||% wf[["TRUE"]]

test_that("the checks that need the published site wait for a successful pkgdown run on main", {
  expect_identical(workflow("pkgdown.yml")$name, "pkgdown")
  wf <- workflow("public-checks.yml")
  on <- workflow_triggers(wf)
  expect_identical(unlist(on$workflow_run$workflows), "pkgdown")
  expect_identical(unlist(on$workflow_run$types), "completed")
  expect_identical(unlist(on$workflow_run$branches), "main")
  expect_false(any(c("push", "pull_request", "pull_request_target") %in% names(on)))
  expect_setequal(names(wf$jobs), c("url-resolves", "cran-incoming"))
  for (id in names(wf$jobs)) {
    condition <- wf$jobs[[id]][["if"]]
    expect_match(
      condition,
      "github.event.workflow_run.conclusion == 'success'",
      fixed = TRUE,
      info = id
    )
    expect_match(
      condition,
      "github.event.workflow_run.event != 'pull_request'",
      fixed = TRUE,
      info = id
    )
    expect_match(
      condition,
      "github.event.workflow_run.head_repository.full_name == github.repository",
      fixed = TRUE,
      info = id
    )
  }
  # A skipped run (a fork's pull request from its main) or the weekly cron shares
  # github.ref with a real deploy's run: it must neither share its group nor cancel it.
  expect_match(wf$concurrency$group, "github.event.workflow_run.head_sha", fixed = TRUE)
  expect_false(isTRUE(wf$concurrency[["cancel-in-progress"]]))
  expect_match(
    job_runs(wf$jobs[["url-resolves"]]),
    "https://coatless-wasm.github.io/webrarian/",
    fixed = TRUE
  )
  expect_null(workflow("viewer.yml")$jobs[["url-resolves"]])
})

test_that("cran-incoming runs CRAN's incoming checks against the live URLs and fails on an invalid one", {
  job <- workflow("public-checks.yml")$jobs[["cran-incoming"]]
  check <- Filter(
    function(s) startsWith(s$uses %||% "", "r-lib/actions/check-r-package@"),
    job$steps
  )
  expect_length(check, 1L)
  expect_identical(check[[1]]$env[["_R_CHECK_CRAN_INCOMING_"]], "true")
  expect_identical(check[[1]]$env[["_R_CHECK_CRAN_INCOMING_REMOTE_"]], "true")
  expect_identical(check[[1]]$env$NOT_CRAN, "false")
  expect_identical(job$env$QUARTO_CHROMIUM, "/usr/bin/false")
  expect_match(job_runs(job), "Rscript tools/cran-incoming.R check", fixed = TRUE)
})

# exlibris and pyodidarian stay private for now, so their repositories (and
# pyodidarian's PyPI page) answer 404 to CRAN's URL check. tools/cran-incoming.R
# reads the check log and fails on any other invalid URL, and on any invalid
# DOI or file URI.

incoming <- function(lines) {
  skip_if_not(file.exists(repo_path("tools", "cran-incoming.R")), "tools/ is not in this tree")
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "check", "webrarian.Rcheck"), recursive = TRUE)
  writeLines(
    c(
      "* using log directory 'check/webrarian.Rcheck'",
      "* checking for file 'webrarian/DESCRIPTION' ... OK",
      "* checking CRAN incoming feasibility ... NOTE",
      "Maintainer: 'James Joseph Balamuta <james.balamuta@gmail.com>'",
      "",
      "New submission",
      lines,
      "* checking package namespace information ... OK"
    ),
    file.path(dir, "check", "webrarian.Rcheck", "00check.log")
  )
  script <- normalizePath(repo_path("tools", "cran-incoming.R"))
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(
    rscript,
    c(shQuote(script), shQuote(file.path(dir, "check"))),
    stdout = TRUE,
    stderr = TRUE
  ))
  list(status = attr(out, "status") %||% 0L, out = out)
}

private_urls <- c(
  "Found the following (possibly) invalid URLs:",
  "  URL: https://github.com/coatless-wasm/exlibris",
  "    From: README.md",
  "    Status: 404",
  "    Message: Not Found",
  "  URL: https://github.com/coatless-wasm/pyodidarian",
  "    From: inst/doc/ecosystem.html",
  "    Status: 404",
  "    Message: Not Found",
  "  URL: https://pypi.org/project/pyodidarian/",
  "    From: inst/doc/ecosystem.html",
  "    Status: 404",
  "    Message: Not Found"
)

test_that("a new submission alone passes", {
  res <- incoming(character())
  expect_identical(res$status, 0L)
  expect_true(any(grepl("New submission", res$out, fixed = TRUE)))
})

test_that("the private exlibris and pyodidarian URLs are reported but pass", {
  res <- incoming(private_urls)
  expect_identical(res$status, 0L)
  expect_true(any(grepl("still private", res$out, fixed = TRUE)))
})

test_that("any other invalid URL fails, next to the private ones", {
  res <- incoming(c(
    private_urls,
    "  URL: https://github.com/coatless-wasm/webrarian/issues",
    "    From: DESCRIPTION",
    "    Status: 404",
    "    Message: Not Found",
    "  URL: https://github.com/coatless-wasm/exlibris-fork",
    "    From: README.md",
    "    Status: 404",
    "    Message: Not Found"
  ))
  expect_identical(res$status, 1L)
  expect_true(any(grepl(
    "https://github.com/coatless-wasm/webrarian/issues",
    res$out,
    fixed = TRUE
  )))
  expect_true(any(grepl("::error::", res$out, fixed = TRUE)))
})

test_that("an invalid DOI or file URI fails", {
  doi <- incoming(c(
    "Found the following (possibly) invalid DOIs:",
    "  DOI: 10.0/nope",
    "    From: DESCRIPTION"
  ))
  expect_identical(doi$status, 1L)
  uri <- incoming(c(
    "Found the following (possibly) invalid file URIs:",
    "  URI: nope.html",
    "    From: README.md"
  ))
  expect_identical(uri$status, 1L)
})

test_that("a missing check log fails rather than passing unchecked", {
  skip_if_not(file.exists(repo_path("tools", "cran-incoming.R")), "tools/ is not in this tree")
  dir <- withr::local_tempdir()
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(
    rscript,
    c(shQuote(normalizePath(repo_path("tools", "cran-incoming.R"))), shQuote(dir)),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(attr(out, "status"), 1L)
})

# --- CI gates --------------------------------------------------
#
# Every job stops after a bounded time and a newer push cancels an older run
# (five jobs once ran for six hours each); nothing that must gate the run is
# continue-on-error; no workflow names a branch the public repository will not
# have; the rebuild of the vendored bundle uses exlibris's own Node and
# skips cleanly without the token its private repository needs; the check
# matrix tests the oldest R that DESCRIPTION allows; and only a job that never
# runs for a pull request deploys to GitHub Pages.

test_that("every job has a timeout and every workflow a concurrency group", {
  for (name in workflow_files()) {
    wf <- workflow(name)
    expect_false(is.null(wf$concurrency), info = name)
    for (id in names(wf$jobs)) {
      minutes <- wf$jobs[[id]][["timeout-minutes"]]
      expect_true(is.numeric(minutes) && minutes > 0 && minutes <= 60, info = paste(name, id))
    }
  }
})

test_that("no job may fail without failing its run", {
  for (name in workflow_files()) {
    wf <- workflow(name)
    for (id in names(wf$jobs)) {
      expect_null(wf$jobs[[id]][["continue-on-error"]], info = paste(name, id))
    }
  }
})

test_that("pushes and pull requests trigger workflows on main only", {
  for (name in workflow_files()) {
    on <- workflow_triggers(workflow(name))
    for (event in intersect(names(on), c("push", "pull_request"))) {
      branches <- on[[event]]$branches
      if (!is.null(branches)) {
        expect_identical(unlist(branches), "main", info = paste(name, event))
      }
    }
  }
})

# exlibris's repository is private until it is released, so the rebuild needs
# EXLIBRIS_TOKEN, a read-only token for it. Without the secret (a fork's pull
# request, or a repository that has not been given one) the job still passes,
# skips every step after the check, and says why in the job summary. The
# provenance job, which needs no other repository, gates every run regardless.
test_that("vendor-check rebuilds exlibris with its own Node, using EXLIBRIS_TOKEN", {
  job <- workflow("viewer.yml")$jobs[["vendor-check"]]
  checkout <- Filter(function(s) identical(s$with$repository, "coatless-wasm/exlibris"), job$steps)
  expect_length(checkout, 1L)
  expect_identical(checkout[[1]]$with$token, "${{ secrets.EXLIBRIS_TOKEN }}")
  # npm ci runs exlibris's dependencies' scripts: the token stays out of .git/config.
  expect_false(checkout[[1]]$with[["persist-credentials"]])
  node <- Filter(function(s) startsWith(s$uses %||% "", "actions/setup-node@"), job$steps)
  expect_length(node, 1L)
  expect_identical(node[[1]]$with[["node-version-file"]], "exlibris/.nvmrc")
  expect_null(node[[1]]$with[["node-version"]])
})

test_that("vendor-check skips every step after the token check when EXLIBRIS_TOKEN is absent", {
  job <- workflow("viewer.yml")$jobs[["vendor-check"]]
  expect_null(job[["if"]])
  ids <- vapply(job$steps, function(s) s$id %||% "", character(1))
  at <- match("exlibris-token", ids)
  expect_false(is.na(at))
  gate <- job$steps[[at]]
  expect_identical(gate$env$EXLIBRIS_TOKEN, "${{ secrets.EXLIBRIS_TOKEN }}")
  expect_null(gate[["if"]])
  after <- job$steps[-seq_len(at)]
  expect_gt(length(after), 0L)
  for (step in after) {
    expect_identical(
      step[["if"]],
      "steps.exlibris-token.outputs.available == 'true'",
      info = step$name %||% step$uses
    )
  }
  # Only the checkout of webrarian itself comes before the check.
  expect_identical(at, 2L)
  expect_match(job$steps[[1]]$uses, "^actions/checkout@")
  expect_null(job$steps[[1]]$with$repository)
})

run_token_gate <- function(token) {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "needs bash")
  job <- workflow("viewer.yml")$jobs[["vendor-check"]]
  ids <- vapply(job$steps, function(s) s$id %||% "", character(1))
  gate <- job$steps[[match("exlibris-token", ids)]]
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  output <- file.path(dir, "output")
  summary <- file.path(dir, "summary")
  file.create(c(output, summary))
  script <- file.path(dir, "gate.sh")
  writeLines(gate$run, script)
  status <- withr::with_envvar(
    c(EXLIBRIS_TOKEN = token, GITHUB_OUTPUT = output, GITHUB_STEP_SUMMARY = summary),
    system2("bash", c("-e", script), stdout = FALSE, stderr = FALSE)
  )
  list(status = status, output = readLines(output), summary = readLines(summary))
}

test_that("without EXLIBRIS_TOKEN the token check passes and explains the skip in the summary", {
  res <- run_token_gate("")
  expect_identical(res$status, 0L)
  expect_identical(res$output, "available=false")
  summary <- paste(res$summary, collapse = "\n")
  expect_match(summary, "EXLIBRIS_TOKEN", fixed = TRUE)
  expect_match(summary, "skipped", fixed = TRUE)
  expect_match(summary, "provenance", fixed = TRUE)
})

test_that("with EXLIBRIS_TOKEN the token check lets the rebuild run and never prints the token", {
  res <- run_token_gate("not-a-real-token")
  expect_identical(res$status, 0L)
  expect_identical(res$output, "available=true")
  expect_false(any(grepl("not-a-real-token", c(res$output, res$summary), fixed = TRUE)))
})

test_that("the provenance hash check runs on every push and pull request, token or not", {
  wf <- workflow("viewer.yml")
  on <- workflow_triggers(wf)
  expect_true(all(c("push", "pull_request") %in% names(on)))
  job <- wf$jobs$provenance
  expect_null(job[["if"]])
  expect_null(job$needs)
  for (step in job$steps) {
    expect_null(step[["if"]])
  }
  expect_false(grepl("EXLIBRIS_TOKEN", paste(unlist(job), collapse = "\n"), fixed = TRUE))
})

test_that("no workflow sets up an end-of-life Node", {
  for (name in workflow_files()) {
    for (job in workflow(name)$jobs) {
      for (step in job$steps) {
        if (!startsWith(step$uses %||% "", "actions/setup-node@")) {
          next
        }
        version <- step$with[["node-version"]]
        if (is.null(version)) {
          expect_false(is.null(step$with[["node-version-file"]]), info = name)
        } else {
          expect_gte(as.integer(sub("\\..*$", "", version)), 22L)
        }
      }
    }
  }
})

test_that("the check matrix covers CRAN's platforms and the oldest R DESCRIPTION allows", {
  legs <- workflow("R-CMD-check.yaml")$jobs[["R-CMD-check"]]$strategy$matrix$config
  os_r <- vapply(legs, function(leg) paste(leg$os, leg$r), character(1))
  depends <- gsub(
    "\\s+",
    " ",
    read.dcf(test_path("..", "..", "DESCRIPTION"), fields = "Depends")[[1]]
  )
  floor <- sub("^.*R \\(>= ?([0-9.]+)\\).*$", "\\1", depends)
  expect_match(floor, "^[0-9]+\\.[0-9]+")
  expected <- c(
    "ubuntu-latest release",
    "ubuntu-latest devel",
    "ubuntu-latest oldrel-1",
    paste("ubuntu-latest", floor),
    "macos-latest release",
    "windows-latest release"
  )
  expect_setequal(os_r, expected)
})

test_that("only a job that never runs for pull requests names the github-pages environment", {
  # The github-pages environment accepts deployments from main only: a job that
  # names it and runs for a pull request or a release tag fails before its
  # first step, and a workflow-wide Pages token would reach the build too.
  for (name in workflow_files()) {
    wf <- workflow(name)
    on <- workflow_triggers(wf)
    for (id in names(wf$jobs)) {
      job <- wf$jobs[[id]]
      environment <- if (is.list(job$environment)) job$environment$name else job$environment
      if (!identical(environment, "github-pages")) {
        next
      }
      where <- paste(name, id)
      if ("pull_request" %in% names(on)) {
        expect_match(
          job[["if"]] %||% "",
          "github.event_name != 'pull_request'",
          fixed = TRUE,
          info = where
        )
      }
      expect_false(any(c("release", "pull_request_target") %in% names(on)), info = where)
      expect_null(wf$permissions$pages, info = name)
      expect_identical(job$permissions$pages, "write", info = where)
    }
  }
  pkgdown <- workflow("pkgdown.yml")
  expect_setequal(names(pkgdown$jobs), c("build", "deploy"))
  expect_identical(pkgdown$jobs$deploy$needs, "build")
  # A pull request's run neither cancels a deploy nor waits behind one.
  expect_match(pkgdown$concurrency$group, "github.event_name == 'pull_request'", fixed = TRUE)
})

# --- A live demo on the pkgdown site ------------------------------------------
#
# The deploy builds a shipped example with webrarian itself and publishes it
# at demo/ beside the pkgdown pages. A failed bind() fails the step, so a
# broken demo never deploys.

test_that("the pkgdown build binds the data-analysis example into docs/demo", {
  pkgdown <- workflow("pkgdown.yml")
  steps <- pkgdown$jobs$build$steps
  names <- vapply(steps, function(s) s$name %||% "", character(1))
  site <- match("Build site", names)
  demo <- match("Build the demo collection", names)
  cache <- match("Cache webR downloads", names)
  upload <- match("Upload Pages artifact", names)
  expect_false(anyNA(c(site, demo, cache, upload)))
  expect_lt(site, demo)
  expect_lt(cache, demo)
  expect_lt(demo, upload)

  step <- steps[[demo]]
  expect_identical(step$shell, "Rscript {0}")
  expect_null(step[["continue-on-error"]])
  expect_null(step[["if"]])
  expect_match(step$run, 'collection_example("data-analysis"', fixed = TRUE)
  expect_match(step$run, "webrarian::bind(", fixed = TRUE)
  expect_match(step$run, '"docs", "demo"', fixed = TRUE)
  # bind()'s default build: no offline switch, so the engine is bundled and
  # the page boots on GitHub Pages without cross-origin isolation.
  expect_no_match(step$run, "offline", fixed = TRUE)
  expect_identical(step$env$R_USER_CACHE_DIR, "${{ runner.temp }}/r-user-cache")

  cache_step <- steps[[cache]]
  expect_match(cache_step$uses, "^actions/cache@")
  expect_identical(cache_step$with$path, "${{ runner.temp }}/r-user-cache")
  expect_match(cache_step$with$key, "inst/examples/data-analysis/_webrarian.yml", fixed = TRUE)
})

# The R code is formatted with air, configured by air.toml, and CI fails on
# code air would reformat. lintr is gone.
test_that("CI checks the formatting with air", {
  wf <- workflow("format.yml")
  expect_setequal(names(wf$jobs), "air")
  uses <- vapply(
    Filter(function(s) !is.null(s$uses), wf$jobs$air$steps),
    `[[`,
    character(1),
    "uses"
  )
  expect_true("posit-dev/setup-air@v1" %in% uses)
  expect_match(job_runs(wf$jobs$air), "air format . --check", fixed = TRUE)
  root <- test_path("..", "..")
  expect_true(file.exists(file.path(root, "air.toml")))
  expect_false(file.exists(file.path(root, ".lintr")))
})

# The Docker check must probe the daemon, not just the CLI.

make_local_pkg <- function(env = parent.frame()) {
  pkg_root <- withr::local_tempdir(.local_envir = env)
  pkg <- fs::path(pkg_root, "demotools")
  fs::dir_create(pkg)
  writeLines(c("Package: demotools", "Version: 0.1.0"), fs::path(pkg, "DESCRIPTION"))
  pkg_root
}

test_that("docker_status tells a running daemon from a stopped one and a missing CLI", {
  local_fake_docker("ok")
  expect_equal(docker_status(), "running")
  expect_true(docker_available())
})

test_that("an installed CLI with no daemon is 'stopped'", {
  local_fake_docker("down")
  expect_equal(docker_status(), "stopped")
  expect_false(docker_available())
})

test_that("no docker on PATH is 'missing'", {
  local_mocked_bindings(docker_path = function() "")
  expect_equal(docker_status(), "missing")
})

test_that("check_requirements says when Docker is installed but not running", {
  local_fake_docker("down")
  root <- local_collection()
  suppressMessages(settings_set(root, "packages.github" = list("user/repo")))
  expect_error(check_requirements(root), "installed but not running")
})

test_that("bind() with the daemon down fails before touching the previous site", {
  local_fake_docker("down")
  root <- local_collection()
  suppressMessages(bind(root))
  before <- readLines(fs::path(root, "_site", "index.html"))
  suppressMessages(settings_set(root, "packages.github" = list("user/repo")))

  expect_error(suppressMessages(bind(root)), "installed but not running")
  expect_identical(readLines(fs::path(root, "_site", "index.html")), before)
})

test_that("a container that cannot start is not reported as a compile error", {
  local_fake_docker("broken")
  pkg_root <- make_local_pkg()
  expect_error(
    suppressMessages(build_packages_rwasm_docker(
      character(),
      "demotools",
      pkg_root,
      withr::local_tempdir(),
      "4.6",
      "0.6.0"
    )),
    "could not start the webR build container"
  )
})

test_that("a compile failure is reported as one", {
  local_fake_docker("fail")
  pkg_root <- make_local_pkg()
  expect_error(
    suppressMessages(build_packages_rwasm_docker(
      character(),
      "demotools",
      pkg_root,
      withr::local_tempdir(),
      "4.6",
      "0.6.0"
    )),
    "failed inside the webR container"
  )
})

test_that("diagnose_tools reports the daemon state", {
  local_fake_docker("down")
  msgs <- testthat::capture_messages(res <- diagnose_tools())
  expect_true(any(grepl("installed but not running", msgs)))
  expect_equal(res$docker$status, "stopped")
  expect_false(res$docker$available)
})

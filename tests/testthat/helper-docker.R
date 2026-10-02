# Put a fake `docker` first on PATH for the rest of the calling test.
# Returns the path of its invocation log. With `script`, each `docker run`
# copies the build.R it was given there.
local_fake_docker <- function(
  mode = "ok",
  github = NULL,
  r = "4.6",
  script = NULL,
  env = parent.frame()
) {
  testthat::skip_on_os("windows")
  bin <- withr::local_tempdir(.local_envir = env)
  docker <- fs::path(bin, "docker")
  fs::file_copy(testthat::test_path("fixtures", "fake-docker.sh"), docker)
  Sys.chmod(docker, "0755")
  log <- fs::path(bin, "docker.log")
  withr::local_path(bin, action = "prefix", .local_envir = env)
  withr::local_envvar(
    FAKE_DOCKER_MODE = mode,
    FAKE_DOCKER_LOG = as.character(log),
    FAKE_DOCKER_R = r,
    FAKE_DOCKER_GITHUB = if (is.null(github)) "" else as.character(github),
    FAKE_DOCKER_SCRIPT = if (is.null(script)) "" else as.character(script),
    .local_envir = env
  )
  log
}

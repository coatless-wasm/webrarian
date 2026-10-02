# The vendored viewer embeds one webR JavaScript client; only engine versions
# verified with it, all on its major.minor line, may be selected.
# exlibris refuses to boot any other line (exlibris's engineVersionProblem()).

test_that("the listed version resolves to its R line", {
  expect_equal(resolve_webr_version("0.6.0")$r_version, "4.6")
  expect_true(resolve_webr_version("0.6.0")$tested)
  expect_equal(resolve_webr_version("0.6.0")$version, "0.6.0")
})

test_that("webR 0.5.x and 0.4.x are rejected and the error lists the tested versions", {
  expect_error(resolve_webr_version("0.5.9"), "not supported")
  expect_error(resolve_webr_version("0.5.8"), "0.6.0")
  expect_error(resolve_webr_version("0.4.2"), "0.6.0")
  expect_error(get_r_version_for_webr("0.4.1"), "not supported")
})

test_that("an unlisted patch on the client's line is accepted with a warning", {
  expect_warning(v <- resolve_webr_version("0.6.9"), "not been tested")
  expect_equal(v$r_version, "4.6")
  expect_false(v$tested)
})

test_that("an untested minor line is an error", {
  expect_error(resolve_webr_version("0.7.0"), "Tested versions")
})

test_that("a version that is not major.minor.patch is an error, not an untested patch", {
  # "0.6" matches the client's line, but it names no release: the page would
  # load https://webr.r-wasm.org/v0.6/ and Docker would pull webr:v0.6.
  for (v in c("0.6", "0.6.0.1")) {
    expect_error(resolve_webr_version(v), "Tested versions", info = v)
    expect_error(resolve_webr_version(v), "major.minor.patch", info = v)
  }
  withr::local_envvar(WEBRARIAN_WEBR_VERSION = "0.6")
  expect_error(
    resolve_webr_version(apply_config_defaults(list())$webr$version),
    "Tested versions"
  )
})

test_that("bind() refuses a two-part version rather than build a site whose engine 404s", {
  # With build.bundle-engine false nothing is downloaded, so nothing else
  # would stop the build.
  root <- local_collection()
  suppressMessages(settings_set(root, "webr.version" = "0.6"))
  expect_error(suppressMessages(bind(root)), "Tested versions")
  expect_false(fs::dir_exists(fs::path(root, "_site")))
  expect_length(staging_leftovers(root), 0L)
})

test_that("a YAML number is rejected with a hint to quote it", {
  expect_error(resolve_webr_version(0.6), "quote")
})

test_that("bind() checks the version on every build, packages or not", {
  root <- local_collection()
  suppressMessages(settings_set(root, "webr.version" = "0.5.9"))
  expect_error(suppressMessages(bind(root)), "0.5.9")
  expect_false(fs::dir_exists(fs::path(root, "_site")))
  expect_length(staging_leftovers(root), 0L)
})

test_that("WEBRARIAN_WEBR_VERSION applies wherever the config names no version", {
  withr::local_envvar(WEBRARIAN_WEBR_VERSION = "0.6.2")
  expect_equal(apply_config_defaults(list())$webr$version, "0.6.2")
  expect_equal(webr_assets_version(), "0.6.2")
  expect_equal(create_initial_config("x", "minimal")$webr$version, "0.6.2")
})

test_that("the default version is one of the listed ones", {
  listed <- vapply(webr_versions_table()$versions, function(v) v$version, character(1))
  expect_true(default_webr_version() %in% listed)
  expect_false(any(startsWith(listed, "0.4.") | startsWith(listed, "0.5.")))
})

test_that("every listed version is on the line of the webR client the viewer bundles", {
  prov <- jsonlite::fromJSON(
    system.file("viewer", "PROVENANCE.json", package = "webrarian"),
    simplifyVector = TRUE
  )
  table <- webr_versions_table()
  expect_identical(table$verified_with_client, prov$webrClientVersion)
  for (entry in table$versions) {
    expect_identical(
      version_line(entry$version),
      version_line(prov$webrClientVersion),
      info = entry$version
    )
  }
})

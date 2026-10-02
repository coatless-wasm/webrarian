# A WebAssembly package repository for tests, served from a temp dir by
# httpuv's static file server. Static files are answered on httpuv's I/O
# thread, so a download that blocks the R session still gets its response.
#
# `packages` is a named list of DESCRIPTION fields per package, e.g.
# list(glue = c(Version = "1.8.0"), alpha = c(Version = "1.0", Imports = "glue")).
# Packages in `missing` are in the index but their .tgz is not served (404);
# `corrupt` ones are served with bytes that do not match their MD5sum.
local_fixture_repo <- function(
  packages,
  r_version = "4.6",
  missing = character(),
  corrupt = character(),
  index = TRUE,
  env = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = env)
  contrib <- fs::path(dir, "bin", "emscripten", "contrib", r_version)
  fs::dir_create(contrib)

  rows <- list()
  for (pkg in names(packages)) {
    fields <- packages[[pkg]]
    version <- if ("Version" %in% names(fields)) fields[["Version"]] else "1.0.0"
    desc <- c(
      Package = pkg,
      Version = version,
      fields[setdiff(names(fields), c("Package", "Version"))]
    )
    src <- fs::path(withr::local_tempdir(.local_envir = env), pkg)
    fs::dir_create(src)
    write.dcf(as.data.frame(as.list(desc), check.names = FALSE), fs::path(src, "DESCRIPTION"))
    tgz <- fs::path(contrib, sprintf("%s_%s.tgz", pkg, version))
    withr::with_dir(fs::path_dir(src), utils::tar(tgz, pkg, compression = "gzip", tar = "internal"))
    md5 <- unname(tools::md5sum(tgz))
    if (pkg %in% missing) {
      fs::file_delete(tgz)
    }
    if (pkg %in% corrupt) {
      writeLines("corrupt", tgz)
    }
    rows[[pkg]] <- c(desc, MD5sum = md5)
  }

  if (index) {
    cols <- unique(c("Package", "Version", unlist(lapply(rows, names))))
    m <- matrix(character(), 0, length(cols), dimnames = list(NULL, cols))
    for (r in rows) {
      row <- stats::setNames(rep(NA_character_, length(cols)), cols)
      row[names(r)] <- r
      m <- rbind(m, row)
    }
    rownames(m) <- m[, "Package"]
    saveRDS(m, fs::path(contrib, "PACKAGES.rds"))
  }

  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1",
    port,
    list(
      call = function(req) {
        list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "Not Found")
      },
      staticPaths = list("/" = httpuv::staticPath(dir, indexhtml = FALSE, fallthrough = FALSE))
    )
  )
  withr::defer(httpuv::stopServer(server), envir = env)
  list(url = sprintf("http://127.0.0.1:%d", port), dir = dir, contrib = contrib)
}

# Point the webrarian caches at a fresh temp dir for the calling test, with
# the package cache on whatever the caller's environment says:
# WEBRARIAN_PACKAGE_CACHE is an on/off switch (R/assets.R), so a caller that
# exported WEBRARIAN_PACKAGE_CACHE=0 would otherwise turn every cache hit
# these tests expect into a download.
local_isolated_cache <- function(env = parent.frame()) {
  withr::local_envvar(
    R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env),
    WEBRARIAN_PACKAGE_CACHE = NA,
    .local_envir = env
  )
  withr::local_options(webrarian.package_cache = NULL, .local_envir = env)
}

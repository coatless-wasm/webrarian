# Measure built sites for the size table in vignettes/getting-started.qmd
# ("Site size").
#
# Builds four throwaway collections (bare, and with dplyr and ggplot2, each
# with the webR engine bundled, the default, and with build.bundle-engine:
# false) and prints the table rows. It downloads the webR engine and the
# packages, so give it a scratch cache. From the package root:
#
#   R_USER_CACHE_DIR="$(mktemp -d)" Rscript tools/measure-sizes.R
#
# The packages column counts repo/ and the library image under library/,
# which a site that bundles packages ships both of.

pkgload::load_all(quiet = TRUE)
# The drift report (packages.json) would otherwise ask CRAN for its versions;
# the sizes do not depend on the answer.
options(webrarian.cran_repo = FALSE)

size <- function(bytes) {
  if (bytes == 0) "0" else format(fs::fs_bytes(bytes))
}

measure <- function(site, mode, packages = character()) {
  root <- fs::path(tempfile("webrarian-size-"), "site")
  suppressMessages(catalog(root, detect = FALSE))
  if (length(packages) > 0L) {
    suppressMessages(acquire_package(packages, path = root))
  }
  if (identical(mode, "engine from CDN")) {
    suppressMessages(settings_set(root, "build.bundle-engine" = FALSE))
  }
  result <- suppressMessages(bind(root))
  parts <- result$size_by_part
  cat(sprintf(
    "| %s | %s | %s | %s | %s | %s |\n",
    site,
    mode,
    size(sum(parts)),
    size(parts[["engine"]]),
    size(parts[["packages"]]),
    size(parts[["viewer"]])
  ))
  fs::dir_delete(fs::path_dir(root))
}

cat("| Site | Mode | On disk | Engine | Packages | Viewer and notices |\n")
cat("|------|------|---------|--------|----------|--------------------|\n")
measure("Bare REPL", "engine bundled (default)")
measure("Bare REPL", "engine from CDN")
measure("dplyr and ggplot2", "engine bundled (default)", c("dplyr", "ggplot2"))
measure("dplyr and ggplot2", "engine from CDN", c("dplyr", "ggplot2"))

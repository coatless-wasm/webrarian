#' @keywords internal
#' @section Package options:
#' Build downloads are cached, so a rebuild reuses them. These settings
#' control the cache, the webR version and the CRAN comparison:
#'
#' - **Where the cache is.** `tools::R_user_dir("webrarian", "cache")`. Set
#'   the `R_USER_CACHE_DIR` environment variable to move it (webrarian's cache
#'   is then its `R/webrarian` subdirectory). [webr_cache_info()] shows what is
#'   there and [webr_cache_clear()] removes it. After each webR engine
#'   download, only the three most recently used engines are kept, and cached
#'   packages are removed for R versions that neither those engines nor this
#'   webrarian's own engines use.
#' - **`webrarian.package_cache` option, `WEBRARIAN_PACKAGE_CACHE` environment
#'   variable.** This is a switch, not a path. Set either to `FALSE` (or
#'   `"0"`) to make every build download its packages afresh. The option wins when both are
#'   set. A cached package is reused only when its checksum matches the
#'   repository's.
#' - **`WEBRARIAN_WEBR_VERSION` environment variable.** The webR version used
#'   when `_webrarian.yml` names none, for new collections, and for
#'   [collection_mirror()]. It must be a version tested with this webrarian,
#'   or a patch release on the same line (accepted with a warning).
#' - **`webrarian.cran_repo` option.** The repository [check_inventory()] and
#'   [bind()] compare WebAssembly package versions with, given as a URL, or
#'   `FALSE` to skip the comparison. Unset, it is the CRAN mirror in `getOption("repos")`,
#'   else `https://cloud.r-project.org`. Its index is read at most once a day
#'   and kept in the user cache, and so is a failed read. When it cannot be
#'   reached the CRAN versions are `NA`, and nothing is printed or fails.
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL

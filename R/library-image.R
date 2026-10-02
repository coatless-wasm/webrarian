# Package library images
#
# A site that bundles its packages used to have the viewer install every one of
# them from repo/ on every visit: download, unpack, install, one after another.
# With build.library-image (the default) bind() also writes the installed
# packages as one filesystem image, which the viewer mounts at start-up
# (config packages.library-images, exlibris) so they load in place.
#
# The image is the format webr::mount() reads: an uncompressed ustar archive of
# the library, "<package>/<path>" per file, whose last member, .vfs-index.json,
# lists where each file's bytes start and end in the archive; gzipped. The
# packages come from the .tgz binaries in repo/, which unpack to "<package>/".

#' One 512-byte ustar header for a regular file
#' @noRd
tar_header <- function(name, size) {
  header <- raw(512)
  put <- function(offset, value) {
    bytes <- charToRaw(value)
    header[offset + seq_along(bytes)] <<- bytes
  }
  if (nchar(name, type = "bytes") > 100) {
    # ustar splits a long path at a slash into prefix (155 bytes) and name (100).
    slashes <- gregexpr("/", name, fixed = TRUE)[[1]]
    ok <- slashes[slashes > 0 & slashes <= 156 & nchar(name, type = "bytes") - slashes <= 100]
    if (length(ok) == 0L) {
      cli::cli_abort("Cannot store {.path {name}} in a library image: the path is too long.")
    }
    cut <- max(ok)
    put(345, substr(name, 1, cut - 1))
    name <- substr(name, cut + 1, nchar(name))
  }
  put(0, name)
  put(100, "0000644")
  put(108, "0000000")
  put(116, "0000000")
  put(124, sprintf("%011o", size))
  put(136, sprintf("%011o", 0L))
  put(148, strrep(" ", 8))
  put(156, "0")
  put(257, "ustar")
  put(263, "00")
  # The checksum is the byte sum with the checksum field read as spaces:
  # six octal digits, NUL, space.
  put(148, sprintf("%06o", sum(as.integer(header))))
  header[155] <- as.raw(0)
  header[156] <- charToRaw(" ")
  header
}

#' Write the library under `lib_dir` as a mountable image at `dest`
#'
#' @param lib_dir A directory of installed packages ("<package>/DESCRIPTION", ...).
#' @param dest Path of the .tgz to write.
#' @return `dest`, invisibly.
#' @noRd
write_library_image <- function(lib_dir, dest) {
  # Byte order, not the locale's: the same packages give the same image, and the same name, on
  # every machine.
  files <- sort(
    list.files(lib_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    method = "radix"
  )
  # Package binaries from repo.r-wasm.org carry their own index; the image gets one index, last.
  files <- files[basename(files) != ".vfs-index.json"]
  con <- gzfile(dest, "wb")
  on.exit(close(con), add = TRUE)
  offset <- 0
  index <- vector("list", length(files))
  write_member <- function(name, bytes) {
    writeBin(tar_header(name, length(bytes)), con)
    start <- offset + 512
    pad <- (512 - length(bytes) %% 512) %% 512
    writeBin(c(bytes, raw(pad)), con)
    offset <<- start + length(bytes) + pad
    start
  }
  for (i in seq_along(files)) {
    path <- file.path(lib_dir, files[[i]])
    bytes <- readBin(path, "raw", file.size(path))
    start <- write_member(files[[i]], bytes)
    index[[i]] <- list(
      filename = paste0("/", files[[i]]),
      start = start,
      end = start + length(bytes)
    )
  }
  json <- jsonlite::toJSON(list(files = index), auto_unbox = TRUE, digits = NA)
  write_member(".vfs-index.json", charToRaw(as.character(json)))
  writeBin(raw(1024), con) # end of archive
  invisible(dest)
}

#' Dependencies of the indexed packages that the index does not hold
#'
#' A mounted image makes the viewer report each package it holds as installed,
#' so it never runs webr::install() for them, and webr::install() is what
#' fetches a dependency the site does not bundle (a compiled package on a site
#' that installs its other packages when the page opens). An image is
#' therefore written only for a closed set: every Depends, Imports and
#' LinkingTo entry of every indexed package is indexed too, or is part of R.
#'
#' @param index `read.dcf()` rows of the repository index (`PACKAGES`).
#' @return The missing dependencies in byte order; `character()` for a closed set.
#' @noRd
library_image_missing_deps <- function(index) {
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(index))
  needed <- parse_deps(as.character(index[, fields]))
  sort(setdiff(needed, c(index[, "Package"], base_package_names(), "R")), method = "radix")
}

#' Build the site's library image from the package binaries in `pkg_dir`
#'
#' Takes the binaries the repository index (`PACKAGES`) names, one
#' `<Package>_<Version>.tgz` per package, rather than every `.tgz` in the
#' directory: a rebuild in place (`build.clean: false`) can leave an older
#' version's binary there, and unpacking two versions of a package into one
#' library merges their files into a broken package.
#'
#' @param pkg_dir The site's repo/bin/emscripten/contrib/<r> directory.
#' @param output_dir The site being built.
#' @param warn_bytes Above this size the image is still written, with a
#'   message: some hosts refuse a single file that large (Cloudflare Pages
#'   takes up to 25 MiB). Internal; tests lower it, `bind()` never passes it.
#' @return The image's path relative to the site ("library/library-<hash>.tgz"),
#'   or NULL when there are no packages, (with a warning) when the index names
#'   a binary that is not there, or (with a message) when an indexed package
#'   needs one the index does not hold.
#' @noRd
build_library_image <- function(pkg_dir, output_dir, warn_bytes = 25 * 1024^2) {
  index_file <- fs::path(pkg_dir, "PACKAGES")
  if (!fs::file_exists(index_file)) {
    return(NULL)
  }
  index <- read.dcf(index_file, fields = c("Package", "Version", "Depends", "Imports", "LinkingTo"))
  if (nrow(index) == 0L) {
    return(NULL)
  }
  tgz <- sort(
    as.character(fs::path(pkg_dir, paste0(index[, "Package"], "_", index[, "Version"], ".tgz"))),
    method = "radix"
  )
  missing <- tgz[!fs::file_exists(tgz)]
  if (length(missing) > 0L) {
    cli::cli_warn(c(
      "No package library image: the repository index names {length(missing)} binar{?y/ies} that {?is/are} not in {.path {pkg_dir}}.",
      "x" = "Missing: {.file {basename(missing)}}.",
      "i" = "The page installs the packages from {.path repo/} one by one instead."
    ))
    return(NULL)
  }
  missing_deps <- library_image_missing_deps(index)
  if (length(missing_deps) > 0L) {
    cli::cli_inform(c(
      "i" = "No package library image: the site's packages need {.pkg {missing_deps}}, which the site does not bundle.",
      " " = "The page installs its packages from {.path repo/} and the other repositories when it opens."
    ))
    return(NULL)
  }
  lib <- fs::path(tempfile("webrarian-library-"))
  fs::dir_create(lib)
  on.exit(fs::dir_delete(lib), add = TRUE)
  for (f in tgz) {
    utils::untar(f, exdir = lib, tar = "internal")
  }

  image_dir <- fs::path(output_dir, "library")
  # One image per site: a rebuild in place (build.clean: false) replaces the last one.
  if (fs::dir_exists(image_dir)) {
    fs::file_delete(fs::dir_ls(image_dir, glob = "*.tgz"))
  }
  fs::dir_create(image_dir)
  tmp <- fs::path(image_dir, "library.tgz.partial")
  write_library_image(lib, tmp)
  # Named after its contents, so it can be cached for good (cache_rules()).
  name <- sprintf("library-%s.tgz", substr(rlang::hash_file(tmp), 1, 8))
  fs::file_move(tmp, fs::path(image_dir, name))
  size <- fs::file_size(fs::path(image_dir, name))
  if (size > warn_bytes) {
    cli::cli_inform(c(
      "i" = "The package library image {.file library/{name}} is {format(size)}, larger than some hosts accept for one file (Cloudflare Pages takes up to 25 MiB).",
      " " = "If your host refuses it, set {.code build.library-image: false} in {.file _webrarian.yml} to leave it out; the page then installs the packages one by one."
    ))
  }
  paste0("library/", name)
}

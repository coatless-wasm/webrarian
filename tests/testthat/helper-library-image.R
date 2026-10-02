# Reads a library image back (see R/library-image.R): the regular files by
# path, the parsed .vfs-index.json, and the uncompressed tar bytes.
read_library_image <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con), add = TRUE)
  chunks <- list()
  repeat {
    chunk <- readBin(con, "raw", 1e6)
    if (length(chunk) == 0L) {
      break
    }
    chunks[[length(chunks) + 1L]] <- chunk
  }
  bytes <- unlist(chunks, use.names = FALSE)
  text <- function(from, to) {
    b <- bytes[from:to]
    end <- match(as.raw(0), b, nomatch = length(b) + 1L) - 1L
    rawToChar(b[seq_len(end)])
  }
  files <- list()
  index <- NULL
  at <- 0
  while (at + 512 <= length(bytes) && any(bytes[(at + 1):(at + 512)] != as.raw(0))) {
    name <- text(at + 1, at + 100)
    prefix <- text(at + 346, at + 500)
    if (nzchar(prefix)) {
      name <- paste0(prefix, "/", name)
    }
    size <- strtoi(trimws(text(at + 125, at + 136)), 8L)
    data <- if (size > 0) bytes[(at + 513):(at + 512 + size)] else raw()
    if (identical(name, ".vfs-index.json")) {
      index <- jsonlite::fromJSON(rawToChar(data), simplifyVector = FALSE)
    } else {
      files[[name]] <- data
    }
    at <- at + 512 + ceiling(size / 512) * 512
  }
  list(files = files, index = index, bytes = bytes)
}

# A directory laid out like an installed library: <package>/<file>.
local_fake_library <- function(files, env = parent.frame()) {
  lib <- withr::local_tempdir(.local_envir = env)
  for (rel in names(files)) {
    fs::dir_create(fs::path_dir(fs::path(lib, rel)))
    writeBin(charToRaw(files[[rel]]), fs::path(lib, rel))
  }
  lib
}

# A package binary as a repository holds one: <pkg_dir>/<package>_<version>.tgz, unpacking to
# <package>/DESCRIPTION plus `files` (relative path = text).
write_fake_binary <- function(pkg_dir, package, version, files = list()) {
  src <- fs::path(tempfile("webrarian-binary-"), package)
  fs::dir_create(src)
  on.exit(fs::dir_delete(fs::path_dir(src)), add = TRUE)
  writeLines(
    c(paste("Package:", package), paste("Version:", version)),
    fs::path(src, "DESCRIPTION")
  )
  for (rel in names(files)) {
    fs::dir_create(fs::path_dir(fs::path(src, rel)))
    writeBin(charToRaw(files[[rel]]), fs::path(src, rel))
  }
  dest <- fs::path(pkg_dir, paste0(package, "_", version, ".tgz"))
  withr::with_dir(
    fs::path_dir(src),
    utils::tar(dest, package, compression = "gzip", tar = "internal")
  )
  invisible(dest)
}

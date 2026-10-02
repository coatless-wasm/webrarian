# Build an offline webR mirror with collection_mirror()
#
# A mirror is a self-contained webR site: the engine, the packages you name
# (with their dependencies) and a package index, so install.packages() works
# with no network. Host it anywhere that sends the cross-origin isolation
# headers webR needs; the mirror's _headers file lists them, and
# vignette("enterprise", package = "webrarian") explains the options.
#
# Run it with the directory to build into (default: ./webr-mirror):
#   Rscript build-mirror.R /srv/webr-mirror

library(webrarian)

args <- commandArgs(trailingOnly = TRUE)
dest <- if (length(args) > 0) args[[1]] else "webr-mirror"

favicon <- system.file("examples", "mirror", "sample-favicon.svg", package = "webrarian")

collection_mirror(dest, packages = c("cli", "glue"), favicon = favicon)

cat("Mirror built in", normalizePath(dest), "\n")
if (interactive()) {
  reading_room(dest)
} else {
  cat("Preview it with: webrarian::reading_room(\"", dest, "\")\n", sep = "")
}

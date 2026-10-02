#!/usr/bin/env Rscript
# Read the "CRAN incoming feasibility" section of an R CMD check log and fail
# on anything but the new-submission note. Exits 1 on an invalid URL, DOI or
# file URI, and when no check log or incoming section is found.
#
#   Rscript tools/cran-incoming.R CHECK_DIR
#
# exlibris and pyodidarian are still private, so their repositories and
# pyodidarian's PyPI page answer 404. Those URLs are listed and allowed; any
# other invalid URL fails. Remove an entry here once its project is public.

private <- c(
  "^https?://github\\.com/coatless-wasm/(exlibris|pyodidarian)(/.*)?$",
  "^https?://pypi\\.org/project/pyodidarian/?$"
)

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[[1L]] else "check"
fail <- function(...) {
  cat("::error::", ..., "\n", sep = "")
  quit(status = 1)
}

log <- list.files(dir, pattern = "^00check\\.log$", recursive = TRUE, full.names = TRUE)
if (!length(log)) {
  fail("no 00check.log under ", dir)
}
lines <- readLines(log[[1L]], warn = FALSE)

start <- grep("checking CRAN incoming feasibility", lines, fixed = TRUE)
if (!length(start)) {
  fail("no CRAN incoming feasibility section in ", log[[1L]])
}
start <- start[[1L]]
after <- grep("^\\* checking", lines)
end <- after[after > start]
end <- if (length(end)) end[[1L]] - 1L else length(lines)
section <- lines[start:end]
writeLines(section)

# Each "Found the following (possibly) invalid URLs:" block lists "  URL: ..."
# entries, each followed by lines indented further, until an unindented line.
urls <- character()
in_urls <- FALSE
for (line in section) {
  if (grepl("(possibly) invalid URLs:", line, fixed = TRUE)) {
    in_urls <- TRUE
  } else if (in_urls && grepl("^  URL: ", line)) {
    urls <- c(urls, trimws(sub("^  URL: ", "", line)))
  } else if (in_urls && !grepl("^\\s", line)) {
    in_urls <- FALSE
  }
}
# "URL: a (moved to b)" names the URL first.
urls <- sub("\\s.*$", "", urls)
allowed <- Reduce(`|`, lapply(private, grepl, x = urls), logical(length(urls)))

cat("\n")
if (any(allowed)) {
  cat("Allowed, because the project is still private:\n")
  cat(paste0("  ", unique(urls[allowed]), "\n"), sep = "")
}
problems <- character()
if (any(!allowed)) {
  problems <- c(problems, paste("invalid URLs:", paste(unique(urls[!allowed]), collapse = ", ")))
}
if (any(grepl("(possibly) invalid DOIs", section, fixed = TRUE))) {
  problems <- c(problems, "invalid DOIs (above)")
}
if (any(grepl("(possibly) invalid file URIs", section, fixed = TRUE))) {
  problems <- c(problems, "invalid file URIs (above)")
}
if (length(problems)) {
  fail("CRAN's incoming checks found ", paste(problems, collapse = "; "))
}
cat("CRAN's incoming checks found nothing but a new submission.\n")

#!/usr/bin/env Rscript
# Run a slice of webrarian's tests for CI. Exits 1 on any failure or error, on
# a run with no tests, and with --no-skips on any skipped test: a job that
# exists to run the browser or Docker suites must not pass by skipping them.
#
#   Rscript tools/ci-test.R --filter REGEX  [--no-skips]
#   Rscript tools/ci-test.R --exclude REGEX [--no-skips]

args <- commandArgs(trailingOnly = TRUE)
value_of <- function(flag) {
  at <- match(flag, args)
  if (is.na(at) || at == length(args)) NULL else args[[at + 1L]]
}
filter <- value_of("--filter")
exclude <- value_of("--exclude")
no_skips <- "--no-skips" %in% args
if (is.null(filter) == is.null(exclude)) {
  stop("Pass exactly one of --filter or --exclude.", call. = FALSE)
}

results <- if (!is.null(filter)) {
  devtools::test(filter = filter, stop_on_failure = FALSE)
} else {
  devtools::test(filter = exclude, invert = TRUE, stop_on_failure = FALSE)
}
df <- as.data.frame(results)
if (nrow(df) == 0L) {
  cat("No tests ran.\n")
  quit(status = 1)
}
bad <- df[df$failed > 0 | df$error, c("file", "test"), drop = FALSE]
if (nrow(bad) > 0L) {
  cat("Failed:\n")
  print(bad, row.names = FALSE)
  quit(status = 1)
}
if (no_skips && any(df$skipped)) {
  cat("Skipped tests fail this job:\n")
  print(df[df$skipped, c("file", "test"), drop = FALSE], row.names = FALSE)
  quit(status = 1)
}
cat(sprintf("%d tests passed.\n", nrow(df)))

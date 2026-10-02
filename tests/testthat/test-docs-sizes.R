# One measured size table (vignettes/getting-started.qmd, "Site size"),
# produced by tools/measure-sizes.R.

size_table <- function() {
  lines <- readLines(
    repo_path("vignettes", "getting-started.qmd"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  rows <- grep("^\\| (Bare REPL|dplyr and ggplot2) \\|", lines, value = TRUE)
  if (length(rows) == 0L) {
    return(data.frame(
      site = character(),
      mode = character(),
      total = character(),
      engine = character(),
      packages = character(),
      viewer = character()
    ))
  }
  cells <- do.call(rbind, lapply(strsplit(rows, "|", fixed = TRUE), function(x) trimws(x[2:7])))
  colnames(cells) <- c("site", "mode", "total", "engine", "packages", "viewer")
  as.data.frame(cells, stringsAsFactors = FALSE)
}

# A size as bind() prints it ("0", "11.7K", "1.39M", "45.1M"), in M.
megabytes <- function(x) {
  if (identical(x, "0")) {
    return(0)
  }
  value <- as.numeric(sub("[KMG]$", "", x))
  switch(sub("^[0-9.]+", "", x), K = value / 1024, M = value, G = value * 1024)
}

test_that("the size table covers both engine modes and adds up", {
  skip_if_no_source_tree()
  sizes <- size_table()
  expect_identical(nrow(sizes), 4L)
  expect_setequal(sizes$mode, c("engine bundled (default)", "engine from CDN"))
  for (i in seq_len(nrow(sizes))) {
    parts <- vapply(unlist(sizes[i, c("engine", "packages", "viewer")]), megabytes, numeric(1))
    # bind() rounds each figure to three significant digits, so the parts can
    # miss the total by up to about 0.15M (84M against 45.1M + 37.5M + 1.39M);
    # a gap of 0.2M or more is a row that does not add up.
    expect_lt(
      abs(sum(parts) - megabytes(sizes$total[[i]])),
      0.2,
      label = sprintf(
        "the gap between the parts and the total of '%s | %s'",
        sizes$site[[i]],
        sizes$mode[[i]]
      )
    )
  }
  # build.bundle-engine: false leaves the engine and the prebuilt packages to
  # webR's servers, as the article says.
  expect_true(all(sizes$engine[sizes$mode == "engine from CDN"] == "0"))
  expect_true(all(sizes$packages[sizes$mode == "engine from CDN"] == "0"))
})

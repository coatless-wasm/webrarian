# Helper functions

summarize_data <- function(df) {
  cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
  cat("Names:", paste(names(df), collapse = ", "), "\n")
}

calc_correlation <- function(x, y) {
  r <- cor(x, y)
  cat(sprintf("r = %.3f\n", r))
  r
}

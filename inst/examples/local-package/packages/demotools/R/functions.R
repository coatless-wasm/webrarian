#' Quick Summary Statistics
#'
#' Calculate common summary statistics for a numeric vector.
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Should NA values be removed?
#'
#' @return A named list with summary statistics.
#' @export
#'
#' @examples
#' quick_summary(c(1, 2, 3, 4, 5))
quick_summary <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  list(
    n = length(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    min = min(x),
    max = max(x),
    q25 = unname(quantile(x, 0.25)),
    q75 = unname(quantile(x, 0.75))
  )
}

#' Format Number with Commas
#'
#' Format a number with thousand separators and specified decimal places.
#'
#' @param x A numeric value.
#' @param digits Number of decimal places.
#' @param prefix Optional prefix (e.g., "$").
#' @param suffix Optional suffix (e.g., "%").
#'
#' @return A formatted character string.
#' @export
#'
#' @examples
#' format_number(1234567.89)
#' format_number(1234.5, prefix = "$")
format_number <- function(x, digits = 2, prefix = "", suffix = "") {
  formatted <- formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
  paste0(prefix, formatted, suffix)
}

#' Calculate Percent Change
#'
#' Calculate the percentage change between two values.
#'
#' @param old The original value.
#' @param new The new value.
#' @param digits Number of decimal places for the result.
#'
#' @return The percent change as a numeric value.
#' @export
#'
#' @examples
#' percent_change(100, 125)  # 25% increase
#' percent_change(100, 80)   # -20% decrease
percent_change <- function(old, new, digits = 2) {
  change <- ((new - old) / old) * 100
  round(change, digits)
}

#' Describe a Vector
#'
#' Print a human-readable description of a numeric vector.
#'
#' @param x A numeric vector.
#' @param name Optional name for the variable.
#'
#' @return Invisibly returns the summary statistics.
#' @export
#'
#' @examples
#' describe_vector(mtcars$mpg, "Miles per Gallon")
describe_vector <- function(x, name = "Variable") {
  stats <- quick_summary(x)

  cat(sprintf("\n=== %s ===\n", name))
  cat(sprintf("  Count:  %s\n", format_number(stats$n, 0)))
  cat(sprintf("  Mean:   %s\n", format_number(stats$mean)))
  cat(sprintf("  Median: %s\n", format_number(stats$median)))
  cat(sprintf("  Std Dev: %s\n", format_number(stats$sd)))
  cat(sprintf("  Range:  %s to %s\n", format_number(stats$min), format_number(stats$max)))
  cat(sprintf("  IQR:    %s to %s\n", format_number(stats$q25), format_number(stats$q75)))

  invisible(stats)
}

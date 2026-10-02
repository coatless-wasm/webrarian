# The package's own NAMESPACE exports. getNamespaceExports() is no use under
# devtools::load_all(), which exports everything.
namespace_exports <- function() {
  ns <- readLines(system.file("NAMESPACE", package = "webrarian"), warn = FALSE)
  sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
}

# A function's formals as text: "" for an argument with no default.
formals_text <- function(f) {
  args <- formals(f)
  if (is.null(args)) {
    return(character())
  }
  vapply(args, function(x) paste(deparse(x), collapse = " "), character(1))
}

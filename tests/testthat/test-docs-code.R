# R code in the documentation parses, calls only webrarian's exported
# functions, and passes them arguments they have (reading_room(open = FALSE),
# collection_mirror(overwrite = , path = ), catalog_files(resolved = TRUE) and
# friends). Keys given to settings_set()
# and settings_get() must be real settings.

webrarian_vocabulary <- paste0(
  "^(catalog|acquire_|withdraw_|collection_|settings_|bind$|reading_room|circulat|",
  "clean_shelves|check_|diagnose_|webr_|is_collection|read_brand|watch_)"
)

calls_in <- function(exprs) {
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible())
    }
    found[[length(found) + 1L]] <<- e
    for (i in seq_along(e)) {
      if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
  }
  for (e in exprs) {
    walk(e)
  }
  found
}

call_problems <- function(call, exports) {
  fn <- call[[1]]
  pkg <- NULL
  op <- NULL
  if (is.symbol(fn)) {
    name <- as.character(fn)
  } else if (is.call(fn) && is.symbol(fn[[1]]) && as.character(fn[[1]]) %in% c("::", ":::")) {
    op <- as.character(fn[[1]])
    pkg <- as.character(fn[[2]])
    name <- as.character(fn[[3]])
  } else {
    return(character())
  }
  if (!is.null(pkg) && !identical(pkg, "webrarian")) {
    return(character())
  }
  if (identical(op, ":::")) {
    return(sprintf("webrarian:::%s() is internal", name))
  }
  if (is.null(pkg) && !grepl(webrarian_vocabulary, name)) {
    return(character())
  }
  if (!name %in% exports) {
    return(sprintf("%s() is not exported by webrarian", name))
  }
  f <- webrarian_function(name)
  matched <- tryCatch(match.call(f, call), error = function(e) e)
  if (inherits(matched, "error")) {
    return(sprintf("%s: %s", paste(deparse(call), collapse = " "), conditionMessage(matched)))
  }
  keys <- switch(
    name,
    settings_set = setdiff(names(as.list(call)[-1]), c("", "path")),
    settings_get = if (is.character(matched$key)) matched$key,
    character()
  )
  unlist(lapply(keys, setting_key_problem))
}

for (file in doc_sources()) {
  if (!any(vapply(fenced_blocks(file), function(b) identical(b$lang, "r"), logical(1)))) {
    next
  }
  test_that(paste("R code in", file, "uses the exported API correctly"), {
    skip_if_no_source_tree()
    exports <- namespace_exports()
    for (block in fenced_blocks(file)) {
      if (!identical(block$lang, "r")) {
        next
      }
      where <- sprintf("%s:%d", file, block$line)
      exprs <- tryCatch(parse(text = block$lines, keep.source = FALSE), error = function(e) e)
      expect_false(inherits(exprs, "error"), info = paste(where, "does not parse"))
      if (inherits(exprs, "error")) {
        next
      }
      problems <- as.character(unlist(lapply(calls_in(exprs), call_problems, exports = exports)))
      expect_identical(problems, character(), info = where)
    }
  })
}

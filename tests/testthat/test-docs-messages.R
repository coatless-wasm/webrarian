# The messages the documentation quotes are messages webrarian prints. A quoted
# message is a ```{.text .message} block; <placeholders> stand for the parts
# that vary, and every fixed part must appear in a string in R/.

# R source text with cli markup reduced to what the user sees, for matching
# the messages the documentation quotes: a glue interpolation or a sprintf()
# slot becomes "<>", {.fn x} becomes x(), other inline markup keeps its text,
# {?a/b} keeps the singular form, and quotes, backticks and backslashes go.
cli_plain <- function(x) {
  x <- gsub("\\{\\?([^/{}]*)/[^{}]*\\}", "\\1", x)
  x <- gsub("\\{\\?[^{}]*\\}", "", x)
  repeat {
    y <- gsub("\\{[^.?{}][^{}]*\\}", "<>", x)
    y <- gsub("\\{\\.fn ([^{}]*)\\}", "\\1()", y)
    y <- gsub("\\{\\.[a-z]+ ([^{}]*)\\}", "\\1", y)
    if (identical(y, x)) {
      break
    }
    x <- y
  }
  x <- gsub("%[sd]", "<>", x)
  x <- gsub("[\"'`\\\\]", "", x)
  gsub("\\s+", " ", x)
}

# The fixed parts of a quoted message line: the text between <placeholders>,
# without quotes, backticks or cli's status symbols, keeping parts of at least
# ten characters (shorter ones match anything).
message_fragments <- function(line) {
  line <- sub(
    "^\\s*(Error( in [^:]*)?:|Warning( in [^:]*)?:|[!x>i\u2716\u2139\u2022*-])\\s+",
    "",
    line
  )
  line <- gsub("[\"'`\\\\]", "", line)
  parts <- trimws(strsplit(line, "<[^>]+>")[[1]])
  parts[nchar(parts) >= 10L]
}

r_message_text <- function() {
  files <- list.files(repo_path("R"), pattern = "\\.R$", full.names = TRUE)
  strings <- unlist(
    lapply(files, function(f) {
      data <- utils::getParseData(parse(f, keep.source = TRUE))
      # getParseText(), not data$text, which abbreviates long strings.
      literals <- utils::getParseText(data, data$id[data$token == "STR_CONST"])
      vapply(literals, function(s) eval(parse(text = s, keep.source = FALSE)[[1]]), character(1))
    }),
    use.names = FALSE
  )
  # Each literal on its own: a brace in one string must never pair with a
  # brace in another.
  paste(cli_plain(strings), collapse = "\n")
}

test_that("cli_plain() reduces cli markup to the text a user sees", {
  expect_identical(
    cli_plain("{cli::qty(n)}Include pattern{?s} {.val {x}} matched no files."),
    "<>Include pattern <> matched no files."
  )
  expect_identical(
    cli_plain("Config key{?s} {.field {k}} use{?s/} an underscore."),
    "Config key <> uses an underscore."
  )
  expect_identical(
    cli_plain("it was not created by {.fn webrarian::bind}."),
    "it was not created by webrarian::bind()."
  )
  expect_identical(cli_plain("`%s` is not a webrarian setting"), "<> is not a webrarian setting")
  expect_identical(
    message_fragments("! Include pattern \"<pattern>\" matched no files."),
    c("Include pattern", "matched no files.")
  )
})

test_that("every quoted message is one webrarian prints", {
  skip_if_no_source_tree()
  skip_if_not(dir.exists(repo_path("R")), "R/ is not in this tree")
  source <- r_message_text()
  for (file in doc_sources()) {
    for (block in fenced_blocks(file)) {
      if (!"message" %in% block$classes) {
        next
      }
      for (line in block$lines) {
        for (fragment in message_fragments(line)) {
          expect_true(
            grepl(fragment, source, fixed = TRUE),
            info = sprintf(
              "%s:%d quotes \"%s\", which no string in R/ contains",
              file,
              block$line,
              fragment
            )
          )
        }
      }
    }
  }
})

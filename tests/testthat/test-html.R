# User-supplied config (title, description) and file names are written into the
# generated index.html / init JavaScript. They must be HTML-escaped / emitted as
# JSON so a value containing a quote or </script> cannot break or inject into the
# deployed static site.

test_that("html_escape escapes the five HTML metacharacters", {
  expect_equal(html_escape("a & b"), "a &amp; b")
  expect_equal(html_escape("<b>"), "&lt;b&gt;")
  expect_equal(html_escape('say "hi"'), "say &quot;hi&quot;")
  expect_equal(html_escape("it's"), "it&#39;s")
  expect_equal(html_escape("plain"), "plain")
})

test_that("generate_loading_html escapes the title, subtitle, and message", {
  html <- generate_loading_html(
    loading_title = "T<i>",
    loading_subtitle = "Name & <script>alert(1)</script>",
    loading_message = 'msg "q"'
  )
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert", html, fixed = TRUE))
  expect_true(grepl("Name &amp; ", html, fixed = TRUE))
  expect_true(grepl("T&lt;i&gt;", html, fixed = TRUE))
  expect_true(grepl("msg &quot;q&quot;", html, fixed = TRUE))
})

test_that("generate_loading_html custom_html replaces the splash contents inside its container", {
  # ui.loading.custom-html is the author's own markup, as in pyodidarian. It stays
  # inside the container the dismiss script hides, above the status line a dead
  # viewer reports into. The markup itself is not escaped.
  custom <- '<div class="my-loader"><h3>Loading the lab</h3></div>'
  expect_equal(
    generate_loading_html(loading_title = "Lab", custom_html = custom),
    paste0('<div id="webrarian-loading">', custom, '<div id="webrarian-status"></div></div>')
  )
})

test_that("generate_loading_dismiss_script hides the splash on mount and reports a dead viewer", {
  s <- generate_loading_dismiss_script()
  expect_match(s, 'getElementById\\("webrarian-loading"\\)')
  expect_match(s, 'getElementById\\("root"\\)')
  expect_match(s, "MutationObserver")
  expect_match(s, 'classList.add\\("hidden"\\)')
  expect_match(s, "childList")
  # A module script that fails to load fires `error` on the element, which
  # does not bubble: only a capturing listener on window sees it.
  expect_match(s, 'addEventListener("error"', fixed = TRUE)
  expect_match(s, "}, true);", fixed = TRUE)
  expect_match(s, "setTimeout(", fixed = TRUE)
  expect_match(s, "60000", fixed = TRUE)
  expect_match(s, "Reload", fixed = TRUE)
  # A late mount after the soft timeout clears the error state again.
  expect_match(s, 'removeAttribute("data-state")', fixed = TRUE)
  expect_match(generate_loading_dismiss_script(5), "5000", fixed = TRUE)
})

# --- the loading screen follows the scheme in force ---
#
# Light is what it always was. Dark applies when the system is dark and the
# page is not forced light, or when the page is forced dark (data-theme on
# <html>, from the viewer's Settings gear or a pinned site).

test_that("the loading screen's light colors are unchanged", {
  css <- generate_loading_styles(list())
  expect_match(css, "background: #ffffff;", fixed = TRUE)
  expect_match(css, "border: 4px solid #e0e0e0;", fixed = TRUE)
  expect_match(css, "#webrarian-subtitle { color: #666666;", fixed = TRUE)
  branded <- generate_loading_styles(list(background_color = "#F8F1E0", text_color = "#62291F"))
  expect_match(branded, "background: #F8F1E0;", fixed = TRUE)
  expect_match(branded, "#webrarian-subtitle { color: #62291F;", fixed = TRUE)
})

test_that("the loading screen has a dark variant under both selectors", {
  css <- generate_loading_styles(list(spinner_color = "#B84130"))
  system_dark <- ":where(:root:not([data-theme=\"light\"]))"
  forced_dark <- ":where(:root[data-theme=\"dark\"])"
  expect_match(
    css,
    paste0(
      "@media (prefers-color-scheme: dark) { ",
      system_dark,
      " #webrarian-loading { background: #1e1e1e; }"
    ),
    fixed = TRUE
  )
  for (root in c(system_dark, forced_dark)) {
    expect_match(css, paste0(root, " #webrarian-loading { background: #1e1e1e; }"), fixed = TRUE)
    # The track goes dark; the spinner keeps the brand's color.
    expect_match(
      css,
      paste0(root, " .webrarian-spinner { border-color: #444444; border-top-color: #B84130; }"),
      fixed = TRUE
    )
    expect_match(css, paste0(root, " #webrarian-status { color: #aaaaaa; }"), fixed = TRUE)
  }
})

test_that("a site whose brand is dark in both schemes gets no dark variant", {
  css <- generate_loading_styles(
    list(background_color = "#101820", text_color = "#f2f2f2"),
    dark_variant = FALSE
  )
  expect_no_match(css, "data-theme", fixed = TRUE)
  expect_no_match(css, "prefers-color-scheme", fixed = TRUE)
  expect_match(css, "background: #101820;", fixed = TRUE)
})

test_that("the page applies a stored choice before it draws anything", {
  script <- generate_theme_init_script()
  # A function of its own, as the dismiss script is: its names stay off window.
  expect_match(script, "<script>(function () {", fixed = TRUE)
  expect_match(script, "})();</script>", fixed = TRUE)
  # The key the viewer stores the choice under (exlibris, ARCHITECTURE.md).
  expect_match(
    script,
    "\"exlibris-theme:\" + location.origin + location.pathname.replace(/\\/index\\.html$/, \"/\")",
    fixed = TRUE
  )
  expect_match(script, "stored === \"light\" || stored === \"dark\"", fixed = TRUE)
  # A pinned page's own attribute is left alone, and refused storage is survived.
  expect_match(script, "if (!root.hasAttribute(\"data-theme\"))", fixed = TRUE)
  expect_match(script, "catch (e) {}", fixed = TRUE)
})

# A rule in an author's custom CSS that restyles the loading screen beats the
# light rules (same weight, or more). The dark rules must weigh the same as
# the light ones, or they would take the screen back on a dark system.
test_that("the dark variant weighs no more than the light rules it follows", {
  css <- generate_loading_styles(list())
  dark <- sub("^.*@media \\(prefers-color-scheme: dark\\)", "", css)
  selectors <- regmatches(dark, gregexpr("[^{};]*:root[^{]*\\{", dark))[[1]]
  expect_gt(length(selectors), 0L)
  # Every one starts inside :where(), never on a bare :root.
  expect_true(
    all(grepl("^[\\s{]*:where\\(:root", selectors, perl = TRUE)),
    info = paste(selectors, collapse = " | ")
  )
  expect_false(
    any(grepl("(^|[\\s{,]):root", selectors, perl = TRUE)),
    info = paste(selectors, collapse = " | ")
  )
})

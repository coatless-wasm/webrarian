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

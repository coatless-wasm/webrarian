# chromote helpers shared by the browser tests. Callers skip first, as
# test-browser.R does: skip_on_cran(), skip_if_offline(),
# skip_if_not_installed("chromote"), and a check for a usable Chrome.

local_browser_page <- function(url, env = parent.frame()) {
  b <- chromote::ChromoteSession$new()
  withr::defer(b$close(), envir = env)

  # Collect console.error, uncaught errors and unhandled rejections before any
  # page script runs, so a boot failure surfaces as text, not a bare timeout.
  b$Page$enable()
  b$Page$addScriptToEvaluateOnNewDocument(
    source = paste0(
      "window.__consoleErrors__ = [];",
      "window.addEventListener('error', function(e){ window.__consoleErrors__.push(String(e.message)); });",
      "window.addEventListener('unhandledrejection', function(e){ window.__consoleErrors__.push('unhandledrejection: ' + String(e.reason)); });",
      "(function(){ var o = console.error; console.error = function(){ window.__consoleErrors__.push(Array.prototype.map.call(arguments, String).join(' ')); return o.apply(console, arguments); }; })();"
    )
  )

  page <- list(session = b, url = url)
  page$navigate <- function(to = url) {
    load_fired <- b$Page$loadEventFired(wait_ = FALSE)
    b$Page$navigate(to, wait_ = FALSE)
    b$wait_for(load_fired)
    invisible(TRUE)
  }
  page$eval <- function(expr) {
    b$Runtime$evaluate(expr, awaitPromise = TRUE)$result$value
  }
  page$wait_for <- function(expr, timeout = 120) {
    deadline <- Sys.time() + timeout
    repeat {
      if (isTRUE(page$eval(expr))) {
        return(TRUE)
      }
      if (Sys.time() > deadline) {
        return(FALSE)
      }
      Sys.sleep(0.5)
    }
  }
  page$terminal_text <- function() {
    page$eval(paste0(
      "((document.querySelector('.xterm-rows') || ",
      "document.querySelector('.terminal-container') || ",
      "document.body).innerText || '')"
    ))
  }
  page$errors <- function() {
    jsonlite::fromJSON(page$eval("JSON.stringify(window.__consoleErrors__ || [])"))
  }
  page
}

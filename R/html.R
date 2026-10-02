# Shared HTML generation for webR REPL
#
# Used by both bind() and collection_mirror() to generate
# consistent index.html files with proper webR initialization.

#' Generate loading screen styles
#'
#' @param theme List with optional `background_color`, `text_color`, `spinner_color`
#' @return CSS styles as a string
#' @noRd
generate_loading_styles <- function(theme = list()) {
  bg_color <- css_color(theme$background_color, "brand.color.background") %||% "#ffffff"
  text_color <- css_color(theme$text_color, "brand.color.foreground") %||% "#666666"
  spinner_color <- css_color(theme$spinner_color, "brand.color.primary") %||% "#2196F3"

  sprintf(
    '
    #webrarian-loading {
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: %s;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      z-index: 10000;
      font-family: system-ui, -apple-system, sans-serif;
    }
    #webrarian-loading.hidden { display: none; }
    .webrarian-spinner {
      width: 50px; height: 50px;
      border: 4px solid #e0e0e0;
      border-top-color: %s;
      border-radius: 50%%;
      animation: spin 1s linear infinite;
      margin-bottom: 20px;
    }
    @keyframes spin { to { transform: rotate(360deg); } }
    #webrarian-title { color: %s; font-size: 14px; font-weight: 500; text-transform: uppercase; letter-spacing: 2px; margin-bottom: 30px; opacity: 0.7; }
    #webrarian-subtitle { color: %s; font-size: 28px; font-weight: 600; margin-bottom: 30px; }
    #webrarian-status { color: %s; font-size: 14px; opacity: 0.8; }
  ',
    bg_color,
    spinner_color,
    text_color,
    text_color,
    text_color
  )
}

#' Generate loading screen HTML
#'
#' @param loading_title Main title text (e.g., "Opening Webrarian Environment")
#' @param loading_subtitle Secondary text (e.g., project name). NULL to hide.
#' @param logo_html Optional logo HTML string
#' @param loading_message Status message (default "Loading webR...")
#' @param custom_html Optional custom HTML that replaces the splash's contents
#'   (title, subtitle, logo, spinner and message). It is placed, unescaped,
#'   inside the `#webrarian-loading` container above an empty
#'   `#webrarian-status` line, so the dismiss script still hides it on mount
#'   and can report a viewer that failed to load.
#' @return HTML string for loading screen
#' @noRd
generate_loading_html <- function(
  loading_title = "Opening Webrarian Environment",
  loading_subtitle = NULL,
  logo_html = "",
  loading_message = "Loading webR...",
  custom_html = NULL
) {
  # Custom HTML replaces the contents, not the container the dismiss script needs
  if (!is.null(custom_html)) {
    return(paste0(
      '<div id="webrarian-loading">',
      custom_html,
      '<div id="webrarian-status"></div></div>'
    ))
  }

  # Build subtitle HTML
  subtitle_html <- if (!is.null(loading_subtitle) && nzchar(loading_subtitle)) {
    sprintf('<div id="webrarian-subtitle">%s</div>', html_escape(loading_subtitle))
  } else {
    ""
  }

  sprintf(
    '
  <div id="webrarian-loading">
    <div id="webrarian-title">%s</div>
    %s
    %s
    <div class="webrarian-spinner"></div>
    <div id="webrarian-status">%s</div>
  </div>
  ',
    html_escape(loading_title),
    subtitle_html,
    logo_html,
    html_escape(loading_message)
  )
}

#' Generate the inline script that dismisses the pre-boot loading splash
#'
#' The vendored exlibris viewer renders into #root and shows its own loading
#' overlay once it mounts; it knows nothing about webrarian's
#' #webrarian-loading splash. This observer hides the splash as soon as
#' exlibris mounts anything into #root. If the viewer's module script fails
#' to load, the spinner is replaced by a message and a Reload button for good.
#' If nothing has mounted after `timeout_seconds`, the same message appears,
#' but the observer keeps watching: on a slow connection the bundle can still
#' arrive, and its mount clears the error and hides the splash. A broken
#' deployment never looks like a slow one, and a slow one is never hidden
#' behind an error. Emitted as a classic (non-module) inline script placed
#' before the module script, so both listeners are in place before the bundle
#' loads.
#' @param timeout_seconds Seconds to wait for the viewer to mount before
#'   saying so.
#' @return HTML `<script>` string
#' @noRd
generate_loading_dismiss_script <- function(timeout_seconds = 60) {
  timeout_ms <- format(as.integer(timeout_seconds * 1000), scientific = FALSE)
  paste0(
    '<script>
  (function () {
    var splash = document.getElementById("webrarian-loading");
    var root = document.getElementById("root");
    if (!splash || !root) return;
    var mounted = false;
    var failed = false;
    var obs = null;
    function dismiss() {
      if (mounted) return;
      mounted = true;
      if (obs) obs.disconnect();
      splash.removeAttribute("data-state");
      splash.classList.add("hidden");
    }
    function showError(message) {
      if (mounted) return;
      splash.setAttribute("data-state", "error");
      var spinner = splash.querySelector(".webrarian-spinner");
      if (spinner) spinner.style.display = "none";
      var status = document.getElementById("webrarian-status");
      if (!status) return;
      status.textContent = message + " ";
      var retry = document.createElement("button");
      retry.type = "button";
      retry.textContent = "Reload";
      retry.addEventListener("click", function () { location.reload(); });
      status.appendChild(retry);
    }
    if (root.childElementCount > 0) { dismiss(); return; }
    obs = new MutationObserver(function () {
      if (root.childElementCount > 0) dismiss();
    });
    obs.observe(root, { childList: true });
    // The viewer bundle itself failed to load: terminal.
    window.addEventListener("error", function (event) {
      var el = event.target;
      if (!mounted && el && el.tagName === "SCRIPT" && el.type === "module") {
        failed = true;
        obs.disconnect();
        showError("The viewer (" + (el.getAttribute("src") || "script") + ") could not be loaded.");
      }
    }, true);
    // Soft: say so, but keep observing, so a late mount still takes over.
    setTimeout(function () {
      if (!mounted && !failed) {
        showError("The viewer is taking longer than expected to start. Check your connection, or reload.");
      }
    }, ',
    timeout_ms,
    ');
  })();
  </script>'
  )
}

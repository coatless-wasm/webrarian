# Vignettes build without a browser. Quarto renders a ```{mermaid} block with
# headless Chrome: an ERROR where Quarto finds no Chrome, a detritus NOTE and
# hangs where it does. The figures are SVG files written by hand instead, and
# committed as they are.

vignette_files <- function() {
  list.files(repo_path("vignettes"), pattern = "\\.qmd$", full.names = TRUE)
}

figure_files <- function() {
  c(
    list.files(repo_path("vignettes", "images"), pattern = "\\.svg$", full.names = TRUE),
    repo_path("man", "figures", c("hero-light.svg", "hero-dark.svg"))
  )
}

test_that("no vignette asks Quarto to render a diagram", {
  skip_if_no_source_tree()
  for (f in vignette_files()) {
    text <- readLines(f, warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("^\\s*```\\s*\\{\\s*(mermaid|dot)", text)), info = basename(f))
    expect_false(any(grepl("mermaid-format", text, fixed = TRUE)), info = basename(f))
  }
})

# An <img> shows an SVG in isolation: no page fonts, no scripts and no other
# files, so each figure must stand on its own.
test_that("every figure is a self-contained, labeled SVG", {
  skip_if_no_source_tree()
  for (path in figure_files()) {
    name <- basename(path)
    expect_true(file.exists(path), info = name)
    drawing <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(drawing, '<svg xmlns="http://www.w3.org/2000/svg"', fixed = TRUE, info = name)
    expect_match(drawing, 'role="img" aria-labelledby="t d"', fixed = TRUE, info = name)
    expect_match(drawing, '<title id="t">[^<]+</title>', info = name)
    expect_match(drawing, '<desc id="d">[^<]+</desc>', info = name)
    for (banned in c("<script", "<foreignObject", "<image", "@import", "@font-face", "url(http")) {
      expect_false(grepl(banned, drawing, fixed = TRUE), info = paste(name, "uses", banned))
    }
    # Links point only inside the file (markers, patterns, outlined glyphs).
    hrefs <- regmatches(drawing, gregexpr('href="[^"]*"', drawing))[[1]]
    expect_true(all(startsWith(hrefs, 'href="#')), info = name)
    expect_lt(file.size(path), 40 * 1024)
  }
})

test_that("every vignette figure carries its own name as its id", {
  skip_if_no_source_tree()
  for (path in list.files(
    repo_path("vignettes", "images"),
    pattern = "\\.svg$",
    full.names = TRUE
  )) {
    name <- sub("\\.svg$", "", basename(path))
    drawing <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(drawing, sprintf('id="webrarian-%s"', name), fixed = TRUE, info = name)
  }
})

test_that("every image a vignette shows exists and has alt text", {
  skip_if_no_source_tree()
  for (f in vignette_files()) {
    text <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    refs <- regmatches(text, gregexpr("!\\[[^]]*\\]\\([^)]+\\)(\\{[^}]*\\})?", text))[[1]]
    for (ref in refs) {
      p <- sub("^!\\[[^]]*\\]\\(([^) ]+).*$", "\\1", ref)
      expect_true(file.exists(repo_path("vignettes", p)), info = paste(basename(f), p))
      expect_match(ref, 'fig-alt="[^"]{40,}"', info = paste(basename(f), p))
    }
  }
})

test_that("CI rebuilds the vignettes with no browser to fall back on", {
  skip_if_no_source_tree()
  path <- repo_path(".github", "workflows", "R-CMD-check.yaml")
  skip_if_not(file.exists(path), ".github/ is not in this tree")
  job <- yaml::read_yaml(path)$jobs[["R-CMD-check"]]
  expect_identical(job$env$QUARTO_CHROMIUM, "/usr/bin/false")
})

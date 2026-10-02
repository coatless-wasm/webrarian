# Branded Example — "DataViz Pro"

This example demonstrates **custom branding** for a webR site built with
webrarian. It shows how the `ui` and `brand` sections of `_webrarian.yml`
control the look and feel of a deployed collection: a custom CSS file, an
SVG logo and favicon, brand colors, Google-font typography, and custom page
metadata (title, description, loading message).

No compiled packages are used — the example runs entirely on base R inside
webR, so it builds without Docker. It plots a small in-memory data frame to
give the branded page something to show.

## Files

| File | Role |
|------|------|
| `_webrarian.yml` | Collection config; `ui` + `brand` sections drive the custom look |
| `setup.R` | Auto-run at startup; creates the `sample_data` data frame |
| `visualizations.R` | Helper plotting functions (`create_bar_chart()`, `create_pie_chart()`, `plot_growth()`) |
| `assets/custom.css` | Custom stylesheet referenced by `ui.custom-css` (scrollbars, selection, terminal styling) |
| `assets/logo.svg` | Wide brand logo referenced by `brand.logo.images.wide` |
| `assets/favicon.svg` | Icon / favicon referenced by `brand.logo.images.icon` |

## What to look for

- `ui.custom-css` injects `assets/custom.css` into the page.
- `ui.loading.message` and `ui.meta` set the loading text and HTML `<head>` metadata.
- `brand.color`, `brand.typography`, and `brand.logo` follow the
  [brand.yml](https://posit-dev.github.io/brand-yml/brand/) standard to theme
  colors, fonts, and logos.
- `setup.R` runs automatically (`repl.auto-run`), so `sample_data` is
  ready when the REPL opens; call the functions in `visualizations.R` to plot.

## Build it

```r
path <- webrarian::collection_example("branded", dest = "branded-example")
webrarian::bind(path)          # build the static site (no Docker required)
webrarian::reading_room(path)  # preview it locally
```

# Local Package Example

This example demonstrates compiling a **local R package** to WebAssembly and
bundling it alongside CRAN packages, data, and a demo script into a deployable
webR site.

The local package `demotools` (in `packages/demotools/`) is compiled to Wasm via
Docker during `bind()`, then loaded inside the browser REPL just like the
pre-built CRAN packages (`dplyr`, `ggplot2`). The demo script reads a bundled CSV,
summarizes it with `demotools` functions, and renders a ggplot.

## Requirements

**Docker is required.** Local packages are compiled inside the versioned webR
image (`ghcr.io/r-wasm/webr:v0.6.0`). Pre-built CRAN packages are downloaded, but
`demotools` must be compiled locally.

## Files

- `_webrarian.yml` - Collection config (prebuilt `dplyr`/`ggplot2`, local
  `demotools`, auto-run `demo.R`).
- `demo.R` - Auto-run script: loads data, calls `demotools::describe_vector()`,
  plots revenue vs. expenses with ggplot2.
- `data/metrics.csv` - Sample monthly revenue/expenses/customers data.
- `packages/demotools/` - The local package compiled to Wasm. Exports
  `quick_summary()`, `format_number()`, `percent_change()`, `describe_vector()`.

## Build it

```r
path <- webrarian::collection_example("local-package", dest = "local-package-example")
webrarian::bind(path)          # requires a running Docker to compile demotools
webrarian::reading_room(path)  # preview the built site locally
```

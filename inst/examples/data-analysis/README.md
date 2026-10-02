# Data Analysis Example

Bundles several **prebuilt packages** (WebAssembly binaries from
`repo.r-wasm.org`) into one webR site. Nothing is compiled, so no Docker is
needed: `dplyr`, `ggplot2` and `scales` are downloaded as ready-made binaries.

It also shows a multi-file analysis that runs when the page opens: two scripts
and a CSV file are placed in the webR file system and run in order.

## Files

- `_webrarian.yml` — collection config. Lists the three prebuilt packages,
  includes `*.R` and the `data/` folder, and sets `repl.auto-run` to run
  `01-data-munging.R` and then `02-visualization.R` when the page opens.
- `01-data-munging.R` — reads `data/sales.csv` and summarizes revenue by region
  and by product using dplyr.
- `02-visualization.R` — plots revenue by region with ggplot2, using `scales`
  for dollar-formatted axis labels.
- `demo.R` — runs both scripts again with their output echoed.
- `data/sales.csv` — sample sales dataset (region, product, units, revenue).

## Build it

```r
path <- webrarian::collection_example("data-analysis", dest = "data-analysis-example")
webrarian::bind(path)          # download the WebAssembly packages and assemble the site
webrarian::reading_room(path)  # preview it locally
```

`bind()` downloads the webR engine and the packages over the network and copies
them into the site, but needs **no Docker**. With `bind(path, offline = TRUE)`
the site also makes no request to another origin when it runs. The site runs
webR 0.6.0 (R 4.6.0).

# Basic Example

The simplest possible webrarian collection: a couple of R scripts plus a data
file, and **no packages**. It demonstrates the Virtual File System (VFS) and the
in-browser editor without any WebAssembly package compilation, so it builds fast
and needs no Docker.

When bound, webrarian copies the R files and the `data/` directory into the
site's `vfs-files/`, and the page places them under `/home/web_user`. The
editor opens an R file when the page loads, and users can run the analysis
entirely in the browser.

## Files

| File | Role |
|------|------|
| `_webrarian.yml` | Collection config: webR 0.6.0, no packages, VFS include/exclude rules, REPL editor settings |
| `analysis.R` | Entry script: sources `helpers.R`, reads the CSV, plots a scatter with a fitted line |
| `helpers.R` | Helper functions (`summarize_data()`, `calc_correlation()`) sourced by the analysis |
| `data/sample.csv` | Small sample dataset (`x`, `y`, `group`) read by `analysis.R` |

## Build it

```r
path <- webrarian::collection_example("basic", dest = "basic-example")
webrarian::bind(path)          # bundle the scripts and data into a site
webrarian::reading_room(path)  # preview it in the browser
```

`collection_example()` copies the example first, so the build never writes into
the installed package. The first build downloads the webR engine (about 40 MB)
into webrarian's cache; later builds reuse it. No Docker is needed: there are no
packages to compile.

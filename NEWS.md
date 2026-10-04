# webrarian 0.1.1

* **Editor**: with nothing selected, Run and Ctrl+Enter (Command+Enter on a
  Mac) run the whole statement the cursor is in, from any of its lines, then
  move the cursor to the next statement. They ran only the cursor's line
  before, so a call split over several lines had to be selected first (#1).
* **Editor**: code is colored with GitHub's syntax styles, light or dark with
  the visitor's color scheme.

# webrarian 0.1.0

First public release. webrarian builds static websites where R runs in the
visitor's browser through webR, with your packages installed and your files in
place.

The API is experimental before 1.0 and may change, with deprecation warnings
where practical.

This release covers these areas:

* **Collections**: `catalog()` starts a collection, configured by
  `_webrarian.yml` with lowercase, hyphenated keys (see
  `vignette("config-reference")`). `collection_example()` copies a working
  example.
* **Build and preview**: `bind()` builds the site, and `reading_room(watch = TRUE)`
  previews it and rebuilds on changes.
* **Packages**: `acquire_package()` adds prebuilt packages from repo.r-wasm.org
  and any `packages.repos`, and compiles local and GitHub packages with Docker
  (see `vignette("packages")`).
* **Files**: `acquire_file()` bundles scripts and data with `.gitignore`-style
  patterns (see `vignette("files")`).
* **Share links**: `repl.share-links` sets what a shared link may add to a
  site.
* **Branding**: a brand.yml file themes the workspace
  (see `vignette("customization")`).
* **Offline sites and mirrors**: `build.offline` makes a site that requests
  nothing from another server, and `collection_mirror()` builds a self-contained
  webR mirror (see `vignette("enterprise")`).
* **Deployment**: `circulate_via_github()` and `circulate_via_netlify()` write
  the workflows that publish a site (see `vignette("deployment")`).
* **Diagnostics**: `check_inventory()` reports which packages have WebAssembly
  builds, and `diagnose_collection()` checks Docker, settings and packages.
* **Licenses**: every site and mirror carries a `LICENSES/` directory for what
  it redistributes.

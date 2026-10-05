# webrarian 0.1.2

## New features

* The Files tab opens a file with a double-click or Enter (#3).

* The Files tab has a menu on each file with Open, Rename, Download and
  Delete. Open it with a right-click, the button at the end of the row, or
  Shift+F10. The toolbar keeps Refresh, Upload, New File and New Folder.

* The Files tab selects several files with Shift or Ctrl (Command on a Mac),
  to download as one zip, delete together or open together.

* New `ui.theme` setting pins a site to `light` or `dark`. With the default,
  `auto`, a Settings gear in the footer lets each visitor choose Auto, Light
  or Dark. A brand whose palette is dark is pinned to dark.

## Minor improvements and fixes

* A brand logo with `light` and `dark` variants shows each in its scheme on
  the loading screen.

* A loading screen with `ui.loading.custom-html` keeps its light colors.

* Custom CSS that targets dark mode with `@media (prefers-color-scheme: dark)`
  alone misses a visitor who chose Dark on a light system.
  `vignette("customization")` shows the rule to add.

* The scroll bars the browser draws, and HTML output other than help pages,
  keep following the visitor's system.

* The Files tab renames with F2, names new files and folders in place, and
  asks before a delete without the browser's pop-up boxes, so it works on a
  site embedded in a frame that blocks them.

* Renaming or deleting a file renames or closes its editor tab.

* The Files tab draws the lines that `tree` prints.

* The Files tab keeps the highlighted row when the list is refreshed.

* New File and renaming an editor tab no longer overwrite an existing file.

* A deleted file is no longer written back from its open tab.

* `vignette("deployment")` says where HTTP requests work: curl and httr2 need
  cross-origin isolation headers, so their requests time out on GitHub Pages
  (#2).

# webrarian 0.1.1

* Run and Ctrl+Enter (Command+Enter on a Mac) run the whole statement the
  cursor is in when nothing is selected, then move to the next statement (#1).

* The editor colors code with GitHub's syntax styles, light or dark with the
  visitor's color scheme.

# webrarian 0.1.0

First public release. webrarian builds static websites where R runs in the
visitor's browser through webR, with your packages installed and your files in
place.

The API is experimental before 1.0 and may change, with deprecation warnings
where practical.

* `catalog()` starts a collection, configured by `_webrarian.yml` with
  lowercase, hyphenated keys (see `vignette("config-reference")`).
  `collection_example()` copies a working example.

* `bind()` builds the site, and `reading_room(watch = TRUE)` previews it and
  rebuilds on changes.

* `acquire_package()` adds prebuilt packages from repo.r-wasm.org and any
  `packages.repos`, and compiles local and GitHub packages with Docker (see
  `vignette("packages")`).

* `acquire_file()` bundles scripts and data with `.gitignore`-style patterns
  (see `vignette("files")`).

* `repl.share-links` sets what a shared link may add to a site.

* A brand.yml file themes the workspace (see `vignette("customization")`).

* `build.offline` makes a site that requests nothing from another server, and
  `collection_mirror()` builds a self-contained webR mirror (see
  `vignette("enterprise")`).

* `circulate_via_github()` and `circulate_via_netlify()` write the workflows
  that publish a site (see `vignette("deployment")`).

* `check_inventory()` reports which packages have WebAssembly builds, and
  `diagnose_collection()` checks Docker, settings and packages.

* Every site and mirror carries a `LICENSES/` directory for what it
  redistributes.

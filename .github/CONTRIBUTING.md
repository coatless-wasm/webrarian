# Contributing to webrarian

Thank you for helping. This guide says how to report a problem, how to propose a
change, and how the package fits together.

## Reporting a problem

Open an issue at <https://github.com/coatless-wasm/webrarian/issues> and include:

- the output of `webrarian::diagnose_collection()` run in your collection
- your `_webrarian.yml`, and the call that failed with its full message
- for a problem in the browser, the browser and its version, where the site is
  served from (the preview, GitHub Pages, Netlify, ...) and the errors in the
  browser's console

Report a security problem privately instead, as described in
[the security policy](https://github.com/coatless-wasm/webrarian/security/policy).

## Proposing a change

Follow these steps to propose a change:

1. For anything larger than a typo, open an issue first so we can agree on the
   approach.
2. Fork the repository and branch from `main`.
3. Write a test that fails without your change (testthat 3, in
   `tests/testthat/`), then make it pass.
4. Run the checks under "Testing" and make sure they pass.
5. Add a bullet for your change to the top section of
   [NEWS.md](https://github.com/coatless-wasm/webrarian/blob/main/NEWS.md).
6. Open a pull request against `main`.

Commit messages are one sentence-case imperative line with no `feat:` or `fix:`
prefix, for example "Guard the output directory before cleaning". Stage the
files you changed by name, and never commit build output (`_site/`,
`*.Rcheck/`, tarballs).

The R code is formatted with [air](https://posit-dev.github.io/air/), using the
settings in `air.toml`. Run `air format .` before you commit, because CI fails on
code air would reformat.

The browser workspace in `inst/viewer/` is built from
[exlibris](https://github.com/coatless-wasm/exlibris). Change it there, then
re-vendor it with `tools/vendor-exlibris.sh`. On every run, CI checks the
vendored files against the hashes in `inst/viewer/PROVENANCE.json`. Where the
`EXLIBRIS_TOKEN` secret is set (exlibris is private for now), it also rebuilds
the pinned exlibris commit and fails if the vendored files differ. Without the
secret, that rebuild is skipped.

## Architecture

webrarian builds static sites where R runs in the browser through webR. It has
two halves:

- The R package (this repository) holds the collection model (`_webrarian.yml`),
  package resolution and download, Docker compilation of local and GitHub
  packages, file selection, the webR engine cache, the site build, previews,
  deployment helpers and mirrors.
- The browser workspace is the prebuilt exlibris bundle vendored in
  `inst/viewer/` (`exlibris-r.js`, `exlibris-r.css`, `index.html`, notices and
  `PROVENANCE.json`) plus `inst/viewer-config.schema.json`. webrarian never
  builds it. `tools/vendor-exlibris.sh` copies it from an exlibris checkout at a
  pinned commit and regenerates the notices from `PROVENANCE.json`.

The two meet at one object. `bind()` writes `window.__VIEWER_CONFIG__` inline
into the page (`R/viewer-config.R`, `build_viewer_config()`), with kebab-case
keys, and the tests validate real output against the vendored schema.

### Package sources

Packages come from two kinds of source:

- Prebuilt packages are WebAssembly binaries from repo.r-wasm.org, then each
  repository in `packages.repos` in order. The first repository with the
  package wins (`R/repo.R`).
- Local and GitHub packages are compiled in `ghcr.io/r-wasm/webr:v<webR version>`
  with Docker, then indexed into the site's `repo/`.

rwasm runs only inside the Docker container. Never load rwasm locally. The
`library(rwasm)` in `build_packages_rwasm_docker()` is part of a script written
for the container.

### The engine, package sources and offline sites

`build.bundle-engine: true` (the default) copies the webR engine into
`webr/v<version>/`, and `false` loads it from webr.r-wasm.org. A site that bundles
packages has them in `repo/`. At runtime the page installs from `./repo` when
the site bundles packages, then, unless the site is offline, from
repo.r-wasm.org and each repository in `packages.repos`
(`viewer_package_repos()`). exlibris's webR driver sets the `webr_pkg_repos`
option to the same list, so `install.packages()` in the console searches the
same places the boot installs do. `getOption("repos")` is not touched. An
inline script after the config makes every relative URL absolute against the
page, so sites work from a sub-path.

`build.offline` is strict and false by default, as in pyodidarian. `true`
means the deployed site makes no request to another origin. It implies a
bundled engine and sets the wire key `offline`, which exlibris honors. A
share link's `?packages=`, the Packages tab or `install.packages()` can add
only bundled packages, and anything else ends with a ✗ and a reason, never a
request. `collection_mirror()` output is always offline.

When `repo/` holds every package the site installs, `bind()` also writes them,
installed, as one gzipped tar image, `library/library-<hash>.tgz`
(`build_library_image()`), named in the wire key `packages.library-images`.
exlibris mounts it at `/exlibris/library/<n>` and installs nothing it holds,
falling back to `repo/` when it cannot mount it. `build.library-image: true`
is the default and `false` turns it off. The packages stay in `repo/` either
way, so runtime installs keep working.

### Files

Included files are copied one by one to `_site/vfs-files/<path>` and fetched
by the page into the mount point (`/home/web_user`), where R starts. There is
no archive.

## Key files

These files hold the main parts of the package:

- `R/build.R`: `bind()`, the staged build, engine install, viewer copy, cache headers
- `R/output.R`: output-directory guard, build marker, staging and swap
- `R/config.R`, `R/config-spec.R`: settings, the key table, validation
- `R/utils.R`: YAML read/write (kebab <-> snake keys), Docker probe, versions
- `R/project.R`: `catalog()`, its templates and dependency detection
- `R/files.R`: file selection (gitignore-style), `acquire_file()`
- `R/packages.R`, `R/repo.R`: package lists, repository indexes, verified downloads
- `R/library-image.R`: `build_library_image()` and `write_library_image()`, the
  mountable package library image (`library/library-<hash>.tgz`)
- `R/drift.R`: WebAssembly versions against CRAN and the local library
  (`package_drift()`, `report_package_drift()` writing `packages.json`, and the
  `webrarian.cran_repo` option)
- `R/viewer-config.R`, `R/html.R`, `R/css.R`, `R/brand.R`: the page, its config and branding
  (the same brand rule as pyodidarian: accent and fonts on `:root`, surface and text
  colors scoped to light mode unless the palette is dark)
- `R/preview.R`, `R/watch.R`: `reading_room()` on httpuv static paths, watch mode
- `R/mirror.R`, `R/licenses.R`: mirrors and the `LICENSES/` directory (the same file
  names as pyodidarian's)
- `R/deploy.R`: GitHub Pages and Netlify workflows (templates in `inst/templates/`). A
  generated `netlify.toml` holds only `[build] publish`, because `_headers` carries every
  header.
- `R/diagnostics.R`: `check_inventory()` (with the drift columns), `diagnose_collection()`
- `R/assets.R`: the webR engine cache, which keeps the three most recently used versions
- `R/examples.R`, `inst/examples/`: `collection_example()`
- `R/webrarian-package.R`: the package help page and the package options
- `tools/vendor-exlibris.sh`, `tools/notices/`: vendoring and notices
- `tools/config-reference.R`: regenerates `vignettes/config-reference.qmd` and
  `inst/templates/_webrarian.yml` from `config_spec()`
- `tools/measure-sizes.R`: the size table in `vignettes/getting-started.qmd`
- `tools/ci-test.R`: runs a slice of the test suite the way CI does

## Build flow

A build runs these steps in order:

1. `bind()` reads and validates `_webrarian.yml` and resolves the output directory.
2. It builds into `.webrarian/staging-*`.
3. Engine: cached download, verified, copied to `webr/v<version>/` (unless
   `build.bundle-engine` is false).
4. Packages: prebuilt downloads go through the checksum-verified cache, local
   and GitHub packages are compiled in Docker, and the `repo/` index is written.
5. Library image (when `repo/` holds every package the site installs, unless
   `build.library-image` is false): `build_library_image()` unpacks the
   binaries `repo/`'s index names and writes `library/library-<hash>.tgz`.
6. Files: selected and copied to `vfs-files/`.
7. Page: `index.html` with the inline config, content-hashed viewer bundle,
   branding, `_headers` (the only source of the COOP/COEP and cache headers,
   with `/library/*` immutable), `sw.js` (a kill switch that retires any
   earlier worker unless `build.service-worker` is true) and `LICENSES/`.
   Then `report_package_drift()` writes `packages.json` and names bundled
   packages older than on CRAN (it asks CRAN at most once a day and stays
   silent when it cannot reach it).
8. The staging directory replaces the output, and `bind()` returns a
   `webrarian_bind_result` with sizes by part (`library/` counts as packages).

## Testing

```bash
NOT_CRAN=true Rscript -e 'devtools::test()'                  # everything
NOT_CRAN=true Rscript -e 'devtools::test(filter = "docs")'   # documentation checks
NOT_CRAN=true Rscript tools/ci-test.R --filter 'browser|examples' --no-skips   # as CI runs the browser suite
R CMD build . && R CMD check --as-cran webrarian_*.tar.gz
```

Tests follow these rules:

- Point `R_USER_CACHE_DIR` at a scratch directory for anything that downloads.
  `WEBRARIAN_PACKAGE_CACHE` is an on/off switch, not a path.
- `tests/testthat/setup.R` sets `webrarian.cran_repo = FALSE`, so no test asks
  CRAN. A test that needs CRAN versions mocks `cran_package_versions()`.
- Docker paths use the fake `docker` in `tests/testthat/fixtures/fake-docker.sh`
  (`local_fake_docker()`). Real Docker runs only with `WEBRARIAN_TEST_DOCKER=true`,
  as CI's `docker` job does.
- Browser tests use chromote through `local_browser_page()`.

## Documentation

The documentation follows these rules:

- `README.md` is rendered from `README.qmd` (`quarto render README.qmd`).
- The vignettes run no code except the provenance chunk in `ecosystem.qmd`.
  Vignettes must build without a browser, so never add a `{mermaid}` block.
- The figures in `vignettes/images/` and the README's `man/figures/hero-*.svg`
  are SVG files written by hand, so edit them directly. Each one carries a
  `<title>` and a `<desc>`, draws its own background so it reads in both site
  themes, and uses only system fonts or outlined lettering. The two hero files
  differ only in their `<style>` line.
- After changing `config_spec()`, run `Rscript tools/config-reference.R`.
- Every YAML block in the docs starts with a `# <file name>` comment, and the
  `test-docs-*.R` tests validate the blocks, the R code and the quoted messages.
- `test-vignettes-persistence.R` pins the sections on library images, the drift report
  and Shiny (`packages.qmd`) and on kept edits and embedding
  (`customization.qmd`). Move them freely, but keep their headings.
- The pkgdown site is published only by `.github/workflows/pkgdown.yml`. To
  preview it, build into a scratch directory:
  `pkgdown::build_site(override = list(destination = tempfile("site")))`.

# Enterprise webR Mirror

This example builds a **self-contained webR mirror** with `collection_mirror()`:
a site for enterprise, self-hosted or air-gapped networks that makes **no
external requests** once built. The webR runtime (R 4.6.0), the chosen
WebAssembly packages and their dependencies are downloaded at build time,
verified, and written into one directory you can serve from any static host
that sends the cross-origin isolation headers webR needs (the mirror's
`_headers` file lists them).

Unlike the other examples this is a **script**, not a collection: there is no
`_webrarian.yml`.

## Files

| File | Role |
|------|------|
| `build-mirror.R` | Builds a mirror of `cli` and `glue` into the directory you name (default `./webr-mirror`), with a custom icon, then previews it in an interactive session. |
| `sample-favicon.svg` | Sample icon passed to `collection_mirror(favicon = )`. |

## Build it

From a shell, naming the directory to build into:

```sh
Rscript "$(Rscript -e 'cat(system.file("examples/mirror/build-mirror.R", package = "webrarian"))')" webr-mirror
```

Or call the function directly:

```r
favicon <- system.file("examples/mirror/sample-favicon.svg", package = "webrarian")
webrarian::collection_mirror("webr-mirror", packages = c("cli", "glue"), favicon = favicon)
webrarian::reading_room("webr-mirror")
```

## Notes

- The default `mode = "packages"` bundles the named packages and their
  dependencies. `mode = "full"` mirrors every package of every repository
  (tens of GB).
- No Docker is needed: the packages are prebuilt WebAssembly binaries.
- The mirror's share links are off by default (`share_links = "off"`); its
  `LICENSES/` directory records what it redistributes.

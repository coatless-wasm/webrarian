#!/usr/bin/env bash
# Vendor the prebuilt exlibris-r bundle and its license notices into this package.
#
# From a built checkout of exlibris (https://github.com/coatless-wasm/exlibris)
# this copies
#   dist/exlibris-r.js, dist/exlibris-r.css  -> inst/viewer/
#   dist/EXCEPTION.md                         -> inst/viewer/EXCEPTION.md
#   src/core/viewer-config.schema.json        -> inst/viewer-config.schema.json
# and writes, from the pin it records in inst/viewer/PROVENANCE.json,
#   inst/viewer/THIRD-PARTY.md   tools/notices/THIRD-PARTY-header.md.in, then
#                                exlibris's dist/THIRD-PARTY-r.md (which ends
#                                with the code nested inside bundled packages)
#   inst/viewer/LICENSE.webR.md  tools/notices/LICENSE.webR.md.in
#   LICENSE.note                 tools/notices/LICENSE.note.in
#   inst/viewer/index.html       the host page emit_viewer() patches
#
# Every shipped file is recorded, with its sha256, in PROVENANCE.json
# ("files"), and the dist/ notices it was built from in "sources", so
# tests/testthat/test-vendor.R and test-notices.R can check that the notices
# and the bytes agree with the pin.
#
# Usage:
#   tools/vendor-exlibris.sh [--ref <git-ref>] [--from-dist] [--allow-dirty] [path-to-exlibris-checkout]
#
# pyodidarian's tools/vendor-exlibris.sh shares --ref, --from-dist,
# --allow-dirty, EXLIBRIS_DIR, VENDOR_EXLIBRIS_REF and a destination variable
# (PYODIDARIAN_VENDOR_DIR, a vendor directory; here WEBRARIAN_VENDOR_ROOT, a
# repository root), and writes the same core PROVENANCE.json keys
# (exlibrisCommit, exlibrisRef, exlibrisVersion, exlibrisDirty, vendoredAt,
# files, sources), plus one engine-client key: webrClientVersion here,
# pyodideVersion there.
#
# `--from-dist <checkout>` and `--ref <ref> <checkout>` mean the same here and
# in pyodidarian's script: the first copies the checkout's built dist/, the
# second builds that ref from scratch and vendors the result, and both
# scripts refuse the two together. Only the bare form differs: run bare, this
# script copies the checkout's built dist/ (as --from-dist does), whereas
# pyodidarian's builds its DEFAULT_REF. So pass --from-dist or --ref
# explicitly in any command meant for both repositories. --allow-dirty means
# the same in both as well: it lets a copy of dist/ come from a dirty
# checkout, and both scripts refuse it (exit 2) in the mode that builds a
# clean tree, where it would do nothing. Here that is --ref; pyodidarian's
# refuses it without --from-dist, because its bare form builds a ref too.
# `--from-dist --allow-dirty <checkout>` therefore works in both.
#
# Options:
#   --ref <git-ref>        build that ref of the exlibris checkout in a
#                          throwaway detached worktree (npm ci, npm run build)
#                          and vendor from it; the ref is recorded as
#                          exlibrisRef. Without --ref, the checkout's own
#                          built dist/ is vendored.
#   --from-dist            vendor the checkout's own built dist/: the default
#                          mode, named so that a command means the same in
#                          both repositories; refused together with --ref
#   --allow-dirty          copy dist/ from a checkout with uncommitted
#                          changes, or from a directory that is not a git
#                          checkout (recorded as "exlibrisDirty": true);
#                          refused with --ref, whose worktree is always clean
#
# Environment:
#   EXLIBRIS_DIR           the exlibris checkout, if no argument is given
#   VENDOR_EXLIBRIS_REF    the ref to record instead (the commit recorded is
#                          always the one vendored)
#   WEBRARIAN_VENDOR_ROOT  the destination: write into this tree instead of
#                          the repository above this script (tests use it);
#                          templates are always read from the tools/notices/
#                          beside the script
#
# Without --ref, the exlibris checkout must be a git repo with a clean working
# tree; pass --allow-dirty to override. Rscript must be on PATH: exlibris's
# LICENSE is compared with R's own AGPL-3 text, which every generated site's
# LICENSES/exlibris.md carries. The vendored checkout must have run `npm ci`
# (--ref does that): the webR client version is read from node_modules/webr.
#
# IMPORTANT: emit_viewer() patches the generated index.html by matching these
# exact anchors, so keep them byte-for-byte if you edit the heredoc below:
#   - `<title>webrarian</title>`  -> replaced with the project title
#   - `</head>`                   -> ui head tags + loading <style> inserted before it
#   - `<body>`                    -> loading overlay markup inserted right after it
#   - `<script type="module"`     -> inline window.__VIEWER_CONFIG__ <script> inserted before it

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
NOTICES_DIR="$REPO_ROOT/tools/notices"
DEST_ROOT="${WEBRARIAN_VENDOR_ROOT:-$REPO_ROOT}"
VIEWER_DIR="$DEST_ROOT/inst/viewer"
SCHEMA_DEST="$DEST_ROOT/inst/viewer-config.schema.json"
LICENSE_NOTE="$DEST_ROOT/LICENSE.note"

usage() {
  echo "usage: tools/vendor-exlibris.sh [--ref <git-ref>] [--from-dist] [--allow-dirty] [path-to-exlibris-checkout]" >&2
}

ALLOW_DIRTY=0
BUILD_REF=""
REF_GIVEN=0
FROM_DIST=0
EXLIBRIS_ARG=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --allow-dirty)
      ALLOW_DIRTY=1
      shift
      ;;
    --from-dist)
      FROM_DIST=1
      shift
      ;;
    --ref)
      if [[ $# -lt 2 || -z "$2" ]]; then
        echo "error: --ref needs a git ref" >&2
        usage
        exit 2
      fi
      BUILD_REF="$2"
      REF_GIVEN=1
      shift 2
      ;;
    --ref=*)
      BUILD_REF="${1#--ref=}"
      REF_GIVEN=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      if [[ $# -gt 0 ]]; then
        EXLIBRIS_ARG="$1"
        shift
      fi
      ;;
    -*)
      echo "error: unknown option: $1" >&2
      usage
      exit 2
      ;;
    *)
      EXLIBRIS_ARG="$1"
      shift
      ;;
  esac
done

# --from-dist copies the checkout's own build and --ref builds another tree:
# the pair contradicts itself, so it is refused before the checkout is looked
# for, with the message and exit code pyodidarian's script uses.
if [[ "$REF_GIVEN" -eq 1 && "$FROM_DIST" -eq 1 ]]; then
  echo "error: --ref and --from-dist cannot be combined" >&2
  usage
  exit 2
fi

# --allow-dirty lets a copy of the checkout's dist/ come from a dirty
# checkout. --ref builds a clean worktree of the ref, where the flag would do
# nothing, so it is refused there rather than ignored, as pyodidarian's
# script refuses it without --from-dist.
if [[ "$REF_GIVEN" -eq 1 && "$ALLOW_DIRTY" -eq 1 ]]; then
  echo "error: --allow-dirty applies only when copying a checkout's dist/ (--from-dist, or the bare form here)" >&2
  usage
  exit 2
fi

if [[ -n "$EXLIBRIS_ARG" ]]; then
  CANDIDATES=("$EXLIBRIS_ARG")
elif [[ -n "${EXLIBRIS_DIR:-}" ]]; then
  CANDIDATES=("$EXLIBRIS_DIR")
else
  CANDIDATES=(
    "$REPO_ROOT/../exlibris"
    "$REPO_ROOT/../../exlibris"
    "$HOME/Documents/GitHub/exlibris"
  )
fi

# With --ref any git checkout will do (the ref is built from it); without,
# the checkout must already hold a build.
is_exlibris_checkout() {
  if [[ -n "$BUILD_REF" ]]; then
    git -C "$1" rev-parse --is-inside-work-tree >/dev/null 2>&1
  else
    [[ -f "$1/dist/exlibris-r.js" ]]
  fi
}

EXLIBRIS_DIR=""
for candidate in "${CANDIDATES[@]}"; do
  resolved="$(cd "$candidate" 2>/dev/null && pwd || true)"
  if [[ -n "$resolved" ]] && is_exlibris_checkout "$resolved"; then
    EXLIBRIS_DIR="$resolved"
    break
  fi
done

if [[ -z "$EXLIBRIS_DIR" ]]; then
  if [[ -n "$BUILD_REF" ]]; then
    echo "error: no exlibris git checkout at any of:" >&2
  else
    echo "error: could not find dist/exlibris-r.js in any of:" >&2
  fi
  for candidate in "${CANDIDATES[@]}"; do
    echo "  $candidate" >&2
  done
  echo "  Pass the checkout's path, build it first (npm run build), or use --ref <git-ref>." >&2
  exit 1
fi

# --- --ref: build that ref in a throwaway worktree ---------------------------

if [[ -n "$BUILD_REF" ]]; then
  if ! command -v npm >/dev/null 2>&1; then
    echo "error: --ref builds exlibris, which needs npm on PATH" >&2
    exit 1
  fi
  REF_COMMIT="$(git -C "$EXLIBRIS_DIR" rev-parse --verify --quiet "$BUILD_REF^{commit}" || true)"
  if [[ -z "$REF_COMMIT" ]]; then
    echo "error: $BUILD_REF is not a commit in $EXLIBRIS_DIR" >&2
    exit 1
  fi
  SOURCE_CHECKOUT="$EXLIBRIS_DIR"
  WORK_PARENT="$(mktemp -d)"
  BUILD_TREE="$WORK_PARENT/exlibris"
  remove_build_tree() {
    git -C "$SOURCE_CHECKOUT" worktree remove --force "$BUILD_TREE" >/dev/null 2>&1 || true
    git -C "$SOURCE_CHECKOUT" worktree prune >/dev/null 2>&1 || true
    rm -rf "$WORK_PARENT"
  }
  trap remove_build_tree EXIT
  git -C "$SOURCE_CHECKOUT" worktree add --detach "$BUILD_TREE" "$REF_COMMIT" >&2
  echo "Building exlibris $BUILD_REF (${REF_COMMIT:0:7}) in a temporary worktree" >&2
  (cd "$BUILD_TREE" && npm ci && npm run build) >&2
  EXLIBRIS_DIR="$BUILD_TREE"
fi

DIST_DIR="$EXLIBRIS_DIR/dist"
JS_SRC="$DIST_DIR/exlibris-r.js"
CSS_SRC="$DIST_DIR/exlibris-r.css"
THIRD_PARTY_SRC="$DIST_DIR/THIRD-PARTY-r.md"
EXCEPTION_SRC="$DIST_DIR/EXCEPTION.md"
LICENSE_SRC="$DIST_DIR/LICENSE"
SCHEMA_SRC="$EXLIBRIS_DIR/src/core/viewer-config.schema.json"

# --- check every input before anything is written ---------------------------

for required in "$JS_SRC" "$CSS_SRC" "$SCHEMA_SRC" "$THIRD_PARTY_SRC" "$EXCEPTION_SRC" "$LICENSE_SRC"; do
  if [[ ! -f "$required" ]]; then
    echo "error: $required not found" >&2
    echo "  exlibris's build writes exlibris-r.{js,css}, THIRD-PARTY-r.md, EXCEPTION.md" >&2
    echo "  and LICENSE into dist/; vendor from a commit whose build does." >&2
    exit 1
  fi
done
for template in THIRD-PARTY-header.md.in LICENSE.webR.md.in LICENSE.note.in; do
  if [[ ! -f "$NOTICES_DIR/$template" ]]; then
    echo "error: $NOTICES_DIR/$template is missing" >&2
    exit 1
  fi
done

# Every generated site's LICENSES/exlibris.md carries R's own copy of the
# AGPL-3 text, so exlibris's LICENSE must be exactly that text.
R_AGPL="$(Rscript --vanilla -e 'cat(file.path(R.home("share"), "licenses", "AGPL-3"))' 2>/dev/null || true)"
if [[ -z "$R_AGPL" || ! -f "$R_AGPL" ]]; then
  echo "error: could not find R's AGPL-3 text; is Rscript on PATH?" >&2
  exit 1
fi
if ! cmp -s "$LICENSE_SRC" "$R_AGPL"; then
  echo "error: $LICENSE_SRC is not the GNU AGPL v3 text R ships ($R_AGPL)" >&2
  exit 1
fi

# exlibris's generator (tools/third-party.ts) ends THIRD-PARTY-r.md with the
# code nested inside bundled packages, which esbuild's metafile cannot see:
# lie, immediate, setimmediate and pako 1.x inside jszip.min.js, and the
# jslinux notice (Fabrice Bellard) that xterm.js's stylesheet keeps. The
# generator checks those versions against node_modules; a build without the
# section predates it and would ship the viewer without those notices. webrarian keeps no copy of its own.
NESTED_HEADING="## Code nested inside bundled packages"
nested_start="$(grep -n -x -F "$NESTED_HEADING" "$THIRD_PARTY_SRC" | head -n 1 | cut -d: -f1 || true)"
if [[ -z "$nested_start" ]]; then
  echo "error: $THIRD_PARTY_SRC has no section for the code nested inside bundled packages" >&2
  echo "  exlibris's generator (tools/third-party.ts) writes it; vendor from a commit that has it." >&2
  exit 1
fi
nested_section="$(tail -n "+$nested_start" "$THIRD_PARTY_SRC")"
for nested_name in lie immediate setimmediate pako; do
  if ! grep -q "^| \`$nested_name\` |" <<< "$nested_section"; then
    echo "error: the nested-code section of $THIRD_PARTY_SRC has no row for $nested_name" >&2
    exit 1
  fi
done
if ! grep -q "Fabrice Bellard" <<< "$nested_section"; then
  echo "error: the nested-code section of $THIRD_PARTY_SRC lacks the jslinux notice (Fabrice Bellard)" >&2
  exit 1
fi

# --- provenance: pin down exactly what is being copied ----------------------

EXLIBRIS_COMMIT="unknown"
EXLIBRIS_REF="unknown"
EXLIBRIS_DIRTY="true"
if git -C "$EXLIBRIS_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  EXLIBRIS_COMMIT="$(git -C "$EXLIBRIS_DIR" rev-parse HEAD)"
  # Prefer the ref --ref named, then an exact tag, then the branch, then the
  # short commit; never empty.
  if [[ -n "$BUILD_REF" ]]; then
    EXLIBRIS_REF="$BUILD_REF"
  else
    EXLIBRIS_REF="$(
      git -C "$EXLIBRIS_DIR" describe --tags --exact-match HEAD 2>/dev/null ||
        git -C "$EXLIBRIS_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null ||
        true
    )"
    if [[ -z "$EXLIBRIS_REF" || "$EXLIBRIS_REF" == "HEAD" ]]; then
      EXLIBRIS_REF="${EXLIBRIS_COMMIT:0:7}"
    fi
  fi
  # A caller that pinned a commit names the ref to record;
  # the commit itself is still read from HEAD.
  if [[ -n "${VENDOR_EXLIBRIS_REF:-}" ]]; then
    EXLIBRIS_REF="$VENDOR_EXLIBRIS_REF"
  fi
  if [[ -n "$(git -C "$EXLIBRIS_DIR" status --porcelain)" ]]; then
    if [[ "$ALLOW_DIRTY" -eq 0 ]]; then
      echo "error: exlibris checkout has a dirty working tree: $EXLIBRIS_DIR" >&2
      git -C "$EXLIBRIS_DIR" status --porcelain >&2
      echo "  Commit/stash the changes, or re-run with --allow-dirty." >&2
      exit 1
    fi
    echo "warning: vendoring from a DIRTY exlibris checkout (--allow-dirty)" >&2
  else
    EXLIBRIS_DIRTY="false"
  fi
else
  if [[ "$ALLOW_DIRTY" -eq 0 ]]; then
    echo "error: not a git checkout: $EXLIBRIS_DIR" >&2
    echo "  Use a git checkout, or re-run with --allow-dirty." >&2
    exit 1
  fi
  echo "warning: $EXLIBRIS_DIR is not a git checkout; recording commit as 'unknown'" >&2
fi

if command -v shasum >/dev/null 2>&1; then
  sha256_of() { shasum -a 256 "$1" | awk '{ print $1 }'; }
elif command -v sha256sum >/dev/null 2>&1; then
  sha256_of() { sha256sum "$1" | awk '{ print $1 }'; }
else
  echo "error: neither shasum nor sha256sum is available; cannot record provenance" >&2
  exit 1
fi

json_escape() {
  printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'
}

EXLIBRIS_VERSION="unknown"
if [[ -f "$EXLIBRIS_DIR/package.json" ]]; then
  parsed_version="$(awk -F'"' '/^[[:space:]]*"version"[[:space:]]*:/ { print $4; exit }' \
    "$EXLIBRIS_DIR/package.json")"
  if [[ -n "$parsed_version" ]]; then
    EXLIBRIS_VERSION="$parsed_version"
  fi
fi

# The webR JavaScript client compiled into exlibris-r.js. bind() only offers
# engine versions verified with this client (inst/extdata/webr-versions.json,
# "verified_with_client"), so the pin has to travel with the bundle.
WEBR_CLIENT_PKG="$EXLIBRIS_DIR/node_modules/webr/package.json"
if [[ ! -f "$WEBR_CLIENT_PKG" ]]; then
  echo "error: $WEBR_CLIENT_PKG not found; run 'npm ci' in the exlibris checkout first" >&2
  exit 1
fi
WEBR_CLIENT_VERSION="$(awk -F'"' '/^[[:space:]]*"version"[[:space:]]*:/ { print $4; exit }' \
  "$WEBR_CLIENT_PKG")"
if [[ -z "$WEBR_CLIENT_VERSION" ]]; then
  echo "error: could not read the webR client version from $WEBR_CLIENT_PKG" >&2
  exit 1
fi

# --- copy -------------------------------------------------------------------

mkdir -p "$VIEWER_DIR"
cp "$JS_SRC" "$VIEWER_DIR/exlibris-r.js"
cp "$CSS_SRC" "$VIEWER_DIR/exlibris-r.css"
cp "$SCHEMA_SRC" "$SCHEMA_DEST"
cp "$EXCEPTION_SRC" "$VIEWER_DIR/EXCEPTION.md"

cat > "$VIEWER_DIR/index.html" <<'HTML'
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
  <title>webrarian</title>
  <link rel="stylesheet" href="./exlibris-r.css" />
</head>

<body>
  <div id="root"></div>
  <noscript>
    You need to enable JavaScript to run this app.
  </noscript>
  <script type="module" src="./exlibris-r.js"></script>
</body>

</html>
HTML

js_sha="$(sha256_of "$VIEWER_DIR/exlibris-r.js")"
css_sha="$(sha256_of "$VIEWER_DIR/exlibris-r.css")"
schema_sha="$(sha256_of "$SCHEMA_DEST")"
vendored_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# --- notices, rendered from the pin -----------------------------------------

render_notice() {
  sed -e "s|@EXLIBRIS_COMMIT@|$EXLIBRIS_COMMIT|g" \
      -e "s|@EXLIBRIS_SHORT@|${EXLIBRIS_COMMIT:0:7}|g" \
      -e "s|@EXLIBRIS_REF@|$EXLIBRIS_REF|g" \
      -e "s|@EXLIBRIS_VERSION@|$EXLIBRIS_VERSION|g" \
      -e "s|@WEBR_CLIENT_VERSION@|$WEBR_CLIENT_VERSION|g" \
      -e "s|@JS_SHA256@|$js_sha|g" \
      -e "s|@CSS_SHA256@|$css_sha|g" \
      "$1" > "$2"
  if grep -q '@[A-Z][A-Z_]*@' "$2"; then
    echo "error: $2 still has a placeholder after rendering $1" >&2
    exit 1
  fi
}

render_notice "$NOTICES_DIR/THIRD-PARTY-header.md.in" "$VIEWER_DIR/THIRD-PARTY.md"
{
  printf '\n'
  cat "$THIRD_PARTY_SRC"
} >> "$VIEWER_DIR/THIRD-PARTY.md"
render_notice "$NOTICES_DIR/LICENSE.webR.md.in" "$VIEWER_DIR/LICENSE.webR.md"
render_notice "$NOTICES_DIR/LICENSE.note.in" "$LICENSE_NOTE"

third_party_sha="$(sha256_of "$VIEWER_DIR/THIRD-PARTY.md")"
exception_sha="$(sha256_of "$VIEWER_DIR/EXCEPTION.md")"
webr_notice_sha="$(sha256_of "$VIEWER_DIR/LICENSE.webR.md")"
third_party_src_sha="$(sha256_of "$THIRD_PARTY_SRC")"
exception_src_sha="$(sha256_of "$EXCEPTION_SRC")"

# --- provenance record -------------------------------------------------------

# Keys under "files" are paths relative to inst/viewer/ (where this JSON
# lives), so the schema is ../viewer-config.schema.json.
cat > "$VIEWER_DIR/PROVENANCE.json" <<JSON
{
  "exlibrisCommit": "$(json_escape "$EXLIBRIS_COMMIT")",
  "exlibrisRef": "$(json_escape "$EXLIBRIS_REF")",
  "exlibrisVersion": "$(json_escape "$EXLIBRIS_VERSION")",
  "exlibrisDirty": $EXLIBRIS_DIRTY,
  "webrClientVersion": "$(json_escape "$WEBR_CLIENT_VERSION")",
  "vendoredAt": "$vendored_at",
  "files": {
    "exlibris-r.js": "$js_sha",
    "exlibris-r.css": "$css_sha",
    "EXCEPTION.md": "$exception_sha",
    "THIRD-PARTY.md": "$third_party_sha",
    "LICENSE.webR.md": "$webr_notice_sha",
    "../viewer-config.schema.json": "$schema_sha"
  },
  "sources": {
    "THIRD-PARTY-r.md": "$third_party_src_sha",
    "EXCEPTION.md": "$exception_src_sha"
  }
}
JSON

echo "Vendored exlibris-r from $DIST_DIR into $VIEWER_DIR"
echo "  exlibris-r.js, exlibris-r.css, EXCEPTION.md, index.html"
echo "  THIRD-PARTY.md, LICENSE.webR.md and $LICENSE_NOTE rendered from the pin"
echo "  ../viewer-config.schema.json (from $SCHEMA_SRC)"
echo "  PROVENANCE.json (exlibris ${EXLIBRIS_COMMIT:0:7} @ $EXLIBRIS_REF, v$EXLIBRIS_VERSION, webR client $WEBR_CLIENT_VERSION, dirty=$EXLIBRIS_DIRTY)"

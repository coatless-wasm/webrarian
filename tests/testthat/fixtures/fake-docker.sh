#!/bin/sh
# Fake `docker` for webrarian's tests. It never talks to a daemon.
#   FAKE_DOCKER_MODE     ok (default) | down | fail | broken
#   FAKE_DOCKER_LOG      file that receives one line per invocation
#   FAKE_DOCKER_R        R major.minor of the repo it writes (default 4.6)
#   FAKE_DOCKER_GITHUB   directory whose subdirectories are the "GitHub"
#                        package sources to compile
#   FAKE_DOCKER_SCRIPT   file that receives a copy of the build.R the
#                        container would run
mode="${FAKE_DOCKER_MODE:-ok}"
if [ -n "$FAKE_DOCKER_LOG" ]; then echo "$*" >> "$FAKE_DOCKER_LOG"; fi
case "$1" in
  --version)
    echo "Docker version 99.0.0, build fake"
    exit 0
    ;;
  info)
    if [ "$mode" = "down" ]; then
      echo "Cannot connect to the Docker daemon at unix:///var/run/docker.sock. Is the docker daemon running?" >&2
      exit 1
    fi
    echo "99.0.0"
    exit 0
    ;;
  run)
    case "$mode" in
      down) echo "docker: Cannot connect to the Docker daemon." >&2; exit 125 ;;
      broken) echo "docker: Error response from daemon: fake failure." >&2; exit 125 ;;
      fail) echo "Error in add_pkg(): compilation failed (fake)" >&2; exit 1 ;;
    esac
    build=""
    prev=""
    for arg in "$@"; do
      if [ "$prev" = "-v" ]; then
        build="${arg%%:/build*}"
        build="${build#\'}"
      fi
      prev="$arg"
    done
    if [ -n "$FAKE_DOCKER_LOG" ] && [ -d "$build/packages" ]; then
      (cd "$build/packages" && find . -type f | sort) >> "$FAKE_DOCKER_LOG"
    fi
    if [ -n "$FAKE_DOCKER_SCRIPT" ] && [ -f "$build/build.R" ]; then
      cp "$build/build.R" "$FAKE_DOCKER_SCRIPT"
    fi
    out="$build/repo/bin/emscripten/contrib/${FAKE_DOCKER_R:-4.6}"
    mkdir -p "$out"
    compile() {
      src="$1"
      pkg=$(sed -n 's/^Package:[[:space:]]*//p' "$src/DESCRIPTION" | head -n 1)
      ver=$(sed -n 's/^Version:[[:space:]]*//p' "$src/DESCRIPTION" | head -n 1)
      tmp=$(mktemp -d)
      mkdir -p "$tmp/$pkg"
      cp "$src/DESCRIPTION" "$tmp/$pkg/DESCRIPTION"
      tar -czf "$out/${pkg}_${ver}.tgz" -C "$tmp" "$pkg"
      rm -rf "$tmp"
    }
    if [ -d "$build/packages" ]; then
      for d in "$build"/packages/*/; do
        if [ -f "$d/DESCRIPTION" ]; then compile "$d"; fi
      done
    fi
    if [ -n "$FAKE_DOCKER_GITHUB" ] && [ -d "$FAKE_DOCKER_GITHUB" ]; then
      for d in "$FAKE_DOCKER_GITHUB"/*/; do
        if [ -f "$d/DESCRIPTION" ]; then compile "$d"; fi
      done
    fi
    exit 0
    ;;
esac
echo "fake docker: unsupported command $1" >&2
exit 2

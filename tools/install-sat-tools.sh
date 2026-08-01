#!/usr/bin/env bash
# ADR-0008 D3: build the certification toolchain — kissat (DRAT-emitting CDCL solver) and drat-trim (the
# proof checker) — at PINNED tags, then smoke-test the solve->proof->verify pipeline and record versions,
# commits and binary sha256 in tools/bin/TOOLS.manifest (CertifyProbe copies them into run manifests).
# Dev/CI only: the library stays pure-JVM SAT4J; nothing built here is a runtime dependency.
set -euo pipefail

KISSAT_REPO=https://github.com/arminbiere/kissat
KISSAT_REF=rel-4.0.4 # commit 8af8e56f174b778aef3aa45af9f739b2a5f492c2
DRATTRIM_REPO=https://github.com/marijnheule/drat-trim
# pinned COMMIT, not tag: master is ahead of the only release tag (v05.22.2023 = 2e5e29cb)
DRATTRIM_REF=2e3b2dc0ecf938addbd779d42877b6ed69d9a985 # master HEAD as of 2026-07-14

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VENDOR="$ROOT/tools/vendor"
BIN="$ROOT/tools/bin"
mkdir -p "$VENDOR" "$BIN"

clone_at() { # dir repo ref — idempotent: keep an existing checkout only if it sits exactly on the pin.
  # ref may be a tag (shallow clone) or a full 40-hex commit (full clone + detached checkout).
  local dir=$1 repo=$2 ref=$3
  if [ -d "$dir" ]; then
    local have
    have=$(git -C "$dir" describe --tags --exact-match 2>/dev/null || true)
    [ "$have" = "$ref" ] && return 0
    [ "$(git -C "$dir" rev-parse HEAD 2>/dev/null || true)" = "$ref" ] && return 0
    rm -rf "$dir"
  fi
  if [[ "$ref" =~ ^[0-9a-f]{40}$ ]]; then
    git clone --quiet "$repo" "$dir"
    git -C "$dir" -c advice.detachedHead=false checkout --quiet "$ref"
  else
    git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$ref" "$repo" "$dir"
  fi
}

sha256() { # portable: coreutils sha256sum on Linux, shasum on macOS
  if command -v sha256sum > /dev/null; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

echo "== kissat $KISSAT_REF"
clone_at "$VENDOR/kissat" "$KISSAT_REPO" "$KISSAT_REF"
# NOTE: no './configure --quiet' — that compiles a QUIET binary that REJECTS the runtime '-q' flag
(cd "$VENDOR/kissat" && ./configure > /dev/null && make -s kissat)
cp "$VENDOR/kissat/build/kissat" "$BIN/kissat"

echo "== drat-trim $DRATTRIM_REF"
clone_at "$VENDOR/drat-trim" "$DRATTRIM_REPO" "$DRATTRIM_REF"
(cd "$VENDOR/drat-trim" && make -s drat-trim)
cp "$VENDOR/drat-trim/drat-trim" "$BIN/drat-trim"

echo "== smoke test (solve -> DRAT -> verify)"
SMOKE=$(mktemp -d)
trap 'rm -rf "$SMOKE"' EXIT
printf 'p cnf 2 4\n1 2 0\n1 -2 0\n-1 2 0\n-1 -2 0\n' > "$SMOKE/unsat.cnf"
set +e
"$BIN/kissat" -q "$SMOKE/unsat.cnf" "$SMOKE/unsat.drat" > /dev/null
KISSAT_EXIT=$?
set -e
[ "$KISSAT_EXIT" -eq 20 ] || { echo "FAIL: kissat exit $KISSAT_EXIT on an UNSAT instance (want 20)"; exit 1; }
# capture, don't pipe into grep -q (early-exit SIGPIPE + pipefail = spurious failure); no ^ anchor —
# drat-trim emits "\rs VERIFIED" (carriage return overwriting its progress line)
DRAT_OUT=$("$BIN/drat-trim" "$SMOKE/unsat.cnf" "$SMOKE/unsat.drat")
grep -q 's VERIFIED' <<< "$DRAT_OUT" || { echo "FAIL: drat-trim did not verify the smoke proof"; exit 1; }
echo "   kissat UNSAT (exit 20), drat-trim s VERIFIED"

KISSAT_COMMIT=$(git -C "$VENDOR/kissat" rev-parse HEAD)
DRATTRIM_COMMIT=$(git -C "$VENDOR/drat-trim" rev-parse HEAD)
KISSAT_VERSION=$("$BIN/kissat" --version)
{
  echo "built $(date -u +%Y-%m-%dT%H:%M:%SZ) with $(gcc --version | head -1)"
  echo "kissat $KISSAT_VERSION $KISSAT_REF $KISSAT_COMMIT sha256=$(sha256 "$BIN/kissat")"
  echo "drat-trim $DRATTRIM_REF $DRATTRIM_COMMIT sha256=$(sha256 "$BIN/drat-trim")"
} > "$BIN/TOOLS.manifest"
echo "== done"
cat "$BIN/TOOLS.manifest"

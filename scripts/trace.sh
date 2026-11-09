#!/usr/bin/env bash
# Compile one program with tracing on. Paths are relative to the project root.
# Usage: trace.sh FILE.pav [OUT.json]   (default OUT: traces/NAME.json)
# Fails unless the trace's selfCheck is "ok".
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java

args=()
for arg in "$@"; do
  [ "$arg" = "--verbose" ] || args+=("$arg")
done
[ "${#args[@]}" -ge 1 ] || die "usage: trace.sh FILE.pav [OUT.json]"
src=${args[0]}
name=$(basename "$src" .pav)
out=${args[1]:-traces/$name.json}
[ -f "$src" ] || die "no such file: $src (paths are relative to the project root)"
[ -f compiler/Parva/Parva.class ] || die "the compiler is not built. Run scripts/build-compiler.sh first."

run_parva "$src" -t trace.json
if [ ! -f "$RUN_DIR/trace.json" ]; then
  cat "$RUN_DIR/stderr" >&2
  die "no trace was written for $src (work files kept in $RUN_DIR)"
fi
mkdir -p "$(dirname "$out")"
mv "$RUN_DIR/trace.json" "$out"
rm -rf "$RUN_DIR"

events=$(grep -c '^    {"i":' "$out" || true)
if grep -q '"selfCheck":"ok"' "$out"; then
  echo "$out: $events events, selfCheck ok"
else
  grep -o '"selfCheck":"[^"]*"' "$out" >&2 || true
  die "$out: the trace's self-check failed"
fi

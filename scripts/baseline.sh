#!/usr/bin/env bash
# Phase 0 only: record every example's untraced .cod, stdout and exit code in
# tests/baseline/. check.sh compares every later run against these files.
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java

force=0
for arg in "$@"; do
  case "$arg" in
    --force) force=1 ;;
    --verbose) ;;
    *) die "usage: baseline.sh [--force]" ;;
  esac
done

[ -f compiler/Parva/Parva.class ] || die "the compiler is not built. Run scripts/build-compiler.sh first."
if [ -e tests/baseline ] && [ "$force" -eq 0 ]; then
  die "tests/baseline/ already exists. Use --force to overwrite it."
fi

rm -rf tests/baseline
mkdir -p tests/baseline
count=0
for pav in compiler/examples/*.pav; do
  name=$(basename "$pav" .pav)
  run_parva "$pav"
  if [ -f "$RUN_DIR/$name.cod" ]; then
    cp "$RUN_DIR/$name.cod" "tests/baseline/$name.cod"
  fi
  cp "$RUN_DIR/stdout" "tests/baseline/$name.out"
  cp "$RUN_DIR/exit" "tests/baseline/$name.exit"
  rm -rf "$RUN_DIR"
  count=$((count + 1))
done

echo "Recorded $count examples in tests/baseline/"

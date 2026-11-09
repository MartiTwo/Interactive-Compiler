#!/usr/bin/env bash
# Copy the Prac8 compiler sources into compiler/. Prac8 is only ever read.
# Usage: import-compiler.sh [PRAC8_DIR] [--force]
set -euo pipefail
. "$(dirname "$0")/lib.sh"

src="$PRAC8_DEFAULT"
force=0
for arg in "$@"; do
  case "$arg" in
    --force) force=1 ;;
    --verbose) ;;
    -*) die "usage: import-compiler.sh [PRAC8_DIR] [--force]" ;;
    *) src=$arg ;;
  esac
done

[ -f "$src/Parva.atg" ] || die "no Parva.atg in '$src'"
if [ -e compiler ] && [ "$force" -eq 0 ]; then
  die "compiler/ already exists. Use --force to re-copy the Prac8 files (this discards // TRACE edits in them)."
fi

mkdir -p compiler/Parva compiler/library compiler/examples
for f in Parva.atg Parser.frame Scanner.frame Parva.frame Coco.jar; do
  cp "$src/$f" "compiler/$f"
done
for f in CodeGen.java Table.java PVM.java; do
  cp "$src/Parva/$f" "compiler/Parva/$f"
done
cp "$src"/library/*.class compiler/library/
cp "$src"/examples/*.pav compiler/examples/
cp "$src"/examples/*.cod compiler/examples/

echo "Imported $src into compiler/"

#!/usr/bin/env bash
# Run the Parva compiler, like Parva.bat. Paths are relative to the project root.
# Usage: parva.sh FILE.pav [-l] [-d] [-w] [-c] [-t OUT.json]
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java

args=()
for arg in "$@"; do
  [ "$arg" = "--verbose" ] || args+=("$arg")
done
[ "${#args[@]}" -ge 1 ] || die "usage: parva.sh FILE.pav [-l] [-d] [-w] [-c] [-t OUT.json]"
[ -f compiler/Parva/Parva.class ] || die "the compiler is not built. Run scripts/build-compiler.sh first."

exec java -cp compiler Parva.Parva "${args[@]}"

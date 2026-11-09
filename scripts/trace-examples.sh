#!/usr/bin/env bash
# Trace every example into traces/, then embed them all in src/Stepper/Examples.elm.
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java

[ -f compiler/tools-bin/EmbedTraces.class ] || die "the tools are not built. Run scripts/build-compiler.sh first."

mkdir -p traces
rm -f traces/*.json
for pav in compiler/examples/*.pav; do
  bash scripts/trace.sh "$pav"
done
java -cp compiler/tools-bin EmbedTraces traces src/Stepper/Examples.elm

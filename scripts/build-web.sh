#!/usr/bin/env bash
# Build both web apps: stepper.html (the trace stepper) and index.html (the explorer).
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need elm

[ -f src/Stepper/Examples.elm ] || die "src/Stepper/Examples.elm is missing. Run scripts/trace-examples.sh first."

elm make src/Stepper/Main.elm --optimize --output=stepper.html
elm make src/Main.elm --output=index.html

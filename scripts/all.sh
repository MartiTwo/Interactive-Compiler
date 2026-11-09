#!/usr/bin/env bash
# The one command for a new user: build the compiler, prove that tracing
# changes nothing, trace every example, and build both web apps.
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java
need javac
need elm

bash scripts/build-compiler.sh
bash scripts/check.sh
bash scripts/trace-examples.sh
bash scripts/build-web.sh

echo
echo "Done. Open stepper.html (or index.html) in a browser."

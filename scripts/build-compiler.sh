#!/usr/bin/env bash
# Regenerate the parser with Coco/R, instrument it, and compile compiler/.
# The full log goes to .tmp/build.log; on failure its last lines are shown.
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java
need javac

[ -f compiler/Parva.atg ] || die "compiler/ is missing. Run scripts/import-compiler.sh first."
mkdir -p .tmp
log=.tmp/build.log
: > "$log"

fail() {
  echo "build failed at: $1. Last lines of $log:" >&2
  last_lines 25 "$log" >&2
  exit 1
}

# 1. Coco/R, run inside compiler/ exactly as cmake.bat does.
( cd compiler && java -jar Coco.jar Parva.atg -options m ) >> "$log" 2>&1 || fail "Coco/R"
if [ -f compiler/listing.txt ]; then
  cat compiler/listing.txt >> "$log"
  rm -f compiler/listing.txt
fi
for f in Parser Scanner Parva; do
  [ -f "compiler/$f.java" ] || fail "Coco/R did not write $f.java"
  mv "compiler/$f.java" "compiler/Parva/$f.java"
done

# 2. Build tools, then instrument the parser once Parser.frame has the markers.
if ls compiler/tools/*.java > /dev/null 2>&1; then
  mkdir -p compiler/tools-bin
  javac -d compiler/tools-bin compiler/tools/*.java >> "$log" 2>&1 || fail "javac tools"
fi
if grep -q "TRACE-PRODUCTIONS-BEGIN" compiler/Parser.frame; then
  java -cp compiler/tools-bin Instrument compiler/Parva/Parser.java >> "$log" 2>&1 || fail "Instrument"
  grep "^Instrumented" "$log" || true
fi

# 3. The compiler itself.
javac -cp compiler -d compiler -nowarn compiler/Parva/*.java >> "$log" 2>&1 || fail "javac compiler"

echo "Built compiler/ (log: $log)"

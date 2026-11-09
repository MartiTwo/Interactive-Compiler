#!/usr/bin/env bash
# Prove that tracing changes nothing, and that every trace is sound:
# (1) each example's untraced run matches tests/baseline/ (.cod, stdout, exit), and
#     its traced run is byte-identical to the untraced one
# (2) each trace ends with "end" and has selfCheck "ok"
# (3) the build log says all 42 productions were instrumented
# (4) both Elm apps compile
set -euo pipefail
. "$(dirname "$0")/lib.sh"
need java

[ -f compiler/Parva/Parva.class ] || die "the compiler is not built. Run scripts/build-compiler.sh first."
[ -d tests/baseline ] || die "tests/baseline/ is missing. Run scripts/baseline.sh once."

passed=0
failed=0
pass() { printf 'PASS  %s\n' "$1"; passed=$((passed + 1)); }
fail() { printf 'FAIL  %s\n' "$1"; failed=$((failed + 1)); }

# same_text A B: A and B are equal once every CR is ignored (bash only).
same_text() {
  local x y more_x more_y
  exec 3< "$1" 4< "$2"
  while :; do
    more_x=1
    more_y=1
    IFS= read -r x <&3 || [ -n "$x" ] || more_x=0
    IFS= read -r y <&4 || [ -n "$y" ] || more_y=0
    if [ "$more_x" = 0 ] || [ "$more_y" = 0 ] || [ "${x//$'\r'/}" != "${y//$'\r'/}" ]; then
      exec 3<&- 4<&-
      [ "$more_x" = 0 ] && [ "$more_y" = 0 ]
      return
    fi
  done
}

# compare ACTUAL EXPECTED LABEL: byte-identical, or (noted) equal apart from line
# ends, which the compiler writes with the platform's separator. Adds to WHY/NOTE.
compare() {
  if [ ! -f "$2" ]; then
    [ ! -f "$1" ] || WHY="$WHY unexpected $3;"
  elif [ ! -f "$1" ]; then
    WHY="$WHY no $3;"
  elif ! cmp -s "$1" "$2"; then
    if same_text "$1" "$2"; then NOTE=" (line ends differ from the baseline's)"; else WHY="$WHY $3 differs;"; fi
  fi
}

for pav in compiler/examples/*.pav; do
  name=$(basename "$pav" .pav)
  base="tests/baseline/$name"
  WHY=""
  NOTE=""

  run_parva "$pav"
  plain=$RUN_DIR
  compare "$plain/$name.cod" "$base.cod" ".cod"
  compare "$plain/stdout" "$base.out" "stdout"
  cmp -s "$plain/exit" "$base.exit" || WHY="$WHY exit $(cat "$plain/exit") not $(cat "$base.exit");"

  run_parva "$pav" -t trace.json
  for f in "$name.cod" stdout exit; do
    if [ -f "$plain/$f" ] || [ -f "$RUN_DIR/$f" ]; then
      cmp -s "$plain/$f" "$RUN_DIR/$f" || WHY="$WHY traced $f differs from untraced;"
    fi
  done
  rm -rf "$plain"

  if [ -z "$WHY" ]; then
    pass "same output   $name$NOTE"
  else
    fail "same output   $name:$WHY"
  fi

  t="$RUN_DIR/trace.json"
  if [ ! -f "$t" ]; then
    fail "self-check    $name: no trace written"
  elif ! grep -q '"ev":"end"' "$t"; then
    fail "self-check    $name: trace has no end event ($(grep -o '"reason":"[^"]*"' "$t" || echo 'no reason'))"
  elif ! grep -q '"selfCheck":"ok"' "$t"; then
    fail "self-check    $name: $(grep -o '"selfCheck":"[^"]*"' "$t")"
  else
    pass "self-check    $name"
  fi
  rm -rf "$RUN_DIR"
done

if grep -q "TRACE-PRODUCTIONS-BEGIN" compiler/Parser.frame; then
  if grep -q "^Instrumented 42 productions" .tmp/build.log 2> /dev/null; then
    pass "build log     Instrumented 42 productions"
  else
    fail "build log     no 'Instrumented 42 productions' in .tmp/build.log (rebuild with scripts/build-compiler.sh)"
  fi
else
  echo "SKIP  build log     Parser.frame has no production markers yet"
fi

need elm
for entry in src/Main.elm src/Stepper/Main.elm; do
  if [ ! -f "$entry" ]; then
    echo "SKIP  elm make      $entry does not exist yet"
    continue
  fi
  label=$(basename "$(dirname "$entry")")
  if elm make "$entry" --output=".tmp/check-$label.js" > .tmp/elm-make.log 2>&1; then
    pass "elm make      $entry"
  else
    fail "elm make      $entry (see .tmp/elm-make.log)"
  fi
done

echo
echo "$passed passed, $failed failed"
[ "$failed" -eq 0 ]

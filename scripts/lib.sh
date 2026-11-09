#!/usr/bin/env bash
# Shared helpers, sourced by every script in scripts/. Sets ROOT and cd's there.
# With --verbose anywhere on the caller's command line, prints tool versions.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

PRAC8_DEFAULT="/c/Users/marti/OneDrive/Desktop/2026/CS301/PLT/Prac8"

die() {
  echo "error: $*" >&2
  exit 1
}

need() {
  if command -v "$1" >/dev/null 2>&1; then
    return 0
  fi
  case "$1" in
    java | javac) die "'$1' not found. Install a JDK (version 8 or newer) and put its bin/ folder on PATH." ;;
    elm) die "'elm' not found. Install Elm 0.19.1: https://guide.elm-lang.org/install/elm.html" ;;
    *) die "'$1' not found." ;;
  esac
}

# last_lines N FILE: the last N lines of FILE, using bash only.
last_lines() {
  local n=$1 file=$2 count=0 k line
  local -a ring=()
  while IFS= read -r line || [ -n "$line" ]; do
    ring[count % n]=$line
    count=$((count + 1))
  done < "$file"
  k=0
  if [ "$count" -gt "$n" ]; then k=$((count - n)); fi
  while [ "$k" -lt "$count" ]; do
    printf '%s\n' "${ring[k % n]}"
    k=$((k + 1))
  done
}

# run_parva SRC.pav [compiler flags...]
# Compiles a copy of SRC inside a fresh dir directly under .tmp/ and sets
# RUN_DIR to it. The run happens inside RUN_DIR so the compiler sees a bare
# file name: Errors prints the path it was given, and a .tmp/XXXX path would
# make stdout differ on every run. Leaves NAME.cod, stdout, stderr and exit.
run_parva() {
  local src=$1 name
  shift
  name=$(basename "$src" .pav)
  mkdir -p .tmp
  RUN_DIR=$(mktemp -d .tmp/XXXX)
  cp "$src" "$RUN_DIR/$name.pav"
  set +e
  (
    cd "$RUN_DIR" || exit 99
    echo n | java -cp ../../compiler Parva.Parva "$@" "$name.pav" > stdout 2> stderr
    exit "${PIPESTATUS[1]}"
  )
  echo "$?" > "$RUN_DIR/exit"
  set -e
}

for arg in "$@"; do
  if [ "$arg" = "--verbose" ]; then
    echo "ROOT=$ROOT"
    for tool in java javac elm; do
      if command -v "$tool" >/dev/null 2>&1; then
        printf '%-6s ' "$tool"
        case "$tool" in
          elm) elm --version ;;
          *) "$tool" -version 2>&1 | { IFS= read -r first; echo "$first"; } ;;
        esac
      else
        echo "$tool   (not found)"
      fi
    done
    echo "bash   $BASH_VERSION"
  fi
done

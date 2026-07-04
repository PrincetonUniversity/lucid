#!/usr/bin/env bash
#
# C backend (lucidcc) golden-file regression tests.
#
# Each program in ctests/programs/ is compiled with the C backend; the generated
# C output is compared byte-for-byte against a checked-in expected file in
# ctests/expected/.
#
# Usage:
#   ./ctests/run_tests.sh             normal mode: compile + diff against expected
#   ./ctests/run_tests.sh --update    update mode: (re)generate the expected files
#   ./ctests/run_tests.sh --no-build  skip the `make` step (reuse the current lucidcc)
#   ./ctests/run_tests.sh --help      show this help
#
# Normal mode exits non-zero if any test fails (compile error or output mismatch),
# so it is CI-friendly. Update mode always exits 0.
#
# Note: the generated C embeds fresh-id temp names (e.g. tmp_4331). These are
# deterministic for a fixed compiler, so byte-diffing is stable -- but a compiler
# change that shifts id allocation will legitimately change the output and require
# a `--update`. Inspect the printed diff to confirm a change is intended.
#
set -uo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROGRAMS_DIR="$SCRIPT_DIR/programs"
EXPECTED_DIR="$SCRIPT_DIR/expected"
LUCIDCC="$ROOT/lucidcc"
FLAGS="--lpcap"

UPDATE=0
BUILD=1
for arg in "$@"; do
  case "$arg" in
    --update|-u)  UPDATE=1 ;;
    --no-build)   BUILD=0 ;;
    -h|--help)    grep '^#' "$0" | cut -c3- ; exit 0 ;;
    *) echo "unknown argument: $arg (try --help)" >&2; exit 2 ;;
  esac
done

# Build the compiler first (unless skipped) so tests reflect the current code.
if [ "$BUILD" -eq 1 ]; then
  echo "building lucidcc (make)..."
  if ! ( cd "$ROOT" && make ) >/tmp/ctests_build.log 2>&1; then
    echo "BUILD FAILED -- last lines of /tmp/ctests_build.log:" >&2
    tail -20 /tmp/ctests_build.log >&2
    exit 1
  fi
fi

[ -x "$LUCIDCC" ] || { echo "lucidcc not found at $LUCIDCC (run \`make\` first)" >&2; exit 1; }
mkdir -p "$EXPECTED_DIR"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0; updated=0; ran=0
for prog in "$PROGRAMS_DIR"/*.dpt; do
  name="$(basename "$prog" .dpt)"
  # reject-marked programs produce no C (they're expected to fail the compat gate); the
  # gate itself is checked by run_c_tests.sh, so there's no golden to diff here.
  if grep -q 'LUCIDCC_REJECT' "$prog"; then continue; fi
  ran=$((ran+1))
  got="$WORK/$name.cc"
  log="$WORK/$name.log"
  exp="$EXPECTED_DIR/$name.cc"

  # Compile. The compiler chatters on stdout; the artifact is the -o file.
  if ! "$LUCIDCC" "$prog" -o "$got" $FLAGS >"$log" 2>&1; then
    echo "FAIL  $name  (compiler exited non-zero)"
    tail -5 "$log" | sed 's/^/      | /'
    fail=$((fail+1))
    continue
  fi
  if [ ! -f "$got" ]; then
    echo "FAIL  $name  (no output file produced)"
    fail=$((fail+1))
    continue
  fi

  if [ "$UPDATE" -eq 1 ]; then
    cp "$got" "$exp"
    echo "UPDATE  $name"
    updated=$((updated+1))
  elif [ ! -f "$exp" ]; then
    echo "FAIL  $name  (no expected file -- run with --update first)"
    fail=$((fail+1))
  elif diff -q "$exp" "$got" >/dev/null; then
    echo "PASS  $name"
    pass=$((pass+1))
  else
    echo "FAIL  $name  (output differs from expected)"
    diff "$exp" "$got" | head -30 | sed 's/^/      | /'
    fail=$((fail+1))
  fi
done

echo "-----"
if [ "$ran" -eq 0 ]; then
  echo "no programs found in $PROGRAMS_DIR" >&2
  exit 1
fi
if [ "$UPDATE" -eq 1 ]; then
  echo "updated $updated expected file(s)"
else
  echo "$pass passed, $fail failed"
  [ "$fail" -eq 0 ] || exit 1
fi

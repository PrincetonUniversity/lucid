#!/usr/bin/env bash
#
# pcap driver run tests.
#
# For each program with a pcap (ctests/pcaps/<name>.in.pcap), generate + compile
# the binary, run it, and compare the output against the pcap. 
# Usage:
#   ./ctests/test_pcap.sh            run + diff
#   ./ctests/test_pcap.sh --update   regenerate pcaps/<name>.expected.pcap
#   ./ctests/test_pcap.sh --no-build skip `make`, reuse the current ./lucidcc
#   ./ctests/test_pcap.sh --help     show this help
set -uo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROGRAMS_DIR="$SCRIPT_DIR/programs"
PCAPS_DIR="$SCRIPT_DIR/pcaps"
LUCIDCC="$ROOT/lucidcc"

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

if [ "$BUILD" -eq 1 ]; then
  echo "building lucidcc (make)..."
  if ! ( cd "$ROOT" && make ) >/tmp/test_pcap_build.log 2>&1; then
    echo "BUILD FAILED -- last lines of /tmp/test_pcap_build.log:" >&2
    tail -20 /tmp/test_pcap_build.log >&2
    exit 1
  fi
fi
[ -x "$LUCIDCC" ] || { echo "lucidcc not found at $LUCIDCC (run \`make\` first)" >&2; exit 1; }

# refresh the input pcaps from their generator (keeps fixtures reproducible)
if [ -f "$PCAPS_DIR/gen_inputs.py" ]; then
  python3 "$PCAPS_DIR/gen_inputs.py" >/dev/null || { echo "input pcap generation failed" >&2; exit 1; }
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0; updated=0; ran=0
for prog in "$PROGRAMS_DIR"/*.dpt; do
  name="$(basename "$prog" .dpt)"
  in_pcap="$PCAPS_DIR/$name.in.pcap"
  [ -f "$in_pcap" ] || continue   # only programs with a fixture
  ran=$((ran+1))
  exp_pcap="$PCAPS_DIR/$name.expected.pcap"

  # generate + compile the pcap binary
  if ! "$LUCIDCC" "$prog" -o "$WORK/$name.c" --lpcap >"$WORK/$name.gen.log" 2>&1; then
    echo "FAIL  $name  (lucidcc codegen)"; sed 's/^/    /' "$WORK/$name.gen.log" | tail -3 >&2; fail=$((fail+1)); continue
  fi
  if ! gcc -O2 -o "$WORK/$name.bin" "$WORK/$name.c" -lpcap >"$WORK/$name.gcc.log" 2>&1; then
    echo "FAIL  $name  (gcc)"; grep "error:" "$WORK/$name.gcc.log" | sed 's/^/    /' | head -3 >&2; fail=$((fail+1)); continue
  fi
  if ! "$WORK/$name.bin" --interface "1:$in_pcap:$WORK/$name.out.pcap" >"$WORK/$name.run.log" 2>&1; then
    echo "FAIL  $name  (run crashed, exit $?)"; fail=$((fail+1)); continue
  fi

  if [ "$UPDATE" -eq 1 ]; then
    cp "$WORK/$name.out.pcap" "$exp_pcap"; echo "UPDATE  $name"; updated=$((updated+1)); continue
  fi
  if [ -f "$exp_pcap" ] && cmp -s "$WORK/$name.out.pcap" "$exp_pcap"; then
    echo "PASS  $name"; pass=$((pass+1))
  else
    echo "FAIL  $name  (output differs from expected)"; fail=$((fail+1))
  fi
done

echo "-----"
if [ "$ran" -eq 0 ]; then echo "no pcap fixtures found in $PCAPS_DIR" >&2; exit 1; fi
if [ "$UPDATE" -eq 1 ]; then
  echo "updated $updated expected pcap(s)"
else
  echo "$pass passed, $fail failed  ($ran fixtures)"
fi
[ "$fail" -eq 0 ] || exit 1

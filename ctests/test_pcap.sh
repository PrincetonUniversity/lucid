#!/usr/bin/env bash
#
# pcap driver run tests.
#
# Generate the fixtures (gen_inputs.py -> pcaps/<name>.{in,expected}.pcap), then for
# each program with a fixture, generate + compile the binary, run it on the input,
# and byte-compare its output against the expected pcap. On a mismatch the output
# is kept as pcaps/<name>.actual.pcap.
# Usage:
#   ./ctests/test_pcap.sh            run + compare
#   ./ctests/test_pcap.sh --no-build skip `make`, reuse the current ./lucidcc
#   ./ctests/test_pcap.sh --help     show this help
set -uo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROGRAMS_DIR="$SCRIPT_DIR/programs"
PCAPS_DIR="$SCRIPT_DIR/pcaps"
LUCIDCC="$ROOT/lucidcc"

BUILD=1
for arg in "$@"; do
  case "$arg" in
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

python3 "$SCRIPT_DIR/gen_inputs.py" >/dev/null || { echo "pcap fixture generation failed" >&2; exit 1; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0; ran=0
for prog in "$PROGRAMS_DIR"/*.dpt; do
  name="$(basename "$prog" .dpt)"
  in_pcap="$PCAPS_DIR/$name.in.pcap"
  [ -f "$in_pcap" ] || continue   # only programs with a fixture
  ran=$((ran+1))
  exp_pcap="$PCAPS_DIR/$name.expected.pcap"
  act_pcap="$PCAPS_DIR/$name.actual.pcap"
  rm -f "$act_pcap"

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

  if cmp -s "$WORK/$name.out.pcap" "$exp_pcap"; then
    echo "PASS  $name"; pass=$((pass+1))
  else
    cp "$WORK/$name.out.pcap" "$act_pcap"
    echo "FAIL  $name  (output differs from expected; see pcaps/$name.actual.pcap)"; fail=$((fail+1))
  fi
done

echo "-----"
if [ "$ran" -eq 0 ]; then echo "no pcap fixtures found in $PCAPS_DIR" >&2; exit 1; fi
echo "$pass passed, $fail failed  ($ran fixtures)"
[ "$fail" -eq 0 ] || exit 1

#!/usr/bin/env bash
#
# C backend (lucidcc) compile + run tests.
#
# For each program in ctests/programs/:
#   1. generate C with `lucidcc --lpcap` and gcc-compile it (always);
#   2. if ctests/pcaps/<name>.in.pcap exists, run the binary on it and compare the
#      output pcap byte-for-byte against ctests/pcaps/<name>.expected.pcap.
#
# This complements run_tests.sh, which only byte-diffs the *generated C text*;
# this one checks the generated C actually compiles and (for programs with a pcap
# fixture) produces the expected packets.
#
# Finally, if DPDK is installed, it also runs the DPDK driver test (test_dpdk.py:
# compile the reflector with `lucidcc --dpdk` and run it on the pcap PMD). That
# portion is skipped -- not failed -- where DPDK or non-interactive root is absent.
#
# Adding a run test for a program: add an input pcap (e.g. via pcaps/gen_inputs.py)
# at ctests/pcaps/<name>.in.pcap, then run with --update to capture its expected
# output. Programs without an input pcap are compile-only.
#
# Usage:
#   ./ctests/run_c_tests.sh             compile all; run+diff those with fixtures
#   ./ctests/run_c_tests.sh --update    (re)capture the expected output pcaps
#   ./ctests/run_c_tests.sh --no-build  skip `make` (reuse the current lucidcc)
#   ./ctests/run_c_tests.sh --help      show this help
#
# Exits non-zero if any program fails to compile or its output differs (CI-friendly).
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
  if ! ( cd "$ROOT" && make ) >/tmp/cctests_build.log 2>&1; then
    echo "BUILD FAILED -- last lines of /tmp/cctests_build.log:" >&2
    tail -20 /tmp/cctests_build.log >&2
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

pass=0; fail=0; ran=0; updated=0

for prog in "$PROGRAMS_DIR"/*.dpt; do
  name="$(basename "$prog" .dpt)"

  # A program marked `// LUCIDCC_REJECT: <substr>` is expected to be REJECTED by the C
  # compatibility gate (e.g. it uses an unsupported feature). It passes iff lucidcc exits
  # non-zero AND the error mentions <substr>.
  reject_pat="$(sed -n 's/.*LUCIDCC_REJECT:[[:space:]]*//p' "$prog" | head -1)"
  if [ -n "$reject_pat" ]; then
    if "$LUCIDCC" "$prog" -o "$WORK/$name.c" --lpcap >"$WORK/$name.gen.log" 2>&1; then
      echo "FAIL  $name  (expected rejection, but codegen succeeded)"; fail=$((fail+1))
    elif grep -qF "$reject_pat" "$WORK/$name.gen.log"; then
      echo "ok    $name  (correctly rejected: $reject_pat)"; pass=$((pass+1))
    else
      echo "FAIL  $name  (rejected, but error did not mention '$reject_pat')"; fail=$((fail+1))
    fi
    continue
  fi

  # 1. generate C
  if ! "$LUCIDCC" "$prog" -o "$WORK/$name.c" --lpcap >"$WORK/$name.gen.log" 2>&1; then
    echo "FAIL  $name  (lucidcc codegen)"; sed 's/^/    /' "$WORK/$name.gen.log" | tail -3 >&2
    fail=$((fail+1)); continue
  fi

  # 2. gcc-compile
  if ! gcc -o "$WORK/$name.bin" "$WORK/$name.c" -lpcap >"$WORK/$name.gcc.log" 2>&1; then
    echo "FAIL  $name  (gcc)"; grep "error:" "$WORK/$name.gcc.log" | sed 's/^/    /' | head -3 >&2
    fail=$((fail+1)); continue
  fi

  # 3. run, if a fixture exists
  in_pcap="$PCAPS_DIR/$name.in.pcap"
  exp_pcap="$PCAPS_DIR/$name.expected.pcap"
  if [ ! -f "$in_pcap" ]; then
    echo "ok    $name  (compile only)"; pass=$((pass+1)); continue
  fi
  ran=$((ran+1))
  if ! "$WORK/$name.bin" "$in_pcap" "$WORK/$name.out.pcap" >"$WORK/$name.run.log" 2>&1; then
    echo "FAIL  $name  (run crashed, exit $?)"; fail=$((fail+1)); continue
  fi
  if [ "$UPDATE" -eq 1 ]; then
    cp "$WORK/$name.out.pcap" "$exp_pcap"; echo "UPDATE  $name"; updated=$((updated+1)); continue
  fi
  if [ -f "$exp_pcap" ] && cmp -s "$WORK/$name.out.pcap" "$exp_pcap"; then
    echo "PASS  $name  (compile + run)"; pass=$((pass+1))
  else
    echo "FAIL  $name  (output differs from expected)"; fail=$((fail+1))
  fi
done

echo "-----"
if [ "$UPDATE" -eq 1 ]; then
  echo "updated $updated expected pcap(s)"; exit 0
fi

# DPDK driver test: regenerate the reflector with `lucidcc --dpdk` (fresh from the
# make above), build it against DPDK, and run it on the pcap PMD (net_pcap vdev),
# checking the reflection -- via test_dpdk.py, pointed at temp dirs so nothing
# committed is touched. Skipped (not failed) where it can't run: no libdpdk
# installed, or no non-interactive root for DPDK's EAL.
if ! pkg-config --exists libdpdk 2>/dev/null; then
  echo "skip  dpdk driver test  (libdpdk not installed)"
elif [ "$(id -u)" -ne 0 ] && ! sudo -n true 2>/dev/null; then
  echo "skip  dpdk driver test  (needs root / passwordless sudo for DPDK EAL)"
else
  dpdk_env=(env "DPDK_BUILD_DIR=$WORK/_dpdk_build" "DPDK_PCAP_DIR=$WORK/dpdk_pcaps")
  if "${dpdk_env[@]}" python3 "$SCRIPT_DIR/test_dpdk.py" --gen >"$WORK/dpdk.gen.log" 2>&1 \
     && "${dpdk_env[@]}" python3 "$SCRIPT_DIR/test_dpdk.py" >"$WORK/dpdk.run.log" 2>&1; then
    counts="$(grep -o 'sent [0-9]*, received [0-9]*' "$WORK/dpdk.run.log" | tail -1)"
    echo "PASS  dpdk driver test  (compile + run${counts:+: $counts})"; pass=$((pass+1)); ran=$((ran+1))
  else
    echo "FAIL  dpdk driver test"
    { tail -5 "$WORK/dpdk.gen.log"; tail -5 "$WORK/dpdk.run.log"; } 2>/dev/null | sed 's/^/    /' >&2
    fail=$((fail+1))
  fi
fi

echo "$pass passed, $fail failed  ($ran with run fixtures)"
[ "$fail" -eq 0 ]

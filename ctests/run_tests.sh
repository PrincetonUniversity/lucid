#!/usr/bin/env bash
#
# C backend (lucidcc) codegen gate.
#
# For each program in ctests/programs/, generate C with the pcap driver, check it
# COMPILES (gcc -lpcap), and byte-diff the generated C text against the checked-in
# golden in ctests/expected/. A program marked `// LUCIDCC_REJECT: <substr>` is instead
# checked to be *rejected* by lucidcc (it uses a feature the C backend does not support).
#
# This is the fast, local, no-root gate. The pcap *run* fixtures (execute the compiled
# binary + byte-compare output packets) live in ctests/test_pcap; the live-traffic
# driver tests in ctests/test_rawsock and ctests/test_dpdk.
#
# Usage:
#   ./ctests/run_tests.sh             normal: compile + diff each program vs expected/
#   ./ctests/run_tests.sh --update    (re)generate expected/ (compile-checked first)
#   ./ctests/run_tests.sh --no-build  skip `make`, reuse the current ./lucidcc
#   ./ctests/run_tests.sh --help      show this help
#
# Exits non-zero on any failure (CI-friendly). Update mode still fails on a compile error
# or a bad rejection -- it won't record broken C as a golden.
#
# Note: generated C embeds fresh-id temp names (e.g. tmp_4331), deterministic for a fixed
# compiler -- so a compiler change that shifts id allocation legitimately changes the
# output and needs a --update. Inspect the printed diff to confirm a change is intended.
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
  ran=$((ran+1))
  c="$WORK/$name.c"
  log="$WORK/$name.log"
  exp="$EXPECTED_DIR/$name.cc"

  # A program marked `// LUCIDCC_REJECT: <substr>` must be REJECTED by lucidcc (it uses a
  # feature the C backend intentionally does not support). It passes iff lucidcc exits
  # non-zero AND the error mentions <substr>. No golden and no compile for these.
  reject_pat="$(sed -n 's/.*LUCIDCC_REJECT:[[:space:]]*//p' "$prog" | head -1)"
  if [ -n "$reject_pat" ]; then
    if "$LUCIDCC" "$prog" -o "$c" $FLAGS >"$log" 2>&1; then
      echo "FAIL  $name  (expected rejection, but codegen succeeded)"; fail=$((fail+1))
    elif grep -qF "$reject_pat" "$log"; then
      echo "PASS  $name  (correctly rejected: $reject_pat)"; pass=$((pass+1))
    else
      echo "FAIL  $name  (rejected, but error did not mention '$reject_pat')"; fail=$((fail+1))
    fi
    continue
  fi

  # 1. generate C. The compiler chatters on stdout; the artifact is the -o file.
  if ! "$LUCIDCC" "$prog" -o "$c" $FLAGS >"$log" 2>&1; then
    echo "FAIL  $name  (compiler exited non-zero)"; tail -5 "$log" | sed 's/^/      | /'; fail=$((fail+1)); continue
  fi
  [ -f "$c" ] || { echo "FAIL  $name  (no output file produced)"; fail=$((fail+1)); continue; }

  # 2. gcc-compile the generated C. A compile error is always a failure -- and we won't
  #    record broken C as a golden (this runs before the --update copy).
  if ! gcc -o "$WORK/$name.bin" "$c" -lpcap >"$WORK/$name.gcc.log" 2>&1; then
    echo "FAIL  $name  (gcc)"; grep "error:" "$WORK/$name.gcc.log" | sed 's/^/      | /' | head -3; fail=$((fail+1)); continue
  fi

  # 3. golden diff (or, in update mode, record the compile-checked output).
  if [ "$UPDATE" -eq 1 ]; then
    cp "$c" "$exp"; echo "UPDATE  $name"; updated=$((updated+1))
  elif [ ! -f "$exp" ]; then
    echo "FAIL  $name  (no expected file -- run with --update first)"; fail=$((fail+1))
  elif diff -q "$exp" "$c" >/dev/null; then
    echo "PASS  $name"; pass=$((pass+1))
  else
    echo "FAIL  $name  (output differs from expected)"; diff "$exp" "$c" | head -30 | sed 's/^/      | /'; fail=$((fail+1))
  fi
done

echo "-----"
if [ "$ran" -eq 0 ]; then echo "no programs found in $PROGRAMS_DIR" >&2; exit 1; fi
if [ "$UPDATE" -eq 1 ]; then
  echo "updated $updated golden(s); $pass passed, $fail failed"
else
  echo "$pass passed, $fail failed"
fi
[ "$fail" -eq 0 ] || exit 1

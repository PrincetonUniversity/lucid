#!/usr/bin/env python3
"""Run the P4-BMv2 Lucid example tests and check each against expected output.

Each example is run by invoking the Lucid interpreter (`dpt`) on its program
and committed interpreter spec, then comparing the interpreter's stdout against
a stored "expected output" trace. The specs all set `"random seed": 1` so the
output is deterministic across runs.

Usage:
    python test.py                  # run every example, compare vs expected/
    python test.py basic calc       # run only the named examples
    python test.py --expected       # (re)generate expected_output/<name>.out for all
    python test.py --expected calc  # regenerate expected output for one example

Exit status is non-zero if any test fails.
"""

import argparse
import subprocess
import sys
from pathlib import Path

# Resolve everything relative to this script so it works from any CWD.
HERE = Path(__file__).resolve().parent          # examples/p4_bmv2_examples
REPO_ROOT = HERE.parent.parent                  # repo root (holds the dpt binary)
DPT = REPO_ROOT / "dpt"
EXPECTED_DIR = HERE / "expected_output"

PER_TEST_TIMEOUT = 120  # seconds; a generous ceiling so a hang can't wedge CI

# Every example below runs the same way: `dpt <name>.dpt --spec <name>.json
# --silent`, executed from the example's own directory, with stdout being the
# trace we compare. They differ only in the program/spec they point at, so we
# just list the names. (To cover an example with a different command, switch
# this to a list of dicts carrying a per-example `cmd`.)
EXAMPLES = [
    "basic",
    "basic_tunnel",
    "calc",
    "ecn",
    "flowcache",
    "link_monitor",
    "load_balance",
    "mri",
    "multicast",
    "qos",
    "source_routing",
]


def run_example(name):
    """Run one example and return (stdout, stderr, returncode)."""
    workdir = HERE / name
    cmd = [str(DPT), f"{name}.dpt", "--spec", f"{name}.json", "--silent"]
    proc = subprocess.run(
        cmd,
        cwd=workdir,
        capture_output=True,
        text=True,
        timeout=PER_TEST_TIMEOUT,
    )
    return proc.stdout, proc.stderr, proc.returncode


def expected_path(name):
    return EXPECTED_DIR / f"{name}.out"


def generate_expected(names):
    """Run each example and save its stdout as the expected output trace."""
    EXPECTED_DIR.mkdir(exist_ok=True)
    for name in names:
        try:
            stdout, stderr, rc = run_example(name)
        except subprocess.TimeoutExpired:
            print(f"  TIMEOUT  {name}  (exceeded {PER_TEST_TIMEOUT}s) -- not saved")
            continue
        if rc != 0:
            # Don't enshrine a broken run as the expected output.
            print(f"  ERROR    {name}  (dpt exit {rc}) -- not saved")
            if stderr.strip():
                print(_indent(stderr.strip()))
            continue
        expected_path(name).write_text(stdout)
        print(f"  wrote    expected/{name}.out  ({_line_count(stdout)} lines)")


def check_example(name):
    """Run one example and compare to its expected trace. Returns True on pass."""
    exp_file = expected_path(name)
    if not exp_file.exists():
        print(f"  MISSING  {name}  (no expected/{name}.out -- run with --expected)")
        return False
    try:
        stdout, stderr, rc = run_example(name)
    except subprocess.TimeoutExpired:
        print(f"  TIMEOUT  {name}  (exceeded {PER_TEST_TIMEOUT}s)")
        return False

    expected = exp_file.read_text()
    if stdout == expected:
        print(f"  PASS     {name}")
        return True

    print(f"  FAIL     {name}  (output differs from expected/{name}.out)")
    if rc != 0:
        print(f"           dpt exited non-zero ({rc})")
        if stderr.strip():
            print(_indent(stderr.strip()))
    _print_diff(expected, stdout)
    return False


def _print_diff(expected, actual, max_lines=40):
    import difflib

    diff = list(
        difflib.unified_diff(
            expected.splitlines(),
            actual.splitlines(),
            fromfile="expected",
            tofile="actual",
            lineterm="",
        )
    )
    shown = diff[:max_lines]
    print(_indent("\n".join(shown)))
    if len(diff) > max_lines:
        print(f"           ... ({len(diff) - max_lines} more diff lines)")


def _indent(text, prefix="           | "):
    return "\n".join(prefix + line for line in text.splitlines())


def _line_count(text):
    return text.count("\n") + (0 if text.endswith("\n") or not text else 1)


def main():
    parser = argparse.ArgumentParser(
        description="Run the P4-BMv2 Lucid example tests against expected output."
    )
    parser.add_argument(
        "--expected",
        action="store_true",
        help="(re)generate the expected output files instead of checking",
    )
    parser.add_argument(
        "names",
        nargs="*",
        help="examples to run (default: all)",
    )
    args = parser.parse_args()

    if not DPT.exists():
        sys.exit(f"error: dpt binary not found at {DPT}")

    if args.names:
        unknown = [n for n in args.names if n not in EXAMPLES]
        if unknown:
            sys.exit(
                f"error: unknown example(s): {', '.join(unknown)}\n"
                f"known examples: {', '.join(EXAMPLES)}"
            )
        names = args.names
    else:
        names = EXAMPLES

    if args.expected:
        print(f"Generating expected output for {len(names)} example(s):")
        generate_expected(names)
        return

    print(f"Running {len(names)} example test(s):")
    results = [check_example(name) for name in names]
    passed = sum(results)
    failed = len(results) - passed
    print(f"\n{passed} passed, {failed} failed, {len(results)} total")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

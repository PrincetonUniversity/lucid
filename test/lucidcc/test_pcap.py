#!/usr/bin/env python3
"""pcap driver tests: generate the test cases (gen_pcaps.py), then for each program with a
test case, compile it with the pcap driver, run it on _pcaps/<name>.in.pcap, and compare its
output packets with _pcaps/<name>.expected.pcap (and their times, when gen_pcaps.py wrote
the expected file with times). Mismatched outputs are stored as _pcaps/<name>.actual.pcap.
A VARIANTS entry runs a program on a test case of another name, with extra driver arguments."""
import os
import shutil
import subprocess
import sys
import tempfile

import driverlib as dl

# test case -> (program, extra pcap-driver arguments)
VARIANTS = {
    "delay_latency600": ("delay", ["--latency-ns", "600"]),
}

def describe_mismatch(expected, actual):
    if len(expected) != len(actual):
        return f"expected {len(expected)} packets, got {len(actual)}"
    i = next(i for i, (e, a) in enumerate(zip(expected, actual)) if e != a)
    if isinstance(expected[i], tuple):
        return (f"packet {i} differs\nexpected {expected[i][1].hex()} at {expected[i][0]} ns"
                f"\nactual   {actual[i][1].hex()} at {actual[i][0]} ns")
    return f"packet {i} differs\nexpected {expected[i].hex()}\nactual   {actual[i].hex()}"


def check(name, work):
    prog, extra = VARIANTS.get(name, (name, []))
    dpt = os.path.join(dl.PROGRAMS, prog + ".dpt")
    base = os.path.join(dl.PCAPS, name)
    actual_pcap = base + ".actual.pcap"
    if os.path.exists(actual_pcap):
        os.remove(actual_pcap)

    cfile, binfile = os.path.join(work, name + ".c"), os.path.join(work, name)
    gen = dl.run_lucidcc(dpt, "-o", cfile, "--lpcap")
    if gen.returncode != 0:
        return False, "lucidcc failed\n" + dl.tail(gen.stdout, 3)
    cc = dl.run_gcc(cfile, binfile, ["-lpcap"])
    if cc.returncode != 0:
        return False, "gcc failed\n" + dl.gcc_errors(cc.stdout)

    out_pcap = os.path.join(work, name + ".out.pcap")
    try:
        run = subprocess.run([binfile, "--interface", f"1:{base}.in.pcap:{out_pcap}", *extra],
                             capture_output=True, timeout=60)
    except subprocess.TimeoutExpired:
        return False, "run timed out"
    if run.returncode != 0:
        return False, f"run exited {run.returncode}"

    read = dl.read_pcap_records if dl.pcap_has_times(base + ".expected.pcap") else dl.read_pcap
    expected, actual = read(base + ".expected.pcap"), read(out_pcap)
    if expected == actual:
        return True, ""
    shutil.copy(out_pcap, actual_pcap)
    return False, describe_mismatch(expected, actual) + f"\noutput kept as _pcaps/{name}.actual.pcap"


def main():
    dl.local_test_args(__doc__).parse_args()
    dl.build_lucid()
    gen = subprocess.run([sys.executable, os.path.join(dl.SCRIPT_DIR, "gen_pcaps.py")],
                         capture_output=True, text=True)
    if gen.returncode != 0:
        sys.exit("pcap test case generation failed:\n" + gen.stderr)
    def program(name): return VARIANTS.get(name, (name, []))[0]
    names = sorted(f[:-len(".in.pcap")] for f in os.listdir(dl.PCAPS) if f.endswith(".in.pcap")
                   and os.path.exists(os.path.join(dl.PROGRAMS, program(f[:-len(".in.pcap")]) + ".dpt")))
    with tempfile.TemporaryDirectory() as work:
        sys.exit(dl.report((n, *check(n, work)) for n in names))


if __name__ == "__main__":
    main()

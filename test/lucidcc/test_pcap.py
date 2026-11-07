#!/usr/bin/env python3
"""pcap driver tests: generate the fixtures (gen_pcaps.py), then for each program with a
fixture, compile it with the pcap driver, run it on pcaps/<name>.in.pcap, and compare its
output packets with pcaps/<name>.expected.pcap. A mismatched output is kept as
pcaps/<name>.actual.pcap."""
import os
import shutil
import subprocess
import sys
import tempfile

import driverlib as dl


def describe_mismatch(expected, actual):
    if len(expected) != len(actual):
        return f"expected {len(expected)} packets, got {len(actual)}"
    i = next(i for i, (e, a) in enumerate(zip(expected, actual)) if e != a)
    return f"packet {i} differs\nexpected {expected[i].hex()}\nactual   {actual[i].hex()}"


def check(name, work):
    dpt = os.path.join(dl.PROGRAMS, name + ".dpt")
    base = os.path.join(dl.PCAPS, name)
    actual_pcap = base + ".actual.pcap"
    if os.path.exists(actual_pcap):
        os.remove(actual_pcap)

    cfile, binfile = os.path.join(work, name + ".c"), os.path.join(work, name)
    gen = dl.run_lucidcc(dpt, cfile, "--lpcap")
    if gen.returncode != 0:
        return False, "lucidcc failed\n" + dl.tail(gen.stdout, 3)
    cc = dl.run_gcc(cfile, binfile, ["-lpcap"])
    if cc.returncode != 0:
        return False, "gcc failed\n" + dl.gcc_errors(cc.stdout)

    out_pcap = os.path.join(work, name + ".out.pcap")
    try:
        run = subprocess.run([binfile, "--interface", f"1:{base}.in.pcap:{out_pcap}"],
                             capture_output=True, timeout=60)
    except subprocess.TimeoutExpired:
        return False, "run timed out"
    if run.returncode != 0:
        return False, f"run exited {run.returncode}"

    expected, actual = dl.read_pcap(base + ".expected.pcap"), dl.read_pcap(out_pcap)
    if expected == actual:
        return True, ""
    shutil.copy(out_pcap, actual_pcap)
    return False, describe_mismatch(expected, actual) + f"\noutput kept as pcaps/{name}.actual.pcap"


def main():
    args = dl.local_test_args(__doc__).parse_args()
    dl.prepare(args)
    gen = subprocess.run([sys.executable, os.path.join(dl.SCRIPT_DIR, "gen_pcaps.py")],
                         capture_output=True, text=True)
    if gen.returncode != 0:
        sys.exit("pcap fixture generation failed:\n" + gen.stderr)
    names = sorted(f[:-len(".in.pcap")] for f in os.listdir(dl.PCAPS) if f.endswith(".in.pcap")
                   and os.path.exists(os.path.join(dl.PROGRAMS, f[:-len(".in.pcap")] + ".dpt")))
    with tempfile.TemporaryDirectory() as work:
        sys.exit(dl.report((n, *check(n, work)) for n in names))


if __name__ == "__main__":
    main()

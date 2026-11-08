#!/usr/bin/env python3
"""Compiler tests: generate C for each programs/*.dpt (pcap driver), check that it compiles
with gcc, and diff its "program code" section (the part every driver
shares) against expected/<name>.cc. A program marked `// LUCIDCC_REJECT: <substr>` must
instead be rejected with an error containing <substr>."""
import difflib
import glob
import os
import re
import sys
import tempfile

import driverlib as dl

EXPECTED = os.path.join(dl.SCRIPT_DIR, "expected")


def reject_pattern(dpt):
    m = re.search(r"LUCIDCC_REJECT:\s*(.*)", open(dpt).read())
    return m.group(1).strip() if m else None


def check(dpt, work, update):
    name = os.path.splitext(os.path.basename(dpt))[0]
    cfile = os.path.join(work, name + ".c")
    gen = dl.run_lucidcc(dpt, "-o", cfile, "--lpcap")

    pat = reject_pattern(dpt)
    if pat:
        if gen.returncode == 0:
            return False, "expected rejection, but compilation succeeded"
        if pat not in gen.stdout:
            return False, f"rejected, but error did not mention '{pat}'"
        return True, f"correctly rejected: {pat}"

    if gen.returncode != 0:
        return False, "lucidcc failed\n" + dl.tail(gen.stdout, 5)
    cc = dl.run_gcc(cfile, os.path.join(work, name), ["-lpcap"])
    if cc.returncode != 0:
        return False, "gcc failed\n" + dl.gcc_errors(cc.stdout)

    exp = os.path.join(EXPECTED, name + ".cc")
    actual = dl.program_section(open(cfile).read())
    if update:
        open(exp, "w").writelines(actual)
        return True, "updated"
    if not os.path.exists(exp):
        return False, "no expected file (run with --update)"
    expected = open(exp).readlines()
    if expected != actual:
        diff = difflib.unified_diff(expected, actual, "expected", "actual", n=1)
        return False, "output differs from expected\n" + "".join(list(diff)[:30])
    return True, ""


def main():
    ap = dl.local_test_args(__doc__)
    ap.add_argument("--update", action="store_true",
                    help="accept the generated C as expected/ (gcc-checked first)")
    args = ap.parse_args()
    dl.build_lucid()
    os.makedirs(EXPECTED, exist_ok=True)
    with tempfile.TemporaryDirectory() as work:
        progs = sorted(glob.glob(os.path.join(dl.PROGRAMS, "*.dpt")))
        results = ((os.path.basename(p)[:-4], *check(p, work, args.update)) for p in progs)
        sys.exit(dl.report(results))


if __name__ == "__main__":
    main()

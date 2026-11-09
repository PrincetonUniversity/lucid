#!/usr/bin/env python3
"""ltf: the Lucid test framework runner (docs/design_notes/03.lucid-test-framework.md).

A test case is an interpreter spec (one node) plus an "expected" block that maps each
output port to its ordered outputs, given as events ({"name", "args"}) or hex bytes
({"bytes"}). The runner encodes the spec with lucidEncode, runs it on a target, and
compares the wire bytes leaving each port, in order. Extra outputs fail the test.
Order across ports and timing are not compared.

  ltf.py run PROGRAM SPEC [--target interp] [--work DIR]
  ltf.py suite DIR [--target interp] [-k NAME]   # DIR/cases/<prog>[.<variant>].json
                                                 # against DIR/programs/<prog>.dpt
Targets: interp (the interpreter's simulator, driven directly by the spec).
"""
import argparse
import difflib
import glob
import json
import os
import shutil
import struct
import subprocess
import sys
import tempfile

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))


# ---- tools ---------------------------------------------------------------------

def find_tool(name, override):
    """a tool binary: the --flag path, else on PATH, else at the root of the git
    repo this script is in"""
    if override:
        return os.path.abspath(override)
    on_path = shutil.which(name)
    if on_path:
        return on_path
    r = subprocess.run(["git", "rev-parse", "--show-toplevel"], cwd=SCRIPT_DIR,
                       capture_output=True, text=True)
    if r.returncode == 0:
        candidate = os.path.join(r.stdout.strip(), name)
        if os.path.exists(candidate):
            return candidate
    sys.exit(f"ltf: can't find {name} (not on PATH or at the repo root; pass --{name.lower()})")


class TestError(Exception):
    """a test that couldn't run (as opposed to one whose outputs differ)"""


def run_tool(cmd, what):
    r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0:
        tail = "\n".join((r.stdout + r.stderr).strip().splitlines()[-8:])
        raise TestError(f"{what} failed (exit {r.returncode}):\n{tail}")
    return r


# ---- pcaps (nanosecond, ethernet) ------------------------------------------------

NS_MAGIC = 0xa1b23c4d
US_MAGIC = 0xa1b2c3d4

def write_pcap(path, records):
    """records: (time_ns, frame bytes)"""
    with open(path, "wb") as f:
        f.write(struct.pack("<IHHiIII", NS_MAGIC, 2, 4, 0, 0, 65535, 1))
        for t, frame in records:
            f.write(struct.pack("<IIII", t // 10**9, t % 10**9, len(frame), len(frame)))
            f.write(frame)


def read_pcap(path):
    """[(time_ns, frame bytes)]; reads micro- and nanosecond pcaps"""
    data = open(path, "rb").read()
    magic = struct.unpack("<I", data[:4])[0]
    if magic not in (NS_MAGIC, US_MAGIC):
        raise TestError(f"{path}: not a little-endian pcap")
    scale = 1 if magic == NS_MAGIC else 1000
    out, off = [], 24
    while off + 16 <= len(data):
        sec, frac, incl, _ = struct.unpack("<IIII", data[off:off + 16])
        out.append((sec * 10**9 + frac * scale, data[off + 16:off + 16 + incl]))
        off += 16 + incl
    return out


# ---- outputs ---------------------------------------------------------------------
# An output is a dict with "wire_bytes" (hex), and "name"/"args" when known.

def describe(out):
    if "name" in out:
        return f"{out['name']}({', '.join(json.dumps(a) for a in out.get('args', []))})"
    return out["wire_bytes"]


def json_records(text):
    """the JSON objects in a stream of (pretty-printed) records; skips other lines"""
    dec, i, out = json.JSONDecoder(), 0, []
    while i < len(text):
        if text[i].isspace():
            i += 1
            continue
        try:
            obj, i = dec.raw_decode(text, i)
            out.append(obj)
        except json.JSONDecodeError:
            nl = text.find("\n", i)
            i = len(text) if nl < 0 else nl + 1
    return out


# ---- targets ---------------------------------------------------------------------
# A target runs the case and returns ({port: [output]}, [printf line]), writing
# out_<port>.pcap per output port into its work dir.

def target_interp(tools, program, spec, work):
    r = run_tool([tools["dpt"], "--json", program, "--spec", spec], "dpt")
    with open(os.path.join(work, "dpt.out"), "w") as f:
        f.write(r.stdout)
    outputs, timed, prints = {}, {}, []
    for rec in json_records(r.stdout):
        if "event_exit" in rec:
            ex = rec["event_exit"]
            sw, port = ex["locations"][0].split(":")
            port = int(port)
            out = {k: ex[k] for k in ("name", "args") if k in ex}
            out["wire_bytes"] = ex["wire_bytes"]
            outputs.setdefault(port, []).append(out)
            timed.setdefault(port, []).append((ex["timestamp"], bytes.fromhex(ex["wire_bytes"])))
        elif "printf" in rec:
            prints.append(rec["printf"])
    for port, records in timed.items():
        write_pcap(os.path.join(work, f"out_{port}.pcap"), records)
    return outputs, prints


TARGETS = {"interp": target_interp}


# ---- one test --------------------------------------------------------------------

def check_spec(spec):
    return json.load(open(spec))


def compare(expected, actual):
    """expected, actual: {port: [output]}; returns a list of failure lines"""
    lines = []
    for port in sorted(set(expected) | set(actual)):
        exp, act = expected.get(port, []), actual.get(port, [])
        if [o["wire_bytes"] for o in exp] == [o["wire_bytes"] for o in act]:
            continue
        lines.append(f"port {port}: expected {len(exp)} output(s), got {len(act)}")
        # diff the descriptions, falling back to bytes where they describe the same
        exp_d = [f"{describe(o)}  {o['wire_bytes']}" for o in exp]
        act_d = [f"{describe(o)}  {o['wire_bytes']}" for o in act]
        for d in list(difflib.unified_diff(exp_d, act_d, "expected", "actual", n=2, lineterm=""))[2:]:
            lines.append("  " + d)
    return lines


def run_case(tools, program, spec, target, work):
    """returns (ok, detail lines)"""
    os.makedirs(work, exist_ok=True)
    s = check_spec(spec)
    if "expected" not in s:
        raise TestError("spec has no \"expected\" block (an agreement test needs a second target)")
    enc = os.path.join(work, "encoded")
    run_tool([tools["lucidEncode"], program, "--spec", spec, "-o", enc], "lucidEncode")
    expected = {int(p): outs for p, outs in
                json.load(open(os.path.join(enc, "expected.json"))).items()}
    tdir = os.path.join(work, target)
    os.makedirs(tdir, exist_ok=True)
    actual, prints = TARGETS[target](tools, program, spec, tdir)
    with open(os.path.join(tdir, "prints.txt"), "w") as f:
        f.writelines(p + "\n" for p in prints)
    diffs = compare(expected, actual)
    return not diffs, diffs


# ---- commands --------------------------------------------------------------------

def cases_in(suite_dir):
    """[(name, program, spec)] for DIR/cases/<prog>[.<variant>].json"""
    out = []
    for spec in sorted(glob.glob(os.path.join(suite_dir, "cases", "*.json"))):
        name = os.path.basename(spec)[:-len(".json")]
        program = os.path.join(suite_dir, "programs", name.split(".")[0] + ".dpt")
        out.append((name, program, spec))
    return out


def report(name, ok, detail, work):
    print(f"{'PASS' if ok else 'FAIL'}  {name}" + ("" if ok else f"  (work dir: {work})"))
    for line in detail:
        print("      | " + line)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--dpt", help="path to the interpreter (dpt)")
    ap.add_argument("--lucidencode", help="path to lucidEncode")
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run", help="run one test case")
    r.add_argument("program")
    r.add_argument("spec")
    r.add_argument("--target", choices=TARGETS, default="interp")
    r.add_argument("--work", help="work dir (default: a temporary dir, kept on failure)")
    s = sub.add_parser("suite", help="run every case in a directory")
    s.add_argument("dir")
    s.add_argument("--target", choices=TARGETS, default="interp")
    s.add_argument("-k", dest="only", help="only cases whose name contains this")
    s.add_argument("--work", help="work dir root (default: DIR/_work)")
    args = ap.parse_args()

    tools = {"dpt": find_tool("dpt", args.dpt),
             "lucidEncode": find_tool("lucidEncode", args.lucidencode)}

    if args.cmd == "run":
        cases = [(os.path.basename(args.spec)[:-len(".json")], args.program, args.spec)]
        root = args.work or tempfile.mkdtemp(prefix="ltf-")
        keep_passing = bool(args.work)
    else:
        cases = [c for c in cases_in(args.dir) if not args.only or args.only in c[0]]
        if not cases:
            sys.exit(f"ltf: no cases in {os.path.join(args.dir, 'cases')}")
        root = args.work or os.path.join(args.dir, "_work")
        keep_passing = True
    passed = failed = 0
    for name, program, spec in cases:
        work = os.path.join(root, name) if args.cmd == "suite" else root
        if args.cmd == "suite" and os.path.exists(work):
            shutil.rmtree(work)
        try:
            ok, detail = run_case(tools, program, spec, args.target, work)
        except TestError as e:
            ok, detail = False, str(e).splitlines()
        report(name, ok, detail, work)
        passed, failed = passed + ok, failed + (not ok)
        if ok and not keep_passing:
            shutil.rmtree(work, ignore_errors=True)
    print("-----")
    print(f"{passed} passed, {failed} failed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""ltf: the Lucid test framework runner (docs/design_notes/03.lucid-test-framework.md).

A test case is an interpreter spec (one node) plus an "expected" block that maps each
output port to its ordered outputs, given as events ({"name", "args"}) or hex bytes
({"bytes"}). The runner encodes the spec with lucidEncode, runs it on a target, and
compares the wire bytes leaving each port, in order. Extra outputs fail the test.
Order across ports and timing are not compared.

  ltf.py run PROGRAM SPEC [--target T] [--work DIR]
  ltf.py suite DIR [--target T] [-k NAME]   # DIR/cases/<prog>[.<variant>].json
                                            # against DIR/programs/<prog>.dpt
Targets:
  interp  the interpreter's simulator, driven directly by the spec
  dpdk    the lucidcc --dpdk driver on net_af_packet veths, run by ltf_dpdk.py in the
          docker/dpdk image (--dpdk-run native: run it here, on Linux as root). The
          inputs are replayed at their timestamps, every port in the case is bound,
          and any driver drop fails the test.
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

def repo_root():
    r = subprocess.run(["git", "rev-parse", "--show-toplevel"], cwd=SCRIPT_DIR,
                       capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else None


def find_tool(name, override, in_repo=None):
    """a tool: the --flag path, else on PATH, else at in_repo (default: name) in the
    git repo this script is in"""
    if override:
        return os.path.abspath(override)
    on_path = shutil.which(name)
    if on_path:
        return on_path
    root = repo_root()
    if root:
        candidate = os.path.join(root, in_repo or name)
        if os.path.exists(candidate):
            return candidate
    sys.exit(f"ltf: can't find {name} (not on PATH or in the repo; pass --{name.lower()})")


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

LUCID_HDR_HEX = 32   # a background event's frame: dst, src, ethertype 0x029a, then a 16-bit tag

def describe(out, known=()):
    """an output's name and args. For an output that only has bytes: the name of a
    known (expected) background event with the same header and tag, if any."""
    if "name" in out:
        return f"{out['name']}({', '.join(json.dumps(a) for a in out.get('args', []))})"
    hdr = out["wire_bytes"][:LUCID_HDR_HEX]
    if hdr[24:28] == "029a":
        for o in known:
            if "name" in o and o["wire_bytes"][:LUCID_HDR_HEX] == hdr:
                return f"{o['name']}(?)"
    return "(bytes)"


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
# A target runs prepared cases. prepare(case) is per case (case: a Case); run(cases)
# runs them all and returns, per case, ({port: [output]}, [printf line], [failure
# line]) or raises TestError. Each target writes out_<port>.pcap per output port, and
# its tool output, into the case's <work>/<target> dir.

class Case:
    def __init__(self, name, program, spec, work):
        self.name, self.program, self.spec, self.work = name, program, spec, work
        self.expected = None   # {port: [output]}, from lucidEncode
        self.error = None      # a TestError, once the case can't go on


class Interp:
    name = "interp"

    def __init__(self, tools, args):
        self.dpt = tools["dpt"]

    def prepare(self, case):
        pass

    def run(self, cases):
        return {c.name: self.run_one(c) for c in cases}

    def run_one(self, case):
        work = os.path.join(case.work, self.name)
        try:
            r = run_tool([self.dpt, "--json", case.program, "--spec", case.spec], "dpt")
        except TestError as e:
            return e
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
        return outputs, prints, []


class Dpdk:
    name = "dpdk"
    EXECUTOR = os.path.join(SCRIPT_DIR, "ltf_dpdk.py")

    def __init__(self, tools, args):
        self.lucidcc = find_tool("lucidcc", args.lucidcc)
        if args.dpdk_run == "native":
            self.runner = None
        else:
            self.runner = find_tool("run.sh", args.dpdk_run, in_repo="docker/dpdk/run.sh")

    def prepare(self, case):
        """generate the C build dir, and the port map: every port the case uses
        (declared, input or expected) except recirculation ports"""
        work = os.path.join(case.work, self.name)
        run_tool([self.lucidcc, case.program, "--dpdk", "--build", os.path.join(work, "build")],
                 "lucidcc --dpdk")
        spec = json.load(open(case.spec))
        declared = spec.get("topology", {}).get("nodes", {}).get("0", {}).get("ports", {})
        recirc = {int(p) for p, d in declared.items() if d.get("type") == "recirc"}
        inputs = {int(os.path.basename(f)[3:-5])
                  for f in glob.glob(os.path.join(case.work, "encoded", "in_*.pcap"))}
        ports = sorted(({int(p) for p in declared} | inputs | set(case.expected)) - recirc)
        with open(os.path.join(work, "ports.json"), "w") as f:
            json.dump({"ports": ports}, f)

    def run(self, cases):
        """one executor run for every case: in a container, the program builds are slow"""
        root = os.path.commonpath([os.path.abspath(c.work) for c in cases])
        shutil.copy(self.EXECUTOR, os.path.join(root, "ltf_dpdk.py"))
        rel = [os.path.relpath(os.path.abspath(c.work), root) for c in cases]
        if self.runner:
            cmd = [self.runner, root, "python3", "ltf_dpdk.py", *rel]
        else:
            cmd = (["sudo"] if os.geteuid() != 0 else []) + ["python3", "ltf_dpdk.py", *rel]
        print(f"ltf: running {len(cases)} case(s) on dpdk ...", flush=True)
        r = subprocess.run(cmd, cwd=root, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        with open(os.path.join(root, "ltf_dpdk.log"), "w") as f:
            f.write(r.stdout)
        if r.returncode != 0:
            tail = "\n".join(r.stdout.strip().splitlines()[-8:])
            err = TestError(f"the dpdk executor failed (exit {r.returncode}):\n{tail}")
            return {c.name: err for c in cases}
        return {c.name: self.collect(c) for c in cases}

    def collect(self, case):
        work = os.path.join(case.work, self.name)
        result_f = os.path.join(work, "result.json")
        if not os.path.exists(result_f):
            return TestError("the dpdk executor left no result.json")
        result = json.load(open(result_f))
        if "error" in result:
            return TestError("dpdk: " + result["error"])
        problems = [f"dpdk dropped {n} ({cause})" for cause, n in result["drops"].items() if n]
        outputs = {}
        for port in json.load(open(os.path.join(work, "ports.json")))["ports"]:
            path = os.path.join(work, f"out_{port}.pcap")
            frames = [f for _, f in read_pcap(path)] if os.path.exists(path) else []
            if frames:
                outputs[port] = [{"wire_bytes": f.hex()} for f in frames]
        prints = open(os.path.join(work, "switch.stdout")).read().splitlines()
        return outputs, prints, problems


TARGETS = {"interp": Interp, "dpdk": Dpdk}


# ---- comparison ------------------------------------------------------------------

def compare(expected, actual):
    """expected, actual: {port: [output]}; returns a list of failure lines. Outputs
    without a name (a target that only sees bytes) are described by the expected
    output with the same bytes, if there is one."""
    named = [o for outs in expected.values() for o in outs if "name" in o]
    known = {o["wire_bytes"]: o for o in named}
    lines = []
    for port in sorted(set(expected) | set(actual)):
        exp, act = expected.get(port, []), actual.get(port, [])
        if [o["wire_bytes"] for o in exp] == [o["wire_bytes"] for o in act]:
            continue
        lines.append(f"port {port}: expected {len(exp)} output(s), got {len(act)}")
        exp_d = [f"{describe(o)}  {o['wire_bytes']}" for o in exp]
        act_d = [f"{describe(known.get(o['wire_bytes'], o), named)}  {o['wire_bytes']}" for o in act]
        for d in list(difflib.unified_diff(exp_d, act_d, "expected", "actual", n=2, lineterm=""))[2:]:
            lines.append("  " + d)
    return lines


def prepare(tools, target, case):
    """encode the case, then the target's own preparation"""
    s = json.load(open(case.spec))
    if "expected" not in s:
        raise TestError("spec has no \"expected\" block (an agreement test needs a second target)")
    enc = os.path.join(case.work, "encoded")
    run_tool([tools["lucidEncode"], case.program, "--spec", case.spec, "-o", enc], "lucidEncode")
    case.expected = {int(p): outs for p, outs in
                     json.load(open(os.path.join(enc, "expected.json"))).items()}
    os.makedirs(os.path.join(case.work, target.name), exist_ok=True)
    target.prepare(case)


def check(target, case, result):
    """(ok, detail lines)"""
    if isinstance(result, TestError):
        return False, str(result).splitlines()
    actual, prints, problems = result
    with open(os.path.join(case.work, target.name, "prints.txt"), "w") as f:
        f.writelines(p + "\n" for p in prints)
    lines = problems + compare(case.expected, actual)
    return not lines, lines


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
    ap.add_argument("--lucidcc", help="path to lucidcc (dpdk target)")
    ap.add_argument("--dpdk-run", help="dpdk target: the container runner (default: "
                    "docker/dpdk/run.sh in this repo), or 'native' to run here")
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
    target = TARGETS[args.target](tools, args)

    if args.cmd == "run":
        root = os.path.abspath(args.work or tempfile.mkdtemp(prefix="ltf-"))
        cases = [Case(os.path.basename(args.spec)[:-len(".json")], args.program, args.spec, root)]
        keep_passing = bool(args.work)
    else:
        root = os.path.abspath(args.work or os.path.join(args.dir, "_work"))
        cases = [Case(n, p, s, os.path.join(root, n))
                 for n, p, s in cases_in(args.dir) if not args.only or args.only in n]
        if not cases:
            sys.exit(f"ltf: no cases in {os.path.join(args.dir, 'cases')}")
        keep_passing = True
    for case in cases:
        if args.cmd == "suite" and os.path.exists(case.work):
            shutil.rmtree(case.work)
        os.makedirs(case.work, exist_ok=True)
        try:
            prepare(tools, target, case)
        except TestError as e:
            case.error = e
    ready = [c for c in cases if c.error is None]
    results = target.run(ready) if ready else {}
    passed = failed = 0
    for case in cases:
        ok, detail = check(target, case, case.error or results[case.name])
        report(case.name, ok, detail, case.work)
        passed, failed = passed + ok, failed + (not ok)
        if ok and not keep_passing:
            shutil.rmtree(case.work, ignore_errors=True)
    print("-----")
    print(f"{passed} passed, {failed} failed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

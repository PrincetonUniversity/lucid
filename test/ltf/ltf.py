#!/usr/bin/env python3
"""ltf: the Lucid test framework runner (docs/design_notes/03.lucid-test-framework.md).

A test case is an interpreter spec (one node) plus an "expected" block that maps each
output port to its ordered outputs, given as events ({"name", "args"}) or hex bytes
({"bytes"}). The runner encodes the spec with lucidEncode, runs it on a target, and
compares the wire bytes leaving each port, in order. Extra outputs fail the test.
Order across ports and timing are not compared.

  ltf.py run PROGRAM SPEC [--target T[,T...]] [--work DIR]
  ltf.py suite DIR [--target T[,T...]] [-k NAME] [--programs PDIR]
        # DIR/cases/<prog>[.<variant>].json against PDIR/<prog>.dpt (default PDIR: DIR/programs)

When there are multiple targets, the case's expected block is checked on each one.
A case with no expected block is a compiler validation test: the outputs and printf
lines of the first target (the reference, normally interp) are what the rest must match.
With one target, compiler validation tests are skipped.

Targets:
  interp  the interpreter's simulator, driven directly by the spec
  dpdk    programs compiled with lucidcc --dpdk driver, run in the docker/dpdk
          image on net_af_packet veths. Uses ltf_dpdk.py.
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
        self.expected = None   # {port: [output]}, from lucidEncode; None: a compiler validation test
        self.out_ports = set() # the ports outputs are expected on (expected, or the reference's)
        self.error = None      # a TestError, once the case can't go on
        self.results = {}      # target name -> a target's result, or a TestError


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
        ports = sorted(({int(p) for p in declared} | inputs | case.out_ports) - recirc)
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
            return TestError(result["error"])
        # a parse drop can be the program's own (a parser's `drop`), so it isn't a
        # failure by itself: a wrong one shows up as missing outputs or prints
        problems = [f"dpdk dropped {n} ({cause})" for cause, n in result["drops"].items()
                    if n and cause != "parse"]
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

def compare(expected, actual, labels=("expected", "actual")):
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
        lines.append(f"port {port}: {labels[0]} {len(exp)} output(s), {labels[1]} {len(act)}")
        exp_d = [f"{describe(o)}  {o['wire_bytes']}" for o in exp]
        act_d = [f"{describe(known.get(o['wire_bytes'], o), named)}  {o['wire_bytes']}" for o in act]
        for d in list(difflib.unified_diff(exp_d, act_d, *labels, n=2, lineterm=""))[2:]:
            lines.append("  " + d)
    return lines


def encode(tools, case):
    """encode the case's inputs (and its expected outputs, if any)"""
    s = json.load(open(case.spec))
    enc = os.path.join(case.work, "encoded")
    run_tool([tools["lucidEncode"], case.program, "--spec", case.spec, "-o", enc], "lucidEncode")
    if "expected" in s:
        case.expected = {int(p): outs for p, outs in
                         json.load(open(os.path.join(enc, "expected.json"))).items()}
        case.out_ports = set(case.expected)


def outcome(target, case):
    """a target's result for a case: (outputs, prints, problems), or a TestError;
    writes its prints"""
    result = case.results[target.name]
    if isinstance(result, TestError):
        return result
    outputs, prints, problems = result
    with open(os.path.join(case.work, target.name, "prints.txt"), "w") as f:
        f.writelines(p + "\n" for p in prints)
    return result


def compare_prints(ref_prints, prints, labels):
    """the printf lines, in order; returns a list of failure lines"""
    if ref_prints == prints:
        return []
    lines = [f"prints: {labels[0]} {len(ref_prints)} line(s), {labels[1]} {len(prints)}"]
    for d in list(difflib.unified_diff(ref_prints, prints, *labels, n=2, lineterm=""))[2:]:
        lines.append("  " + d)
    return lines


def checks(targets, case):
    """[(label, ok, detail lines)] for a case: one per target if it has expected
    outputs, else one per non-reference target"""
    if case.error:
        return [(case.name, False, str(case.error).splitlines())]
    if case.expected is not None:
        out = []
        for t in targets:
            r = outcome(t, case)
            lines = str(r).splitlines() if isinstance(r, TestError) else r[2] + compare(case.expected, r[0])
            out.append((f"{case.name} [{t.name}]", not lines, lines))
        return out
    if len(targets) < 2:
        return [(case.name, None, ["compiler validation test (no \"expected\"): needs two targets"])]
    ref = targets[0]
    ref_r = outcome(ref, case)
    out = []
    for t in targets[1:]:
        label = f"{case.name} [{t.name} = {ref.name}]"
        r = outcome(t, case)
        if isinstance(ref_r, TestError) or isinstance(r, TestError):
            errs = [f"{x.name}: {e}" for x, e in ((ref, ref_r), (t, r)) if isinstance(e, TestError)]
            out.append((label, False, "\n".join(errs).splitlines()))
        else:
            labels = (ref.name, t.name)
            lines = (ref_r[2] + r[2] + compare(ref_r[0], r[0], labels)
                     + compare_prints(ref_r[1], r[1], labels))
            out.append((label, not lines, lines))
    return out


# ---- commands --------------------------------------------------------------------

def cases_in(suite_dir, programs_dir):
    """[(name, program, spec)] for DIR/cases/<prog>[.<variant>].json, with
    programs_dir/<prog>.dpt"""
    out = []
    for spec in sorted(glob.glob(os.path.join(suite_dir, "cases", "*.json"))):
        name = os.path.basename(spec)[:-len(".json")]
        program = os.path.join(programs_dir, name.split(".")[0] + ".dpt")
        out.append((name, program, spec))
    return out


def report(label, ok, detail, work):
    status = {True: "PASS", False: "FAIL", None: "SKIP"}[ok]
    print(f"{status}  {label}" + (f"  (work dir: {work})" if ok is False else ""))
    for line in detail:
        print("      | " + line)


def target_list(spec):
    names = spec.split(",")
    for n in names:
        if n not in TARGETS:
            sys.exit(f"ltf: unknown target {n} (targets: {', '.join(TARGETS)})")
    if len(set(names)) != len(names):
        sys.exit("ltf: a target is listed twice")
    return names


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
    r.add_argument("--target", default="interp", help="target(s), comma-separated")
    r.add_argument("--work", help="work dir (default: a temporary dir, kept on failure)")
    s = sub.add_parser("suite", help="run every case in a directory")
    s.add_argument("dir")
    s.add_argument("--target", default="interp", help="target(s), comma-separated")
    s.add_argument("-k", dest="only", help="only cases whose name contains this")
    s.add_argument("--work", help="work dir root (default: DIR/_work)")
    s.add_argument("--programs", help="the programs dir (default: DIR/programs)")
    args = ap.parse_args()

    tools = {"dpt": find_tool("dpt", args.dpt),
             "lucidEncode": find_tool("lucidEncode", args.lucidencode)}
    targets = [TARGETS[n](tools, args) for n in target_list(args.target)]

    if args.cmd == "run":
        root = os.path.abspath(args.work or tempfile.mkdtemp(prefix="ltf-"))
        cases = [Case(os.path.basename(args.spec)[:-len(".json")], args.program, args.spec, root)]
        keep_passing = bool(args.work)
    else:
        root = os.path.abspath(args.work or os.path.join(args.dir, "_work"))
        cases = [Case(n, p, s, os.path.join(root, n))
                 for n, p, s in cases_in(args.dir, args.programs or os.path.join(args.dir, "programs"))
                 if not args.only or args.only in n]
        if not cases:
            sys.exit(f"ltf: no cases in {os.path.join(args.dir, 'cases')}")
        keep_passing = True
    for case in cases:
        if args.cmd == "suite" and os.path.exists(case.work):
            shutil.rmtree(case.work)
        os.makedirs(case.work, exist_ok=True)
        try:
            encode(tools, case)
        except TestError as e:
            case.error = e
    # compiler validation tests run only when there's more than one target
    runnable = [c for c in cases if c.error is None and (c.expected is not None or len(targets) > 1)]
    for i, t in enumerate(targets):
        ready = []
        for c in runnable:
            if c.expected is None and i > 0:
                # bind the ports the reference sent to (it has no "expected" to say)
                ref = c.results.get(targets[0].name)
                c.out_ports = set(ref[0]) if isinstance(ref, tuple) else set()
            os.makedirs(os.path.join(c.work, t.name), exist_ok=True)
            try:
                t.prepare(c)
                ready.append(c)
            except TestError as e:
                c.results[t.name] = e
        for name, result in (t.run(ready) if ready else {}).items():
            next(c for c in ready if c.name == name).results[t.name] = result
    passed = failed = skipped = 0
    for case in cases:
        results = checks(targets, case)
        for label, ok, detail in results:
            report(label, ok, detail, case.work)
            passed, failed, skipped = passed + (ok is True), failed + (ok is False), skipped + (ok is None)
        if all(ok is not False for _, ok, _ in results) and not keep_passing:
            shutil.rmtree(case.work, ignore_errors=True)
    print("-----")
    print(f"{passed} passed, {failed} failed" + (f", {skipped} skipped" if skipped else ""))
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

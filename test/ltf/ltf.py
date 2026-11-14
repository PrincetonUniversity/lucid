#!/usr/bin/env python3
"""ltf: test case runner for lucidcc (and later, other compiler backends)

Usage: 

ltf.py run PROGRAM SPEC [--test T[,T...]] [--validate T[,T...]] [--work DIR]

--test runs a test case defined as a single-node interpreter spec that MUST 
include an expected block that defines an ordered list of expected outputs 
for each output port.

--validate compares the interpreter's output with a target's output. It 
ignores the expected block.

ltf.py suite PROGRAMS_DIR CASES_DIR [--test T[,T...]] [--validate T[,T...]] [-k NAME] [--work DIR]

Run a suite of test cases in CASES_DIR. The name of the test case, up to the first ".", 
is used to find the name of the program to test. 

Output is compared per-port. Order is considered but timestamps are not. 

Targets:
  interp  the interpreter
  dpdk    lucidcc --dpdk (run in the docker/dpdk container on af_packet veths with run_dpdk_test.py)
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


def find_file(name):
    """find the path to the executable `name`: at git repo root of this script, else on PATH"""
    root = repo_root()
    if root and os.path.exists(os.path.join(root, name)):
        return os.path.join(root, name)
    on_path = shutil.which(name)
    if on_path:
        return on_path
    sys.exit(f"ltf: can't find {name} (not in the repo or on PATH; pass --{name.lower()})")


def container_runner():
    """call docker/dpdk/run.sh to run a command in the DPDK container"""
    root = repo_root()
    runner = os.path.join(root, "docker", "dpdk", "run.sh") if root else None
    if not runner or not os.path.exists(runner):
        sys.exit("ltf: can't find docker/dpdk/run.sh in this repo")
    return runner


class TestError(Exception):
    """a test that couldn't run"""

def run_tool(cmd, name):
    r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0:
        tail = "\n".join((r.stdout + r.stderr).strip().splitlines()[-8:])
        raise TestError(f"{name} failed (exit {r.returncode}):\n{tail}")
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

LUCID_HDR_HEXLEN = 32

def out_event_str(out, known=()):
    """get an output event's name and args. If the event doesn't have a name, 
    try to infer one from a list of known (expected) background events."""
    if "name" in out:
        return f"{out['name']}({', '.join(json.dumps(a) for a in out.get('args', []))})"
    hdr = out["wire_bytes"][:LUCID_HDR_HEXLEN]
    if hdr[24:28] == "029a":
        for o in known:
            if "name" in o and o["wire_bytes"][:LUCID_HDR_HEXLEN] == hdr:
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
        self.expected = None   # {port: [output]}, from lucidEncode; None: no expected block
        self.out_ports = set() # the ports outputs are expected on (expected, and the reference's)
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
    EXECUTOR = os.path.join(SCRIPT_DIR, "run_dpdk_test.py")

    def __init__(self, tools, args):
        self.lucidcc = args.lucidcc or find_file("lucidcc")
        if args.no_container:
            self.runner = None
        else:
            self.runner = container_runner()

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
        shutil.copy(self.EXECUTOR, os.path.join(root, "run_dpdk_test.py"))
        rel = [os.path.relpath(os.path.abspath(c.work), root) for c in cases]
        if self.runner:
            cmd = [self.runner, root, "python3", "run_dpdk_test.py", *rel]
        else:
            cmd = (["sudo"] if os.geteuid() != 0 else []) + ["python3", "run_dpdk_test.py", *rel]
        print(f"ltf: running {len(cases)} case(s) on dpdk ...", flush=True)
        r = subprocess.run(cmd, cwd=root, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        with open(os.path.join(root, "run_dpdk_test.log"), "w") as f:
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
    """compare expected and observed outputs. Logic is a bit convoluted, eventual TODO cleanup."""
    named = [o for outs in expected.values() for o in outs if "name" in o]
    known = {o["wire_bytes"]: o for o in named}
    lines = []
    for port in sorted(set(expected) | set(actual)):
        exp, act = expected.get(port, []), actual.get(port, [])
        if [o["wire_bytes"] for o in exp] == [o["wire_bytes"] for o in act]:
            continue
        lines.append(f"port {port}: {labels[0]} {len(exp)} output(s), {labels[1]} {len(act)}")
        exp_d = [f"{out_event_str(o)}  {o['wire_bytes']}" for o in exp]
        act_d = []
        for o in act:
            if o["wire_bytes"] in known:
                shown = known[o["wire_bytes"]]   # a named expected output with the same bytes
            else:
                shown = o
            act_d.append(f"{out_event_str(shown, named)}  {o['wire_bytes']}")
        for d in list(difflib.unified_diff(exp_d, act_d, *labels, n=2, lineterm=""))[2:]:
            lines.append("  " + d)
    return lines


def encode(tools, case):
    """annotate the cases input and expected events with raw_bytes fields"""
    s = json.load(open(case.spec))
    enc = os.path.join(case.work, "encoded")
    run_tool([tools["lucidEncode"], case.program, "--spec", case.spec, "-o", enc], "lucidEncode")
    if "expected" in s:
        case.expected = {int(p): outs for p, outs in
                         json.load(open(os.path.join(enc, "expected.json"))).items()}
        case.out_ports = set(case.expected)


def record_result(case, target, result):
    """store a target's result for a case: (outputs, prints, problems), or a TestError"""
    case.results[target.name] = result
    if not isinstance(result, TestError):
        outputs, prints, problems = result
        with open(os.path.join(case.work, target.name, "prints.txt"), "w") as f:
            f.writelines(p + "\n" for p in prints)


def compare_prints(ref_prints, prints, labels):
    """compares printf outputs, returns list of failure lines"""
    if ref_prints == prints:
        return []
    lines = [f"prints: {labels[0]} {len(ref_prints)} line(s), {labels[1]} {len(prints)}"]
    for d in list(difflib.unified_diff(ref_prints, prints, *labels, n=2, lineterm=""))[2:]:
        lines.append("  " + d)
    return lines


def checks(tested_target_objs, validated_target_objs, case, reference):
    """[(label, ok, failure lines)] for a case, one entry per target"""
    if case.error:
        return [(case.name, False, str(case.error).splitlines())]
    results = []
    if case.expected is not None:
        for target in tested_target_objs:
            results.append(check_expected(case, target))
    for target in validated_target_objs:
        results.append(check_against_reference(case, target, reference))
    if not results:
        results.append((case.name, None, ["no expected block: only --validate can check it"]))
    return results


def check_expected(case, target):
    label = f"{case.name} [{target.name}]"
    result = case.results[target.name]
    if isinstance(result, TestError):
        return label, False, str(result).splitlines()
    outputs, prints, problems = result
    failures = problems + compare(case.expected, outputs)
    return label, not failures, failures


def check_against_reference(case, target, reference):
    label = f"{case.name} [{target.name} = {reference.name}]"
    ref_result = case.results[reference.name]
    result = case.results[target.name]
    errors = []
    for t, r in ((reference, ref_result), (target, result)):
        if isinstance(r, TestError):
            errors += f"{t.name}: {r}".splitlines()
    if errors:
        return label, False, errors
    ref_outputs, ref_prints, ref_problems = ref_result
    outputs, prints, problems = result
    names = (reference.name, target.name)
    failures = (ref_problems + problems
                + compare(ref_outputs, outputs, names)
                + compare_prints(ref_prints, prints, names))
    return label, not failures, failures


# ---- commands --------------------------------------------------------------------

def get_test_cases(cases_dir, programs_dir):
    """[(name, program, spec)] for cases_dir/<prog>[.<variant>].json, with
    programs_dir/<prog>.dpt"""
    out = []
    for spec in sorted(glob.glob(os.path.join(cases_dir, "*.json"))):
        name = os.path.basename(spec)[:-len(".json")]
        program = os.path.join(programs_dir, name.split(".")[0] + ".dpt")
        out.append((name, program, spec))
    return out


def print_report(label, ok, detail, work):
    status = {True: "PASS", False: "FAIL", None: "SKIP"}[ok]
    print(f"{status}  {label}" + (f"  (work dir: {work})" if ok is False else ""))
    for line in detail:
        print("      | " + line)


def parse_targets(flag, spec):
    names = spec.split(",") if spec else []
    for n in names:
        if n not in TARGETS:
            sys.exit(f"ltf: {flag}: unknown target {n} (targets: {', '.join(TARGETS)})")
    if len(set(names)) != len(names):
        sys.exit(f"ltf: {flag}: a target is listed twice")
    return names


def add_check_args(p):
    p.add_argument("--test", help="check expected blocks on these targets, comma-separated "
                   "(default, with no --validate: interp)")
    p.add_argument("--validate", help="compare these targets (not interp) with the interpreter, "
                   "comma-separated")


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--dpt", help="path to the interpreter (dpt)")
    ap.add_argument("--lucidencode", help="path to lucidEncode")
    ap.add_argument("--lucidcc", help="path to lucidcc (dpdk target)")
    ap.add_argument("--no-container", action="store_true",
                    help="dpdk target: run DPDK directly here (Linux with DPDK, as root), "
                         "not in the docker/dpdk container")
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run", help="run one test case")
    r.add_argument("program")
    r.add_argument("spec")
    add_check_args(r)
    r.add_argument("--work", help="work dir (default: a temporary dir, kept on failure)")
    s = sub.add_parser("suite", help="run every case in a directory")
    s.add_argument("programs_dir", help="the programs: <prog>.dpt")
    s.add_argument("cases_dir", help="the test cases: <prog>[.<variant>].json")
    add_check_args(s)
    s.add_argument("-k", dest="only", help="only cases whose name contains this")
    s.add_argument("--work", help="work dir root (default: _work next to CASES_DIR)")
    args = ap.parse_args()

    tools = {"dpt": args.dpt or find_file("dpt"),
             "lucidEncode": args.lucidencode or find_file("lucidEncode")}
    test_targets = parse_targets("--test", args.test or ("interp" if not args.validate else None))
    validate_targets = parse_targets("--validate", args.validate)
    if "interp" in validate_targets:
        sys.exit("ltf: --validate: interp is the reference, so it can't be validated against itself")
    # one object per target; the interpreter is also the reference for --validate
    objs = {n: TARGETS[n](tools, args) for n in dict.fromkeys(["interp"] + test_targets + validate_targets)}
    reference = objs["interp"]
    tested_target_objs = [objs[n] for n in test_targets]
    validated_target_objs = [objs[n] for n in validate_targets]

    if args.cmd == "run":
        root = os.path.abspath(args.work or tempfile.mkdtemp(prefix="ltf-"))
        cases = [Case(os.path.basename(args.spec)[:-len(".json")], args.program, args.spec, root)]
        keep_passing = bool(args.work)
    else:
        root = os.path.abspath(args.work or os.path.join(os.path.dirname(os.path.abspath(args.cases_dir)), "_work"))
        cases = [Case(n, p, s, os.path.join(root, n))
                 for n, p, s in get_test_cases(args.cases_dir, args.programs_dir)
                 if not args.only or args.only in n]
        if not cases:
            sys.exit(f"ltf: no cases in {args.cases_dir}")
        keep_passing = True
    for case in cases:
        if args.cmd == "suite" and os.path.exists(case.work):
            shutil.rmtree(case.work)
        os.makedirs(case.work, exist_ok=True)
        try:
            encode(tools, case)
        except TestError as e:
            case.error = e
    cases_to_test = [c for c in cases if c.error is None and c.expected is not None]
    cases_to_validate = [c for c in cases if c.error is None] if validated_target_objs else []

    def cases_for(target):
        run = []
        if target in tested_target_objs:
            run += cases_to_test
        if target in validated_target_objs or target is reference:
            run += cases_to_validate
        return list(dict.fromkeys(run))

    reference_first = [reference] + [t for t in objs.values() if t is not reference]
    for t in reference_first:
        ready = []
        for c in cases_for(t):
            if t in validated_target_objs:
                # also bind the ports the reference sent to
                ref_result = c.results.get(reference.name)
                ref_ports = set(ref_result[0]) if isinstance(ref_result, tuple) else set()
                c.out_ports = set(c.expected or {}) | ref_ports
            os.makedirs(os.path.join(c.work, t.name), exist_ok=True)
            try:
                t.prepare(c)
                ready.append(c)
            except TestError as e:
                record_result(c, t, e)
        for name, result in (t.run(ready) if ready else {}).items():
            record_result(next(c for c in ready if c.name == name), t, result)
    passed = failed = skipped = 0
    for case in cases:
        results = checks(tested_target_objs, validated_target_objs, case, reference)
        for label, ok, detail in results:
            print_report(label, ok, detail, case.work)
            passed, failed, skipped = passed + (ok is True), failed + (ok is False), skipped + (ok is None)
        if all(ok is not False for _, ok, _ in results) and not keep_passing:
            shutil.rmtree(case.work, ignore_errors=True)
    print("-----")
    print(f"{passed} passed, {failed} failed" + (f", {skipped} skipped" if skipped else ""))
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Benchmark a Lucid program compiled with lucidcc --dpdk

All input ports replay their pcap workloads in an endless loop,
at max possible rate; outputs are dropped in DPDK by the PMD.

Usage:
bench.py PROGRAM.dpt WORKLOAD [--preload-port P ...] [--duration S] [--warmup S] [--label L] [--out results.jsonl]

- WORKLOAD is an interpreter spec file
    - timestamps are ignored
    - each port's events are converted into a pcap
    - the events on --preload-port are injected once (for setup)
    - the events on all other ports are looped
- after the experiment, the harness checks that all preload packets were injected
- each run prints a summary and appends one JSON row to --out
"""
import argparse
import datetime
import json
import os
import platform
import shutil
import subprocess
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(SCRIPT_DIR, "..", "ltf"))
import ltf

EXECUTOR = os.path.join(SCRIPT_DIR, "run_dpdk.py")
MAX_REPLAY_FRAMES = 4096


def prepare(args, tools, work_dir):
    """the work dir: build/, encoded/, job.json"""
    enc = os.path.join(work_dir, "encoded")
    os.makedirs(enc, exist_ok=True)
    try:
        spec = json.load(open(args.workload))
    except ValueError as e:
        raise ltf.TestError(f"the workload isn't an interpreter spec (JSON): {e}")
    ltf.run_tool([tools["lucidEncode"], args.program, "--spec", args.workload, "-o", enc], "lucidEncode")
    declared = spec.get("topology", {}).get("nodes", {}).get("0", {}).get("ports", {})
    # every port the spec declares (but recirculation) is bound; an output-only port is
    # declared as an unconnected link, e.g. "3": {"type": "link"}
    output_ports = {int(p) for p, d in declared.items() if d.get("type") != "recirc"}
    inputs = {int(f[3:-5]): os.path.join("encoded", f)
              for f in os.listdir(enc) if f.startswith("in_") and f.endswith(".pcap")}
    preload_ports = set(args.preload_port or [])
    for port in preload_ports - set(inputs):
        raise ltf.TestError(f"--preload-port {port}: the spec has no events on port {port}")
    replay = {p: f for p, f in inputs.items() if p not in preload_ports}
    preload = {p: f for p, f in inputs.items() if p in preload_ports}
    if not replay:
        raise ltf.TestError("no replayed traffic: every input port is a preload port")
    for port, pcap in replay.items():
        n = len(ltf.read_pcap(os.path.join(work_dir, pcap)))
        if n > MAX_REPLAY_FRAMES:
            raise ltf.TestError(f"port {port}'s workload has {n} frames; replay holds at most "
                                f"{MAX_REPLAY_FRAMES} (they stay in mbufs)")
    ltf.run_tool([tools["lucidcc"], args.program, "--dpdk", "--build", os.path.join(work_dir, "build")],
                 "lucidcc --dpdk")
    job = {"replay": {str(p): f for p, f in replay.items()},
           "preload": {str(p): f for p, f in preload.items()},
           "preload_frames": {str(p): len(ltf.read_pcap(os.path.join(work_dir, f))) for p, f in preload.items()},
           "output_ports": sorted(output_ports - set(replay) - set(preload)),
           "warmup_s": args.warmup, "duration_s": args.duration}
    with open(os.path.join(work_dir, "job.json"), "w") as f:
        json.dump(job, f)
    shutil.copy(EXECUTOR, os.path.join(work_dir, "run_dpdk.py"))
    return job


def check_preload(job, result, rx_pps):
    """check if all preload pkts were received; warn if preload may not have finished within warmup_s"""
    for port, n in job["preload_frames"].items():
        got = result["preload_rx"].get(port, 0)
        if got != n:
            raise ltf.TestError(f"port {port}'s preload: {got} of {n} frames received")
    total = sum(job["preload_frames"].values())
    if total and rx_pps and total > 0.1 * job["warmup_s"] * rx_pps:
        return [f"the preload ({total} frames) may not have finished within the "
                f"{job['warmup_s']} s warm-up at {rx_pps:,} pkt/s; use a longer --warmup"]
    return []


def execute(args, work_dir):
    if args.no_container:
        cmd = (["sudo"] if os.geteuid() != 0 else []) + ["python3", "run_dpdk.py"]
    else:
        runner = ltf.container_runner()
        cmd = [runner, work_dir, "python3", "run_dpdk.py"]
    r = subprocess.run(cmd, cwd=work_dir, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    with open(os.path.join(work_dir, "run_dpdk.log"), "w") as f:
        f.write(r.stdout)
    result_f = os.path.join(work_dir, "result.json")
    if r.returncode != 0 or not os.path.exists(result_f):
        raise ltf.TestError("the dpdk executor failed:\n" + "\n".join(r.stdout.strip().splitlines()[-8:]))
    result = json.load(open(result_f))
    if "error" in result:
        raise ltf.TestError(result["error"])
    return result


def summarize(args, job, result):
    m = result["measured_stats"]
    if not m:
        raise ltf.TestError("no stats lines after the warm-up (run longer?)")
    mean = lambda k: sum(s[k] for s in m) / len(m)
    return {
        "time": datetime.datetime.now().isoformat(timespec="seconds"),
        "host": platform.node(),
        "container": not args.no_container,
        "program": os.path.basename(args.program),
        "workload": os.path.basename(args.workload),
        "label": args.label,
        "warmup_s": args.warmup,
        "duration_s": len(m),
        "rx_pps": round(mean("rx")),
        "tx_pps": round(mean("tx")),
        "recirc_pps": round(mean("recirc")),
        "rx_pps_min": min(s["rx"] for s in m),
        "rx_pps_max": max(s["rx"] for s in m),
        "replay_ports": sorted(int(p) for p in job["replay"]),
        "preload_ports": sorted(int(p) for p in job["preload"]),
        "preload_frames": sum(job["preload_frames"].values()),
        "drops": {k: v for k, v in result["drops"].items() if v},
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("program")
    ap.add_argument("workload", help="an interpreter spec (.json)")
    ap.add_argument("--preload-port", type=int, action="append",
                    help="port for non-looped setup events")
    ap.add_argument("--warmup", type=int, default=2, help="seconds before measuring (default 2)")
    ap.add_argument("--duration", type=int, default=5, help="seconds measured (default 5)")
    ap.add_argument("--label", default="", help="a free-form label for this run's row, e.g. a variant name")
    ap.add_argument("--out", help="append the run's JSON row to this file")
    ap.add_argument("--work-dir", help="work dir (default: test/bench_lucidcc/_work/<program>-<workload>)")
    ap.add_argument("--lucidcc", help="path to lucidcc")
    ap.add_argument("--lucidencode", help="path to lucidEncode")
    ap.add_argument("--no-container", action="store_true",
                    help="run the DPDK programs on this machine, not in a docker container")
    args = ap.parse_args()

    tools = {"lucidcc": args.lucidcc or ltf.find_file("lucidcc"),
             "lucidEncode": args.lucidencode or ltf.find_file("lucidEncode")}
    name = f"{os.path.basename(args.program)[:-4]}-{os.path.splitext(os.path.basename(args.workload))[0]}"
    work_dir = os.path.abspath(args.work_dir or os.path.join(SCRIPT_DIR, "_work", name))
    if os.path.exists(work_dir):
        shutil.rmtree(work_dir)
    os.makedirs(work_dir)
    try:
        job = prepare(args, tools, work_dir)
        print(f"bench: {name}: {args.warmup} s warm-up + {args.duration} s ...", flush=True)
        result = execute(args, work_dir)
        row = summarize(args, job, result)
        warnings = check_preload(job, result, row["rx_pps"])
    except ltf.TestError as e:
        sys.exit(f"bench: {name}: {e}\n(work dir: {work_dir})")
    print(f"bench: {name}: rx {row['rx_pps']:,} pkt/s (min {row['rx_pps_min']:,}, max {row['rx_pps_max']:,}), "
          f"tx {row['tx_pps']:,} pkt/s, recirc {row['recirc_pps']:,} pkt/s"
          + (f", drops {row['drops']}" if row["drops"] else ""))
    for w in warnings:
        print(f"bench: warning: {w}")
    if args.out:
        with open(args.out, "a") as f:
            f.write(json.dumps(row) + "\n")


if __name__ == "__main__":
    main()

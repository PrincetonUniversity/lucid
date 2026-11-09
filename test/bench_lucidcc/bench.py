#!/usr/bin/env python3
"""Benchmark a Lucid program compiled with lucidcc --dpdk (docs/design_notes/04.dpdk-benchmarking.md).

Mode 1 (self-fed): each input port replays its workload endlessly from a pcap (the pcap
PMD's infinite_rx), with no traffic generator or NIC; outputs are dropped by the PMD.
This measures the program and the driver's pipeline (parse, handlers, deparse, rings)
on one core. Numbers from the docker/dpdk image (x86 emulation on an arm64 laptop)
only check that the harness works; real numbers need a native x86 Linux machine
(--dpdk-run native, as root).

  bench.py PROGRAM.dpt WORKLOAD [--duration S] [--warmup S] [--label L] [--out results.jsonl]

WORKLOAD is an interpreter spec (its "events", encoded per port by lucidEncode; their
timestamps are ignored, since the pcap PMD replays as fast as it can), or a pcap
(replayed on --port, default 1). A spec's optional "preload" list holds events sent once,
at the start, e.g. table installs: each preload port gets a second device that reads
its preload once, so the preload is interleaved with the first of the traffic, and
must finish within the warm-up (the harness checks that it was all received, and warns
if it can't have finished in time). Preloading suits traffic that only reads the state
the preload sets up, e.g. table lookups. Every port the spec declares (except recirculation
ports) and every --sink port is bound, so outputs to it are dropped by the PMD rather
than counted as driver no-port drops.

Each run prints a summary and appends one JSON row to --out.
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
import ltf  # noqa: E402  (tool lookup, tool runs, pcap reading)

EXECUTOR = os.path.join(SCRIPT_DIR, "bench_dpdk.py")
MAX_REPLAY_FRAMES = 4096   # infinite_rx holds a port's whole pcap in mbufs (8191 per port)


def prepare(args, tools, work):
    """the work dir: build/, encoded/, job.json"""
    enc = os.path.join(work, "encoded")
    os.makedirs(enc, exist_ok=True)
    sinks = set(args.sink or [])
    if args.workload.endswith(".json"):
        ltf.run_tool([tools["lucidEncode"], args.program, "--spec", args.workload, "-o", enc], "lucidEncode")
        spec = json.load(open(args.workload))
        declared = spec.get("topology", {}).get("nodes", {}).get("0", {}).get("ports", {})
        sinks |= {int(p) for p, d in declared.items() if d.get("type") != "recirc"}
        replay = {int(f[3:-5]): os.path.join("encoded", f)
                  for f in os.listdir(enc) if f.startswith("in_") and f.endswith(".pcap")}
        preload = encode_preload(tools, args, spec, work)
    else:
        shutil.copy(args.workload, os.path.join(enc, f"in_{args.port}.pcap"))
        replay = {args.port: os.path.join("encoded", f"in_{args.port}.pcap")}
        preload = {}
    for port, pcap in list(replay.items()) + list(preload.items()):
        n = len(ltf.read_pcap(os.path.join(work, pcap)))
        if n > MAX_REPLAY_FRAMES:
            raise ltf.TestError(f"port {port}'s workload has {n} frames; replay holds at most "
                                f"{MAX_REPLAY_FRAMES} (they stay in mbufs)")
        if any(len(f) < 14 for _, f in ltf.read_pcap(os.path.join(work, pcap))):
            raise ltf.TestError(f"port {port}'s workload has a frame shorter than 14 bytes")
    ltf.run_tool([tools["lucidcc"], args.program, "--dpdk", "--build", os.path.join(work, "build")],
                 "lucidcc --dpdk")
    job = {"replay": {str(p): f for p, f in replay.items()},
           "preload": {str(p): f for p, f in preload.items()},
           "preload_frames": {str(p): len(ltf.read_pcap(os.path.join(work, f))) for p, f in preload.items()},
           "sinks": sorted(sinks - set(replay) - set(preload)),
           "warmup_s": args.warmup, "duration_s": args.duration}
    with open(os.path.join(work, "job.json"), "w") as f:
        json.dump(job, f)
    shutil.copy(EXECUTOR, os.path.join(work, "bench_dpdk.py"))
    return job


def encode_preload(tools, args, spec, work):
    """the spec's "preload" events, encoded per port (through a copy of the spec with
    them as its events): {port: pcap path relative to work}"""
    if not spec.get("preload"):
        return {}
    pre_spec = {k: v for k, v in spec.items() if k not in ("events", "preload", "expected")}
    pre_spec["events"] = spec["preload"]
    pre_spec_f = os.path.join(work, "preload_spec.json")
    with open(pre_spec_f, "w") as f:
        json.dump(pre_spec, f)
    pre = os.path.join(work, "preload")
    ltf.run_tool([tools["lucidEncode"], args.program, "--spec", pre_spec_f, "-o", pre], "lucidEncode (preload)")
    return {int(f[3:-5]): os.path.join("preload", f)
            for f in os.listdir(pre) if f.startswith("in_") and f.endswith(".pcap")}


def check_preload(job, result, rx_pps):
    """errors if a preload wasn't all received; a warning if it can't have finished
    within the warm-up"""
    for port, n in job["preload_frames"].items():
        got = result["preload_rx"].get(port, 0)
        if got != n:
            raise ltf.TestError(f"port {port}'s preload: {got} of {n} frames received")
    total = sum(job["preload_frames"].values())
    if total and rx_pps and total > 0.1 * job["warmup_s"] * rx_pps:
        return [f"the preload ({total} frames) may not have finished within the "
                f"{job['warmup_s']} s warm-up at {rx_pps:,} pkt/s; use a longer --warmup"]
    return []


def execute(args, work):
    if args.dpdk_run == "native":
        cmd = (["sudo"] if os.geteuid() != 0 else []) + ["python3", "bench_dpdk.py"]
    else:
        runner = ltf.find_tool("run.sh", args.dpdk_run, in_repo="docker/dpdk/run.sh")
        cmd = [runner, work, "python3", "bench_dpdk.py"]
    r = subprocess.run(cmd, cwd=work, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    with open(os.path.join(work, "bench_dpdk.log"), "w") as f:
        f.write(r.stdout)
    result_f = os.path.join(work, "result.json")
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
        "dpdk_run": args.dpdk_run or "docker",
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
        "preload_frames": sum(job["preload_frames"].values()),
        "drops": {k: v for k, v in result["drops"].items() if v},
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("program")
    ap.add_argument("workload", help="an interpreter spec (.json) or a pcap")
    ap.add_argument("--port", type=int, default=1, help="the port a pcap workload replays on (default 1)")
    ap.add_argument("--sink", type=int, action="append", help="another output port to bind (repeatable)")
    ap.add_argument("--warmup", type=int, default=2, help="seconds before measuring (default 2)")
    ap.add_argument("--duration", type=int, default=5, help="seconds measured (default 5)")
    ap.add_argument("--label", default="", help="a free-form label for this run's row, e.g. a variant name")
    ap.add_argument("--out", help="append the run's JSON row to this file")
    ap.add_argument("--work", help="work dir (default: test/bench_lucidcc/_work/<program>-<workload>)")
    ap.add_argument("--lucidcc", help="path to lucidcc")
    ap.add_argument("--lucidencode", help="path to lucidEncode")
    ap.add_argument("--dpdk-run", help="the container runner (default: docker/dpdk/run.sh in this repo), "
                    "or 'native' to run here (Linux, root)")
    args = ap.parse_args()

    tools = {"lucidcc": ltf.find_tool("lucidcc", args.lucidcc),
             "lucidEncode": ltf.find_tool("lucidEncode", args.lucidencode)}
    name = f"{os.path.basename(args.program)[:-4]}-{os.path.splitext(os.path.basename(args.workload))[0]}"
    work = os.path.abspath(args.work or os.path.join(SCRIPT_DIR, "_work", name))
    if os.path.exists(work):
        shutil.rmtree(work)
    os.makedirs(work)
    try:
        job = prepare(args, tools, work)
        print(f"bench: {name}: {args.warmup} s warm-up + {args.duration} s ...", flush=True)
        result = execute(args, work)
        row = summarize(args, job, result)
        warnings = check_preload(job, result, row["rx_pps"])
    except ltf.TestError as e:
        sys.exit(f"bench: {name}: {e}\n(work dir: {work})")
    print(f"bench: {name}: rx {row['rx_pps']:,} pkt/s (min {row['rx_pps_min']:,}, max {row['rx_pps_max']:,}), "
          f"tx {row['tx_pps']:,} pkt/s, recirc {row['recirc_pps']:,} pkt/s"
          + (f", drops {row['drops']}" if row["drops"] else ""))
    for w in warnings:
        print(f"bench: warning: {w}")
    if args.dpdk_run != "native":
        print("bench: (under emulation in docker: the numbers only show that the harness works)")
    if args.out:
        with open(args.out, "a") as f:
            f.write(json.dumps(row) + "\n")


if __name__ == "__main__":
    main()

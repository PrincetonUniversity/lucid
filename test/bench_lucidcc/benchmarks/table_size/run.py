#!/usr/bin/env python3
"""Benchmark script for lucid's initial table implementation, 
   measures throughput as number of missed entries increase from 0 - 4096.
   
   Uses the program: examples/lucidcc/ethswaprefl_table.dpt
    An ethernet reflector with 1 lookup per packet in a 4096-slot table.

   Usage:
   run.py [--sizes 0,1,2,4,...] [--warmup S] [--duration S] [--out results.jsonl] [--no-container]
"""
import argparse
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
BENCH = os.path.join(HERE, "..", "..", "bench.py")
PROGRAM = os.path.normpath(os.path.join(HERE, "..", "..", "..", "..", "examples", "lucidcc", "ethswaprefl_table.dpt"))
DEFAULT_SIZES = [0] + [2 ** i for i in range(13)]   # 0 ... 4096
N_PACKETS = 64
MISS_KEY_BASE = 1 << 20
PRELOAD_PORT = 2   # the installs' port, sent once (bench.py --preload-port); traffic is on port 1

TOPO = {"nodes": {"0": {"ports": {"196": {"type": "recirc"}}}}, "links": []}
PAYLOAD = ("PAYLOAD-DATA" * 4).encode().hex()


def frame(key):
    """ethpkt with "key" as the last 32 bits of its header"""
    return {"name": "ethpkt", "args": [f"0x{(0x0002 << 32) | key:012x}", "0x112233445566", "0x0800", PAYLOAD],
            "locations": ["0:1"], "timestamp": 0}


def workload(n):
    return {
        "comment": f"table_size benchmark: "
                   f"N entries = {n}, "
                   f"N_PACKETS = {N_PACKETS}",
        "topology": TOPO,
        # the installs first (1 us apart), then the traffic: the replay ignores
        # timestamps, but this way the spec also runs in order on the interpreter
        "events": [{"name": "do_install", "args": [k, "0x0801"], "locations": [f"0:{PRELOAD_PORT}"],
                    "timestamp": (k - 1) * 1000} for k in range(1, n + 1)]
                  + [dict(frame(MISS_KEY_BASE + i), timestamp=n * 1000 + 1000000 + i * 1000)
                     for i in range(N_PACKETS)],
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--sizes", help="comma-separated table sizes (default: 0,1,2,4,...,4096)")
    ap.add_argument("--warmup", type=int, default=3, help="seconds before measuring (default 3)")
    ap.add_argument("--duration", type=int, default=5, help="seconds measured (default 5)")
    ap.add_argument("--out", help="output jsonl (default: _work/results.jsonl)")
    ap.add_argument("--no-container", action="store_true",
                    help="passed to bench.py: run DPDK directly here, not in the docker/dpdk container")
    args = ap.parse_args()

    sizes = [int(s) for s in args.sizes.split(",")] if args.sizes else DEFAULT_SIZES
    if max(sizes) > 4096:
        sys.exit("run.py: the table holds at most 4096 entries")
    work = os.path.join(HERE, "_work")
    os.makedirs(os.path.join(work, "workloads"), exist_ok=True)
    out = os.path.abspath(args.out or os.path.join(work, "results.jsonl"))
    run_out = os.path.join(work, "this_run.jsonl")   # this run's rows, for the summary
    if os.path.exists(run_out):
        os.remove(run_out)

    for n in sizes:
        spec = os.path.join(work, "workloads", f"table_{n}.json")
        with open(spec, "w") as f:
            json.dump(workload(n), f)
        cmd = [sys.executable, BENCH, PROGRAM, spec, "--warmup", str(args.warmup),
               "--duration", str(args.duration), "--label", f"entries={n}",
               "--out", run_out, "--work-dir", os.path.join(work, "runs", f"table_{n}")]
        if n > 0:
            cmd += ["--preload-port", str(PRELOAD_PORT)]
        if args.no_container:
            cmd += ["--no-container"]
        r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        for line in r.stdout.splitlines():
            if "rx " in line or "warning" in line or r.returncode != 0:
                print(line, flush=True)
        if r.returncode != 0:
            sys.exit(f"run.py: the run for {n} entries failed")

    rows = [json.loads(l) for l in open(run_out)]
    with open(out, "a") as f:
        for row in rows:
            f.write(json.dumps(dict(row, campaign="table_size")) + "\n")
    base_ns = None
    print(f"\n{'entries':>8}  {'rx pkt/s':>12}  {'ns/pkt':>8}  {'+ns vs 0':>9}")
    for row in rows:
        n = int(row["label"].split("=")[1])
        ns = 1e9 / row["rx_pps"] if row["rx_pps"] else float("inf")
        if n == 0:
            base_ns = ns
        extra = f"{ns - base_ns:9.1f}" if base_ns is not None else f"{'':>9}"
        print(f"{n:>8}  {row['rx_pps']:>12,}  {ns:8.1f}  {extra}")
    print(f"\n(rows appended to {out})")
    if not args.no_container:
        print("(under emulation in docker: the numbers only show that the campaign works)")


if __name__ == "__main__":
    main()

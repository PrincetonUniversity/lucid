#!/usr/bin/env python3
"""Benchmark script for lucid's tables, 
   measures throughput as the number of entries in a table increases from 0 - 4096,
   for exact and ternary tables use the same way.

   Uses the programs: 
    examples/lucidcc/ethswaprefl_table.dpt          (exact)
    examples/lucidcc/ethswaprefl_ternary_table.dpt  (ternary)
    Each is an ethernet reflector with 1 lookup per packet in a 4096-entry table.

   The traffic either misses every entry (--traffic miss, the default) or 
   hits entries spread evenly over the table (--traffic hit).

   Usage:
   run.py [--tables exact,ternary] [--traffic miss|hit] [--sizes 0,1,2,4,...] 
          [--warmup S] [--duration S] [--out results.jsonl] [--no-container]
"""
import argparse
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
BENCH = os.path.join(HERE, "..", "..", "bench.py")
EXAMPLES = os.path.normpath(os.path.join(HERE, "..", "..", "..", "..", "examples", "lucidcc"))
PROGRAMS = {"exact": os.path.join(EXAMPLES, "ethswaprefl_table.dpt"),
            "ternary": os.path.join(EXAMPLES, "ethswaprefl_ternary_table.dpt")}
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


def packet_keys(n, traffic):
    """the keys of the replayed packets: none is in the table (miss), or all are (hit).
       The entries' keys are 1 ... n; an empty table can only miss."""
    if traffic == "miss" or n == 0:
        return [MISS_KEY_BASE + i for i in range(N_PACKETS)]
    return [1 + (i * n) // N_PACKETS for i in range(N_PACKETS)]


def workload(n, traffic):
    return {
        "comment": f"table_size benchmark: "
                   f"N entries = {n}, "
                   f"N_PACKETS = {N_PACKETS}, "
                   f"traffic = {traffic}",
        "topology": TOPO,
        # the installs first (1 us apart), then the traffic: the replay ignores
        # timestamps, but this way the spec also runs in order on the interpreter
        "events": [{"name": "do_install", "args": [k, "0x0801"], "locations": [f"0:{PRELOAD_PORT}"],
                    "timestamp": (k - 1) * 1000} for k in range(1, n + 1)]
                  + [dict(frame(key), timestamp=n * 1000 + 1000000 + i * 1000)
                     for i, key in enumerate(packet_keys(n, traffic))],
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--tables", default="exact,ternary", help="comma-separated table kinds (default: exact,ternary)")
    ap.add_argument("--traffic", default="miss", choices=["miss", "hit"],
                    help="the packets miss every entry (default), or hit entries spread over the table")
    ap.add_argument("--sizes", help="comma-separated table sizes (default: 0,1,2,4,...,4096)")
    ap.add_argument("--warmup", type=int, default=3, help="seconds before measuring (default 3)")
    ap.add_argument("--duration", type=int, default=5, help="seconds measured (default 5)")
    ap.add_argument("--out", help="output jsonl (default: _work/results.jsonl)")
    ap.add_argument("--no-container", action="store_true",
                    help="passed to bench.py: run DPDK directly here, not in the docker/dpdk container")
    args = ap.parse_args()

    tables = args.tables.split(",")
    for table in tables:
        if table not in PROGRAMS:
            sys.exit(f"run.py: unknown table kind '{table}' (expected exact or ternary)")
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
        spec = os.path.join(work, "workloads", f"table_{n}_{args.traffic}.json")
        with open(spec, "w") as f:
            json.dump(workload(n, args.traffic), f)
        for table in tables:
            cmd = [sys.executable, BENCH, PROGRAMS[table], spec, "--warmup", str(args.warmup),
                   "--duration", str(args.duration), "--label", f"{table} {args.traffic} entries={n}",
                   "--out", run_out, "--work-dir", os.path.join(work, "runs", f"{table}_{n}_{args.traffic}")]
            if n > 0:
                cmd += ["--preload-port", str(PRELOAD_PORT)]
            if args.no_container:
                cmd += ["--no-container"]
            r = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            for line in r.stdout.splitlines():
                if "rx " in line or "warning" in line or r.returncode != 0:
                    print(line, flush=True)
            if r.returncode != 0:
                sys.exit(f"run.py: the run for the {table} table with {n} entries failed")

    rows = [json.loads(l) for l in open(run_out)]
    with open(out, "a") as f:
        for row in rows:
            f.write(json.dumps(dict(row, campaign="table_size")) + "\n")
    # a table kind's rows, by number of entries: (pkt/s, ns/pkt)
    results = {table: {} for table in tables}
    for row in rows:
        table, _, entries = row["label"].split()
        ns = 1e9 / row["rx_pps"] if row["rx_pps"] else float("inf")
        results[table][int(entries.split("=")[1])] = (row["rx_pps"], ns)
    print(f"\ntraffic: {args.traffic}")
    print(f"{'entries':>8}" + "".join(f"  {table + ' pkt/s':>15}  {'ns/pkt':>8}  {'+ns vs 0':>9}" for table in tables))
    for n in sizes:
        line = f"{n:>8}"
        for table in tables:
            pps, ns = results[table][n]
            base_ns = results[table][0][1] if 0 in results[table] else None
            extra = f"{ns - base_ns:9.1f}" if base_ns is not None else f"{'':>9}"
            line += f"  {pps:>15,}  {ns:8.1f}  {extra}"
        print(line)
    print(f"\n(rows appended to {out})")
    if not args.no_container:
        print("(under emulation in docker: the numbers only show that the campaign works)")


if __name__ == "__main__":
    main()

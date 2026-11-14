# lucidcc benchmarks

Benchmark the throughput of Lucid programs compiled with `lucidcc --dpdk`. 

The benchmark runs the events from a single-node interpreter spec into 
a dpdk program through pcap devices. Events are split into per-port pcaps.

The pcaps are either played once (for preload ports that contain setup events)
or looped forever (for all the other ports). 

Runs in the docker/dpdk container by default, or optionally can run directly on the host. 

### basic usage
*(from repo root)*
```bash
./test/bench_lucidcc/bench.py examples/lucidcc/ethswaprefl.dpt test/bench_lucidcc/workloads/ethswaprefl.json
```
benchmark the ethswaprefl program on the given workload. 


### full usage
```bash
./test/bench_lucidcc/bench.py PROGRAM.dpt WORKLOAD [--warmup S] [--duration S] [--label L] [--out results.jsonl] [--no-container]
```

- WORKLOAD is an interpreter spec file
    - timestamps are ignored
    - each port's events are converted into a pcap
    - the events on --preload-port are injected once (for setup)
    - the events on all other ports are looped
- after the experiment, the harness checks that all preload packets were injected
- each run prints a summary and appends one JSON row to --out
- --no-container makes the benchmarks run on the host instead of in a container

### files
- `bench.py`: the harness. Encodes events to bytes with `lucidEncode`; builds the program
  with `lucidcc --dpdk`; runs it; averages the driver's per-second `--stats` rates after a warm-up.
- `run_dpdk.py`: wrapper for compiled Lucid program, copied by `bench.py` to work dir
- `workloads/`: some specs to use as workloads for ethswaprefl and table_install programs
- `_work/<program>-<workload>/`: untracked staging dir (build, encoded pcaps, switch logs).
- `benchmarks/<campaign>/`: benchmark campaigns, each a script (and any programs or
  workloads of its own) that runs `bench.py` over a set of variants, e.g.
  `benchmarks/table_size/`, lookup cost against the number of table entries.

### Notes 
- Each port can only contain 4096 events (can hold more with larger mbuf pools).
- Every port the spec's topology declares (but recirculation) is bound, to exercise as much 
  of the tx path as possible.
- Declare an output-only port as an unconnected link, e.g. `"3": {"type": "link"}`.

```sh
./test/bench_lucidcc/bench.py examples/lucidcc/ethswaprefl.dpt test/bench_lucidcc/workloads/ethswaprefl.json
./test/bench_lucidcc/bench.py ... --no-container       # on a Linux machine with DPDK, as root
```

Each run prints a summary and, with `--out`, appends one JSON row: program, workload,
label, rx/tx/recirculation pkt/s (mean, min and max rx), and any drops.

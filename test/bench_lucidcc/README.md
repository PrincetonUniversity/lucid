# lucidcc benchmarks

Throughput of Lucid programs compiled with `lucidcc --dpdk`. The design is in
`docs/design_notes/04.dpdk-benchmarking.md`.

Mode 1 (self-fed), the only mode so far: each input port replays its workload endlessly
from a pcap (the pcap PMD's `infinite_rx`), with no traffic generator or NIC, and
outputs are dropped by the PMD. It measures the program and the driver's pipeline on one
core. In the `docker/dpdk` image (x86 emulation on an arm64 laptop) the numbers only show
that the harness works; real numbers need a native x86 Linux machine.

- `bench.py`: the harness. It encodes the workload with `lucidEncode`, builds the program
  with `lucidcc --dpdk`, runs it, and averages the driver's per-second `--stats` rates
  after a warm-up.
- `bench_dpdk.py`: internal to `bench.py`, the executor it runs in the image (not meant
  to be run directly).
- `workloads/`: example workloads. A workload is an interpreter spec whose `events` are
  the traffic (timestamps are ignored), or a pcap. A port's workload holds at most 4096
  frames, since the replay keeps them in mbufs. Frames must be at least 14 bytes.
  A spec's optional `preload` list holds events sent once at the start, e.g. table
  installs (`workloads/tables_install_*.json`). Each preload port gets a second device
  that reads its preload once, interleaved with the first of the traffic. The harness
  checks that the whole preload was received, and warns if it can't have finished
  within the warm-up. Preloading suits traffic that only reads the state the preload
  sets up.
- `_work/<program>-<workload>/`: untracked staging dir (build, encoded pcaps, switch logs).

```sh
./test/bench_lucidcc/bench.py examples/lucidcc/ethswaprefl.dpt test/bench_lucidcc/workloads/ethswaprefl.json
./test/bench_lucidcc/bench.py PROGRAM.dpt WORKLOAD [--warmup S] [--duration S] [--label L] [--out results.jsonl]
./test/bench_lucidcc/bench.py ... --dpdk-run native     # on a Linux machine with DPDK, as root
```

Each run prints a summary and, with `--out`, appends one JSON row: program, workload,
label, rx/tx/recirculation pkt/s (mean, min and max rx), and any drops.

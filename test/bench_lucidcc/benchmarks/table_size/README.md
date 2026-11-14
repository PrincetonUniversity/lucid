# table_size: lookup cost as a table grows

Benchmarks table lookup with respect to number of non-matching entries.

Uses `examples/lucidcc/ethswaprefl_table.dpt` (eth reflector with 1 lookup per pkt)

`run.py` generates a workload for each N (number of entries) from 0 to 4096 (powers of 2).

Each workload: 
    1) preloads N entries (installs on port 2, run with `--preload-port 2`);
    2) replays 64 packets on port 1 that miss all N entries

The script runs `../../bench.py` once per size (label `entries=N`), 
appends the rows to `_work/results.jsonl` (or `--out`), 
and prints entries, pkt/s, ns/pkt, and ns/pkt over N = 0.

```bash
./test/bench_lucidcc/benchmarks/table_size/run.py                          # in docker
./test/bench_lucidcc/benchmarks/table_size/run.py --no-container           # on bare metal
./test/bench_lucidcc/benchmarks/table_size/run.py --sizes 0,16,256,4096 --warmup 2 --duration 3 # other options
```
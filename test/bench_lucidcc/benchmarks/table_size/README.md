# table_size: lookup cost as a table grows

Benchmarks table lookup with respect to the number of entries in the table, for 
an exact table (`Table`) and for a ternary table (`TernaryTable`) used as an exact 
table: every entry has a full mask and the same priority.

Uses `examples/lucidcc/ethswaprefl_table.dpt` (exact) and 
`examples/lucidcc/ethswaprefl_ternary_table.dpt` (ternary). 
Each is an eth reflector with 1 lookup per pkt.

`run.py` generates a workload for each N (number of entries) from 0 to 4096 (powers of 2).

Each workload: 
    1) preloads N entries (installs on port 2, run with `--preload-port 2`);
    2) replays 64 packets on port 1 that miss all N entries (`--traffic miss`, the default), 
       or that hit entries spread evenly over the table (`--traffic hit`)

The script runs `../../bench.py` once per table kind and size (label `<kind> <traffic> entries=N`), 
appends the rows to `_work/results.jsonl` (or `--out`), 
and prints, for each table kind: entries, pkt/s, ns/pkt, and ns/pkt over N = 0.

```bash
./test/bench_lucidcc/benchmarks/table_size/run.py                          # in docker
./test/bench_lucidcc/benchmarks/table_size/run.py --no-container           # on bare metal
./test/bench_lucidcc/benchmarks/table_size/run.py --traffic hit            # packets that hit
./test/bench_lucidcc/benchmarks/table_size/run.py --tables ternary         # one table kind
./test/bench_lucidcc/benchmarks/table_size/run.py --sizes 0,16,256,4096 --warmup 2 --duration 3 # other options
```

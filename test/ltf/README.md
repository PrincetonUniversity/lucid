# Lucid test framework (ltf)

`ltf.py` Tests a program compiled with `lucidcc` against an 
expected output trace, or against the output of the interpreter.

## Test cases

A test case is an interpreter spec with a single-node topology, 
an `events` block defining input events, and an `expected` block 
defining expected outputs on each port. For example:
```json
{
  "max time": 10000000,
  "topology": {"nodes": {"0": {"ports": {"196": {"type": "recirc"}}}}, "links": []},
  "events": [
    {"name": "do_install", "args": [42, 7], "locations": ["0:1"], "timestamp": 0},
    {"name": "do_query", "args": [42], "locations": ["0:1"], "timestamp": 1000000}
  ],
  "expected": {
    "1": [ {"name": "query_result", "args": [42, 7, 1]} ]
  }
}
```

- **Control commands** (`Table.install`, ...) aren't supported. Use ordinary events instead as in `examples/lucidcc/tables_install.dpt`.

- **What's checked is chosen on the command line:**
  - `--test T[,T...]` checks each case's `expected` block on the targets. Cases without
    one aren't tested. This is the default, as `--test interp`.
  - `--validate T[,T...]` is compiler validation: it compares the targets (not `interp`)
    with the interpreter, on every case, ignoring `expected` blocks. The targets' outputs
    and printf lines must match the interpreter's.
  - Each case runs once per target, whichever checks use it. A case without `expected`
    only gets checked by `--validate`.

## Comparison

Comparison checks wire bytes per port, ordered. Timestamps are not considered.
Compiler validation (`--validate`) also compares printf lines, in order.

## Directories + usage

- `ltf.py` test runner, uses lucidEncode and dpt (and lucidcc, for dpdk)
- `run_dpdk_test.py` internal to `ltf.py`: the dpdk target's executor, which `ltf.py` runs in the
  `docker/dpdk` image on the work dirs it prepares (not meant to be run directly)
- `cases/<prog>[.<variant>].json` runs against `<prog>.dpt` in the programs dir (the `make`
  targets use `examples/lucidcc`, shared with `test/lucidcc`).
- `_work/<case>/` untracked staging dir:
  - `encoded/`: test cases run through `lucidEncode`;
  - `<target>/`: `out_<port>.pcap`, `prints.txt`, and the raw tool output.

```sh
./test/ltf/ltf.py run PROGRAM.dpt CASE.json [--test T,...] [--validate T,...] [--work DIR]   # one case
make test-ltf                                                    # --test interp
make test-ltf-dpdk                                               # --test interp,dpdk --validate dpdk
./test/ltf/ltf.py suite examples/lucidcc test/ltf/cases [-k NAME] [--test T,...] [--validate T,...]   # a subset
```

The runner finds `dpt` and `lucidEncode` at the repo root (where `make` puts them), or else on `PATH`. 
Use `--dpt` and `--lucidencode` to point it elsewhere.

Targets:
- `interp`: the interpreter's simulator.
- `dpdk`: compiled with `lucidcc --dpdk`, then run in the `docker/dpdk` image with `net_af_packet` + veths for every port. Inputs are replayed at timestamps, the switch stops 1s after last input. Driver drops fail the test (parse drops don't: a parser's `drop` is the program's). Every input and output frame must be at least 14 bytes (an ethernet header) to cross a veth.
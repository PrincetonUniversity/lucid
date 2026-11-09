# Lucid test framework (ltf)

Execution tests. These run a Lucid program on a target with a timed input trace, and compare what leaves each port with the expected output. The design is in `docs/design_notes/03.lucid-test-framework.md`.

## Test cases

A test case is an interpreter spec with one node, plus an `expected` block.
The interpreter ignores the `expected` block.

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

- **Inputs** events in use the spec's usual forms: `name`/`args`, or `{"type": "packet", "bytes": hex}`.
  Every input needs a single `0:<port>` location and a nanosecond timestamp. 
  Int args can be hex strings (`"0x0800"`).
- **`expected`** has an output event list for each port. Outputs are in the same format as inputs, without location and timestamp metadata.
- **Control commands** (`Table.install`, ...) aren't supported. Use ordinary events instead as in `examples/lucidcc/tables_install.dpt`.
- **Compiler validation tests** are cases with no `expected`: with `--target interp,dpdk`,
  the interpreter's outputs and printf lines are what dpdk must match. With one target,
  they're skipped.

## Comparison

Comparison checks wire bytes per port, ordered. Timestamps are not considered.
Compiler validation tests also compare printf lines, in order.

## Directories + usage

- `ltf.py` test runner, uses lucidEncode and dpt (and lucidcc, for dpdk)
- `ltf_dpdk.py` internal to `ltf.py`: the dpdk target's executor, which `ltf.py` runs in the
  `docker/dpdk` image on the work dirs it prepares (not meant to be run directly)
- `cases/<prog>[.<variant>].json` runs against `<prog>.dpt` in the programs dir: `--programs`
  (the `make` targets use `examples/lucidcc`, shared with `test/lucidcc`), or else `programs/`.
- `_work/<case>/` untracked staging dir:
  - `encoded/`: test cases run through `lucidEncode`;
  - `<target>/`: `out_<port>.pcap`, `prints.txt`, and the raw tool output.

```sh
./test/ltf/ltf.py run PROGRAM.dpt CASE.json [--work DIR]         # one case
make test-ltf                                                    # every case, on the interpreter
make test-ltf-dpdk                                               # every case, on the interpreter and dpdk
./test/ltf/ltf.py suite test/ltf --programs examples/lucidcc [-k NAME] [--target T[,T...]]   # the same, or a subset
```

The runner finds `dpt` and `lucidEncode` on `PATH`, or at the repo root. 
Use `--dpt` and `--lucidencode` to point it elsewhere.

Targets:
- `interp`: the interpreter's simulator.
- `dpdk`: compiled with `lucidcc --dpdk`, then run in the `docker/dpdk` image with `net_af_packet` + veths for every port. Inputs are replayed at timestamps, the switch stops 1s after last input. Drops fail the test.
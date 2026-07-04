# C backend tests

Quick regression tests for the `lucidcc` C backend, covering the small set of
programs we iterate on (events, NAT, tables, a learner, a bloom filter, eth
reflectors). Two complementary harnesses:

- **`run_tests.sh`** — byte-diffs the *generated C text* against `expected/`.
- **`run_c_tests.sh`** — *compiles* the generated C with gcc, and for programs
  with a pcap fixture, *runs* it and diffs the output packets against `pcaps/`.

## Layout

- `programs/` — the `.dpt` sources under test.
- `expected/` — the checked-in expected C output, one `<name>.cc` per program.
- `pcaps/` — runtime fixtures: `<name>.in.pcap` + `<name>.expected.pcap`, plus
  `gen_inputs.py` which (re)generates the input pcaps.
- `run_tests.sh`, `run_c_tests.sh` — the two harnesses.

## Usage

```sh
# generated-C text diff
./ctests/run_tests.sh            # build, compile each program, diff vs expected
./ctests/run_tests.sh --update   # regenerate expected/ (do this when a change is intended)
./ctests/run_tests.sh --no-build # reuse the existing ./lucidcc, skip `make`

# compile (+ run, where a fixture exists)
./ctests/run_c_tests.sh            # gcc-compile each program; run+diff those with pcaps
./ctests/run_c_tests.sh --update   # (re)capture pcaps/<name>.expected.pcap
./ctests/run_c_tests.sh --no-build # reuse the existing ./lucidcc, skip `make`
```

Normal mode exits non-zero if any program fails to compile or its output drifts
from `expected/`, so it works in CI. A failure prints the first lines of the diff
(or the compiler error) so you can see whether the change is intended.

When you intentionally change codegen, review the diff, then run `--update` to
accept the new output.

## Adding a program

Drop a `.dpt` into `programs/`, run `run_tests.sh --update` to capture its
expected C, and commit both. It is automatically compile-checked by
`run_c_tests.sh` too.

To also give it a **run** test: add an input pcap at `pcaps/<name>.in.pcap`
(extend `pcaps/gen_inputs.py` so it's reproducible), then
`run_c_tests.sh --update` to capture `pcaps/<name>.expected.pcap`. The eth
reflectors (`ethswaprefl`, `ipv4refl`) are the current examples; programs that
emit no packets (e.g. recirc-only) or need bespoke input framing are left
compile-only for now.

## Note on determinism

The generated C embeds fresh-id temp names (e.g. `tmp_4331`). These are stable
for a fixed compiler, so byte-diffing is reliable — but a compiler change that
shifts id allocation will change the output and require an `--update` even if the
behavior is unchanged. The printed diff makes it easy to tell incidental
renumbering from a real change.

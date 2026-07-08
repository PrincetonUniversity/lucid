# C backend tests

Regression tests for the `lucidcc` C backend and its three drivers (pcap, raw-socket,
DPDK), over the small set of programs we iterate on (events, NAT, tables, a learner, a
bloom filter, eth reflectors).

Two tiers:

**Codegen + pcap (local, no root, no deps beyond gcc + libpcap)** — bash, deterministic:

- **`run_tests.sh`** — the codegen gate. For every program: generate C, **compile it**
  (gcc), and byte-diff the generated C text against `expected/`. Reject-marked programs
  (`// LUCIDCC_REJECT: <substr>`) are checked to be rejected.
- **`test_pcap.sh`** — the pcap *run* tier. For every program with a fixture, run the
  compiled pcap binary on `pcaps/<name>.in.pcap` and byte-compare (`cmp`) the output
  against `pcaps/<name>.expected.pcap`. Stays bash because pcap output is deterministic,
  so the check is a plain byte-compare with no scapy dependency.

**Live-traffic drivers (Linux container, root)** — python + scapy, content checks:

- **`test_rawsock.py`** — the raw-socket driver over a veth pair: `reflector` + `scanloop`.
- **`test_dpdk.py`** — the DPDK driver: `reflector` (pcap PMD) + `af_packet` (veth) +
  `events` (multi-out / multi-port rx) + `scanloop`.
- **`driverlib.py`** — helpers shared by the two above: gen/build, Lucid frame building
  (`lucid_frame`, `event_tags`), the reflector + scanloop traffic/checks, and the veth /
  subprocess plumbing. Each `test_<driver>.py` supplies only its transport-specific run.

The three test *shapes* — reflector, scanloop, events — recur across drivers; that shared
logic lives in `driverlib.py`, so each driver test is mostly its transport wiring plus a
list of sub-tests.

## Layout

- `programs/` — the `.dpt` sources under test.
- `expected/` — checked-in expected C, one `<name>.cc` per program (for `run_tests.sh`).
- `pcaps/` — pcap fixtures (`<name>.in.pcap` + `<name>.expected.pcap` for `test_pcap.sh`)
  and `gen_inputs.py` which (re)generates the input pcaps. The driver tests also write
  transient `*.send.pcap` / `*.recv.pcap` / `dpdk_*.pcap` here at runtime.
- `_rawsock_build/`, `_dpdk_build/` — per-driver build dirs, one subdir per sub-test
  (`refl`, `events`, `scanloop`), holding the pre-generated `lucidprog.c` (see the
  two-phase note below).

## Usage

```sh
# codegen gate (compile + golden) -- fast, local
./ctests/run_tests.sh              # build lucidcc, then compile + diff every program
./ctests/run_tests.sh --update     # accept new C as golden (compile-checked first)
./ctests/run_tests.sh --no-build   # reuse the existing ./lucidcc, skip `make`

# pcap run fixtures -- local
./ctests/test_pcap.sh              # run + byte-compare each fixture
./ctests/test_pcap.sh --update     # (re)capture pcaps/<name>.expected.pcap
./ctests/test_pcap.sh --no-build   # reuse ./lucidcc

# live-traffic drivers -- in the Linux container, as root
sudo python3 ctests/test_rawsock.py     # reflector + scanloop over veths
sudo python3 ctests/test_dpdk.py        # reflector + af_packet + events + scanloop
```

Normal mode exits non-zero on any failure, so these work in CI.

## Two-phase driver tests (`--gen`)

The driver tests run in the Linux container, where `lucidcc` is not natively built, so
they are two-phase: `--gen` invokes the compiler (run it once, where `lucidcc` is built)
and writes the generated C / build dir; a plain run just gcc/make's + runs that committed
source. Regenerate when the compiler or a `.dpt` changes:

```sh
sudo python3 ctests/test_rawsock.py --gen   # .dpt -> _rawsock_build/<test>/lucidprog.c
sudo python3 ctests/test_dpdk.py --gen      # .dpt -> _dpdk_build/<test>/
```

## Adding a program

Drop a `.dpt` into `programs/`, run `run_tests.sh --update` to capture its expected C
(it's compile-checked automatically). To also give it a **pcap run** test, add
`pcaps/<name>.in.pcap` (extend `pcaps/gen_inputs.py` so it's reproducible), then
`test_pcap.sh --update` to capture `pcaps/<name>.expected.pcap`.

## Note on determinism

The generated C embeds fresh-id temp names (e.g. `tmp_4331`), stable for a fixed compiler
— so a compiler change that shifts id allocation legitimately changes the output and
requires an `--update`. The pcap driver stamps output packets with a fixed timestamp (0),
so its output is deterministic and byte-comparable; use a live driver to profile timing.

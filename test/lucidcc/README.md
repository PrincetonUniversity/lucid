# C backend tests

Regression tests for the `lucidcc` C backend and its three drivers (pcap, raw-socket,
DPDK), over a few programs (in `./programs`).


- **Compiler tests** `test_compiler.sh`: run the Lucid-C compiler on the programs, 
  then compile the C and compare against expected output.

- **PCAP tests** `test_pcap.sh`: compile and test programs with the PCAP driver; 
  byte compare program output against `pcaps/<name>.expected.pcap`. The expected 
  outputs are computed in python by `gen_pcaps.py`, not captured from lucidcc. 

- **RAWSOCK tests** `test_rawsock.py`: compile and test certain programs 
  with the raw socket driver over a veth (or feth for macos) pair. 

- **DPDK tests** `test_dpdk.py`: compile and test certain programs 
  with the dpdk driver. Requires x86 linux, works on the container in 
  `$REPO_ROOT/docker/dev`.

## Layout

- `programs/` — the `.dpt` sources under test.
- `expected/` — checked-in expected C, one `<name>.cc` per program (for `test_compiler.sh`).
- `gen_pcaps.py` — generates the pcap fixtures (`<name>.in.pcap` + `<name>.expected.pcap`)
  for `test_pcap.sh`, which runs it first.
- `pcaps/` — untracked; generated fixtures plus the live tests' captures.
- `_rawsock_build/`, `_dpdk_build/` — per-driver build dirs.

## Usage

`make test-cc` runs the compiler and pcap tests; `make test-cc-wire` runs the
live-traffic tests. Or run each directly:

```sh
# codegen gate (compile + golden) -- fast, local
./test/lucidcc/test_compiler.sh              # build lucidcc, then compile + diff every program
./test/lucidcc/test_compiler.sh --update     # accept new C as golden (compile-checked first)
./test/lucidcc/test_compiler.sh --no-build   # reuse the existing ./lucidcc, skip `make`

# pcap run fixtures -- local
./test/lucidcc/test_pcap.sh              # generate fixtures, run + byte-compare each
./test/lucidcc/test_pcap.sh --no-build   # reuse ./lucidcc

# live-traffic drivers -- in the Linux container
sudo python3 test/lucidcc/test_rawsock.py     # reflector + scanloop over veths
sudo python3 test/lucidcc/test_dpdk.py        # reflector + af_packet + events + scanloop
```
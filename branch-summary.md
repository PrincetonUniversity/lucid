# 26.6.c-refactor branch notes

This branch improves the Lucid to C compiler from a prototype with many bugs, 
no tests, and poor performance, to a usable (beta stage) backend. Prior to this 
branch, the C backend generated code that was wrong in basic ways, e.g., events 
were sometimes serialized as empty packets and multi-byte headers were parsed 
in the wrong byte order. There were also limitations that prevented most 
programs from working, e.g., heandlers could only generate one output 
and there was no support for background event handling (for multi threading). 
There were also many mismatches between the behavior of compiled programs 
and that of the Lucid interpreter.

After this branch, the compiler supports most of Lucid, programs generally 
behave equivalently in the interpreter and compiler, and there are automated 
tests to check the generated C.

## Fixes

- (**bug fix**) **Tables miss.** Misses in table entries did not run the 
  default action, they matched the first empty entry which triggered action 0.
- (**bug fix**) **Exact tables.** Exact table install incorrectly used its 
  key as its mask. 
- (**bug fix**) **Network-byte ordering.** Header fields now parsed and 
  serialized in network byte order; sub-byte bitfields correctly packed.
- (**bug fix**) **Output packet length.** With previous PCAP driver, 
  caplen of emitted packets was always 0; dpdk driver had other len-related 
  issues. Both fixed.
- (**bug fix**) **Undefined behavior.** Removed numerous sources of undefined 
  behavior in generated C code: pointer-cast type punning; 
  unaligned loads and store; and incorrectly calculated event container sizes.
- (**bug fix**) **Unsupported feature detection.** Features the backend doesn't 
  yet support (multicast, saturating arithmetic, bit concat, certain field layouts, 
  int types wider than 64 bits) now cause compiler errors instead of incorrect C code. 
- (**bug fix**) **Smaller issues.** Fixed C operator precedence and `mod` name 
  collision; bit-aligned hash calc semantics; fixed numerous printf and formatter 
  bugs; set correct port on event recirculate; fixed bugs in hash implementation.
- (**bug fix**) **Support large internal values** Fixed a bug causing compiler    
  crash for values larger than 63 bits.



## Core feature and execution model support

- (**feature**) **Multi event generation.** The C compiler now generates code that 
  supports handlers that generate multiple events, by tracking an output list of 
  events.
- (**feature**) **Asynchronous event generation.** The C compiler now supports 
  a dedicated queue for recirculated events, and an input stage that round robins 
  between an input packet queue and the recirculated event queue. Previously, 
  recirculated events were implemented as tail calls, so a recursive event/handler 
  would block incoming packets.
- (**feature**) **Backpressure / early drops.** Under overload, arriving packets 
  wait in the NIC (which drops and counts them) rather than being dropped inside the
  pipeline, so work already done isn't thrown away; the one place the pipeline
  drops is the recirculation ring, which keeps recirculation from stalling it.
  There are other reasonable policies, so this should become configurable once
  we've looked at them more.
- (**feature**) **Delayed events.** Lucid's interpreter has a mechanism for 
  scheduling asynchronous operations, where an event is generated with a delay 
  that limits how soon it will be processed. We have added support for delayed 
  events in the C compiler backend, with a calendar queue for delayed events.
- (**feature**) **Missing builtins.** Added `Sys.time()` (per-event timestamp set by the 
  driver) and standard IP checksums (currently expressed as `hash<16>(checksum, ...)`).
- (**feature**) **Records for event args and globals.** Records can now be used as event 
  arguments and for custom data structures of globals, as in the interpreter.
- (**feature**) **`Payload.t`** event args (unparsed packet payload) now
  supported. 
- (**feature**) **Custom-width ints** Custom-width ints, `int<n>`, are now 
  stored in the smallest C int type that fits, and operations that can overflow 
  are automatically masked. 
- (**feature**) **Timestamps** The DPDK driver now calculates nanosecond timestamps, 
  at packet arrival, consistent with the interpreter and tofino targets.


## Drivers

- The branch refactors generated code into: 
  1) a runtime library of data structures and builtins; 
  2) per-program event structs, handler functions, and other user code; 
  3) a fixed-function "driver" pipeline for runtime operations such as 
     packet cloning and recirculation. All drivers have the same general 
     shape: parsing -> handling -> replication -> deparsing.
- There are 3 drivers: pcap, raw_socket, and dpdk.
  - **pcap.** The pcap driver is intended for deterministic offline testing. It handles input and events (possibly recirculated) in timestamp order. `--latency-ns` sets how much recirculation latency to add.
  - **dpdk.** The DPDK implementation uses rte_rings, embeds event structures into 
    mbufs, and deparses packets in place. This is the primary C driver, it implements 
    the full asynchronous event generation and early drop semantics. 
  - **raw socket.** The raw socket driver is experimental for cases where DPDK cannot 
    be supported. It runs on any AF_PACKET (Linux) or BPF (macOS) interface. 
    It introduces simple packet buffer with a slab allocator and ring-buffer 
    queues. It has worse performance than the DPDK driver and does not yet implement 
    the early drop / backpressure.


## Testing

- We improved Lucid's testing framework for `lucidcc` development, adding 3 kinds of automated tests:
  1. code generation tests, in `test/lucidcc`, check that C generated by `lucidcc`
     matches expected copies. Expected copies should be updated whenever it is intended. 
  2. expected output tests, with `test/ltf/ltf.py`, which compares the output of a 
     target backend with an expected event trace. The input is a standard interpreter 
     spec with a block for the expected output and a few additional requirements. It 
     can be used for both the interpreter and lucidcc/DPDK backend.
  3. compiler validation tests, also with `test/ltf/ltf.py`, which can be used to 
     compare the output of the interpreter with the output of the lucidcc/DPDK 
     backend. 
- The expected output and compiler validation tests use the small `docker/dpdk` image.


## Compile codebase improvements

- **IR simplification.** Generalized the C backend's intermediate representation, 
  so that it could represent Lucid programs with value semantics and abstract 
  builtins as used in the interpreter IR and also Lucid programs with reference 
  semantics that can be translated directly to C. 
- **Extensibility.** In the first half of the C compiler's pipeline, the 
  code has no pointers and passes data by value. C-specific transformations 
  happen only after a certain point. The aim is that a backend to other languages 
  besides C will eventually be able to re-use the first half of the current C backend. 
- **Type and form checking improvements.** The C compiler's type and form checking 
  passes have many improvements to catch bugs in transformation passes. 
- **A smaller IR.** Design improvements and code cleanup allowed the backend 
  to shrink from about 7,800 to 6,700 lines while gaining the features above.


## Also on this branch (not C backend)

- Frontend fixes: module interfaces check global type declarations against their full definitions; record elimination also removes global user type declarations.


## Known limits / next

- Not yet supported: multicast (`generate_ports`), saturating arithmetic, bit concatenation, and integers wider than 64 bits.
- `hash` (other than `checksum`) is target-specific by design.


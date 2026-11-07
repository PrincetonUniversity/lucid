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
  yet support (multicast, saturating arithmetic, bit concat, certain field layouts)
  now cause compiler errors instead of incorrect C code. 
- (**bug fix**) **Smaller issues.** Fixed C operator precedence and `mod` name 
  collision; bit-aligned hash calc semantics; made printf behavior 
  consistent with interpreter.


## Core feature and execution model support

- (**feature**) **Multi event generation.** The C compiler now generates code that 
  supports handlers that generate multiple events, by tracking an output list of 
  events.
- (**feature**) **Asynchronous event generation.** The C compiler now supports 
  a dedicated queue for recirculated events, and an input stage that round robins 
  between an input packet queue and the recirculated event queue. Previously, 
  recirculated events were implemented as tail calls, so a recursive event/handler 
  would block incoming packets.
- (**feature**) **Missing builtins.** Added `Sys.time()` (per-event timestamp set by the 
  driver) and standard IP checksums (currently expressed as `hash<16>(checksum, ...)`).
- (**feature**) **Records for event args and globals.** Records can now be used as event 
  arguments and for custom data structures of globals, as in the interpreter.
- (**feature**) **`Payload.t`** event args (unparsed packet payload) now
  supported. 
- (**feature**) **Custom-width ints** Custom-width ints, `int<n>`, are now 
  stored in the smallest C int type that fits, and operations that can overflow 
  are automatically masked. 


## Drivers

- Factored generated code into: 
  1) a runtime library of data structures and builtins; 
  2) per-program event structs, handler functions, and other user code; 
  3) a fixed-function "driver" pipeline for runtime operations such as 
     packet cloning and recirculation. All drivers have the same general 
     shape: parsing -> handling -> replication -> deparsing.
- There are 3 drivers: pcap, raw_socket, and dpdk.
  - **pcap.** The pcap driver is intended for deterministic offline testing. 
    It processes all packets in the recirculation queue before pulling the next input 
    packet. 
  - **raw socket.** The raw socket driver runs on any AF_PACKET (Linux) or BPF (macOS)
    interface. It introduces simple packet buffer with a slab allocator and ring-buffer 
    queues, intended as reference implementations. 
  - **dpdk.** The DPDK implementation uses rte_rings, embeds event structures into 
    mbufs, and deparses packets in place. There are significant performance optimization 
    in this driver. 


## Testing

- Previously, there were no tests on the C code that `lucidcc` generated.
- We added a test suite in `test/lucidcc/` with:
  - expected-C tests: the generated C must compile with gcc and match the checked-in
    output, and programs using unsupported features must be rejected;
  - pcap tests: each program is run on input packets and its output is compared with
    an expected result computed independently in Python, not captured from the compiler
    (so it checks behavior, not just stability);
  - live tests for the raw-socket and DPDK drivers;
  - a small docker image for DPDK testing.


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

- Frontend fixes: module interfaces check global type declarations against their full
  definitions; record elimination also removes global user type declarations.


## Known limits / next

- Not yet supported: multicast (`generate_ports`), saturating arithmetic, bit
  concatenation, timers (`Event.delay`), 62- and 63-bit integers, and integers wider
  than 64 bits.
- `Sys.time()` reads the wall clock, so offline pcap runs that use it aren't
  reproducible.
- `hash` (other than `checksum`) is target-specific by design.
- Next: merge the C compiler improvements from the experimental fork.


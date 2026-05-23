# `basic`

IPv4 forwarding via a control-plane-populated longest-prefix-match table.
On a table hit the switch rewrites the ethernet MACs, decrements the IPv4 TTL,
and emits the packet out the matched port. On a miss the default action drops
the packet.

## Files
- [basic.dpt](basic.dpt) — the Lucid program.
- [basic.json](basic.json) — interpreter spec: 4-switch pod-topo + per-switch
  `Table.install` commands (translating the P4 tutorial's `sX-runtime.json`
  files) + three test packets.

## Running
```bash
../../../sources/lucid/dpt basic.dpt --spec basic.json --silent
```

## Topology
A simple pod topology. Node IDs in the spec map to `s1..s4` as
`0..3`. Host-facing ports (s1 ports 1–2, s2 ports 1–2) are deliberately left
undeclared so forwarded packets show up in each node's `Exits` list, which is
what to scan to verify correct delivery.

```
   h1 -- 1 [s1=0] 3 -------- 1 [s3=2] 2 -------- 4 [s2=1] 1 -- h3
   h2 -- 2        4 -------- 2 [s4=3] 1 -------- 3        2 -- h4
```

## Test cases (in `basic.json`)
1. **h1 → h2** (intra-s1). Exits at `0:2` with dmac `08:00:00:00:02:22`,
   smac `08:00:00:00:01:00`, ttl `63`, recomputed csum `0x64e8`. Input csum
   is `0` so the handler logs a "bad input csum" line.
2. **h1 → h3** (3-hop: s1 → s3 → s2). Exits at `1:1` with dmac
   `08:00:00:00:03:33`, smac `08:00:00:00:02:00`, ttl `61` (decremented at
   each hop), csum `0x65e7`. Input has `csum=0` at s1, but each
   intermediate hop produces a *valid* csum, so s3 and s2 do not log a
   verify error.
3. **h1 → 10.99.99.99** (no route). Drops at s1 via the default action; no
   exit packets, "drop" line in the log.
4. **h1 → h2 with a correct input csum** (`0x63e8`). Same forwarding
   behavior as test 1, but no "bad input csum" line — confirms the
   verify-side hash returns `0` for a well-formed packet.

## Verifying the IPv4 checksum

`hash<W>(checksum, ...)` is a magic form: when the seed is the builtin
`checksum`, the interpreter routes the call to a real one's-complement
IPv4 checksum ([sources/lucid/src/lib/midend/interpreter/InterpCore.ml:180-212](../../../sources/lucid/src/lib/midend/interpreter/InterpCore.ml#L180-L212))
instead of the normal hash function. The Tofino backend lowers the same
form to a P4 `Checksum()` extern, so the two targets agree.

The handler uses this twice:

- **Compute**: `{new_ip with hdr_csum = hash<16>(checksum, new_ip)}`,
  with `new_ip.hdr_csum` pre-zeroed.
- **Verify**: `hash<16>(checksum, ip)` — hashing the *whole*
  header including its existing csum. For a well-formed packet this must
  return `0`, per RFC 1071.

### Smoking-gun test (worked example)

Test 1 input is the h1→h2 packet at `ttl=64`, `csum=0`. By hand, summing
the IP header's 16-bit words (with csum=0):
```
0x4500 + 0x0014 + 0x0000 + 0x0000 + 0x4000
       + 0x0000 + 0x0A00 + 0x0101 + 0x0A00 + 0x0202
= 0x9C17
~0x9C17 = 0x63E8   ← csum the input *should* have carried
```
After s1 decrements TTL, the `(ttl,proto)` word drops from `0x4000` to
`0x3F00` (Δ = −0x100), so the new csum is `0x63E8 + 0x100 = 0x64E8`.
The interpreter prints exactly this in the exit packet:
```
bytes(...3f0064e80a0001010a000202) at port 2
              ^^^^
              csum
```

Test 4 confirms the verify side: we hand it the same packet but with
`csum=0x63e8` (the value we just derived as "correct"). The handler's
verify call returns 0, so no "bad input csum" line is logged.

### How to extend
- To exercise the verify path on a *malformed* packet, send any packet
  with a wrong (non-zero, non-matching) csum and confirm the handler
  prints `bad input csum (verify=...)` with the residual sum.
- Generating real test vectors by hand is tedious — a small Python script
  using `scapy.IP(...).chksum` next to `basic.json` would be the obvious
  next step. We did not add one yet to keep the example self-contained.

## Notable design choices
- **LPM via `Table.install` masks.** The P4 program uses a `lpm` key; the
  Lucid interpreter implements equivalent semantics through
  `Table.install_ternary` (which also backs the JSON `Table.install` command
  when a `mask` field is provided). The current spec uses /32 host routes
  with the default (exact) mask. For real prefixes, add a `"mask":[...]`
  entry to the install command and install longer prefixes first — the
  interpreter matches entries in install order.
- **Install-time data is a tuple `(int<48>, int<32>)`.** The two action
  install args (`dmac`, `port`) are declared positionally on the actions and
  the table's data_ty reflects that as a tuple. The JSON `Table.install`
  command's `args` list maps positionally onto the tuple fields
  (`["<dmac><48>", "<port><32>"]`).
- **Distinct record field names across the program.** Lucid resolves record
  field names globally (a `eth#dmac` reference is unified against any record
  type that has a `dmac` field), so `fwd_t` uses prefixed names
  (`fwd_dmac`/`fwd_port`/`fwd_hit`) to avoid clashing with `eth_hdr_t.dmac`.

## Known caveats
- **No ARP / no host MAC learning.** Exactly like the P4 tutorial, ARP
  resolution is assumed to have already been done; the control plane
  installs the next-hop MAC alongside the egress port.

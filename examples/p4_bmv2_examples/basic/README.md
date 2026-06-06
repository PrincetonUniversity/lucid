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

## Topology (in basic.json)
A simple pod topology. Node IDs in the spec map to `s1..s4` as
`0..3`. Host-facing ports (s1 ports 1–2, s2 ports 1–2) are deliberately left
undeclared so forwarded packets show up in each node's `Exits` list, which is
what to scan to verify correct delivery.

```
   h1 -- 1 [s1=0] 3 -------- 1 [s3=2] 2 -------- 4 [s2=1] 1 -- h3
   h2 -- 2        4 -------- 2 [s4=3] 1 -------- 3        2 -- h4
```

## Test cases (in basic.json)
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

`hash<W>(checksum, ...)` calculates a one's-complement IPv4 checksum. 
The handler uses this twice:

- **Verify**: `hash<16>(checksum, ip)` — hashing the *whole*
  header including its existing csum.
- **Compute**: `{new_ip with hdr_csum = hash<16>(checksum, new_ip)}`,
  with `new_ip.hdr_csum` pre-zeroed.

## Notes
- **LPM via `Table.install` masks.** Lucid's `Table.install_ternary` 
  (which also backs the JSON `Table.install` command
  when a `mask` field is provided) supports ordered rules with wildcard
  bits. This example uses it for LPM.
- **Install-time data is a tuple `(int<48>, int<32>)`.** The two action
  install args (`dmac`, `port`) are declared positionally on the actions and
  the table's data_ty reflects that as a tuple. The JSON `Table.install`
  command's `args` list maps positionally onto the tuple fields
  (`["<dmac><48>", "<port><32>"]`).
- **Distinct record field names across the program.** Lucid resolves record
  field names globally (a `eth#dmac` reference is unified against any record
  type that has a `dmac` field), so `fwd_t` uses prefixed names
  (`fwd_dmac`/`fwd_port`/`fwd_hit`) to avoid clashing with `eth_hdr_t.dmac`.

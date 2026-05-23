# `basic_tunnel`

Extends [`basic`](../basic/) with a custom on-top "MyTunnel" header. The
switch program has two tables and two parse branches:

- **`ipv4_lpm`**: same as `basic` — plain IPv4 packets (ety `0x0800`) get
  MAC rewrite, TTL decrement, and checksum recompute.
- **`myTunnel_exact`**: tunneled packets (ety `0x1212`) carry a 4-byte
  tunnel header `(proto_id, dst_id)` between ethernet and IPv4, and are
  forwarded purely by `dst_id`. No MAC rewrite, no TTL touch, no checksum
  recompute — the encapsulated IPv4 rides through unchanged.

## Files
- [basic_tunnel.dpt](basic_tunnel.dpt) — the Lucid program.
- [basic_tunnel.json](basic_tunnel.json) — interpreter spec: 3-switch
  triangle topology, `Table.install` commands for both tables on all
  switches, four test packets.

## Running
```bash
../../../sources/lucid/dpt basic_tunnel.dpt --spec basic_tunnel.json --silent
```

## Topology
A 3-switch triangle (matches the P4 tutorial's `topology.json`). Lucid
node IDs map to `s1..s3` as `0..2`. Host-facing ports (`s1:1`, `s2:1`,
`s3:1`) are deliberately undeclared so packets show up in `Exits` for
verification.

```
              h1
              |
              1
   [s1=0] 2 ------- 2 [s2=1] 1 -- h2
          3              3
          |              |
          2              3
        [s3=2] 1 -- h3
```

## Test cases
1. **Plain IPv4 h1 → h2** (csum=0 on input). Two hops s1 → s2. Logs a
   "bad input csum" warning at s1; s2 forwards cleanly because s1's
   recompute produced a valid csum. Exits at `1:1` with `ttl=62`,
   `csum=0x65e8`, `dmac=08:00:00:00:02:22`.
2. **Plain IPv4 h1 → h3 with a correct input csum** (`0x62e7`). Two hops
   s1 → s3. No verify warnings. Exits at `2:1` with `ttl=62`,
   `csum=0x64e7`, `dmac=08:00:00:00:03:33`.
3. **Tunneled h1 → h3** with `dst_id=3`. Two hops s1 → s3 via the
   tunnel table. **Exit bytes are byte-identical to the input** (only
   the egress port changes between hops) — the smoking-gun that the
   tunnel path performs no rewrites.
4. **Tunneled with bad dst_id=9**. Drops at s1 via `myTunnel_exact`'s
   default action; no exit packet.

## Notable design choices
- **Two packet events, not one.** `ipv4_pkt(eth, ip, pl)` and
  `tunnel_pkt(eth, tun, ip, pl)` are separate events; the parser
  dispatches based on ethertype. This is cleaner than a single event
  with an optional tunnel field because each handler only deals with
  what its packet actually contains. The Tofino backend would lower the
  two events to the equivalent P4 conditional-emit on parse outcomes.
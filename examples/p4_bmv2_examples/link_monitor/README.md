# `link_monitor`

Per-egress-port telemetry collected by probe packets that traverse a
source-routed path. Each switch maintains two arrays:

- `byte_cnt_reg[port]` — packets sent out that port since the last probe.
- `last_time_reg[port]` — timestamp of the last probe through that port.

When a probe egresses a port, it atomically samples-and-resets the byte
counter, samples-and-updates the last_time, and pushes a tuple
`(swid=self, port, byte_cnt, last_time, cur_time)` onto its accumulated
chain. The receiver of the probe (a host, or in our case a printf at the
last hop) reads the full hop list.

## Files
- [link_monitor.dpt](link_monitor.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator. Builds IPv4 traffic and
  probe events.
- [link_monitor.json](link_monitor.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt link_monitor.dpt --spec link_monitor.json --silent
```

## Test cases (driven by `gen_spec.py`)

1. **3 IPv4 packets h1→h2.** Each forwards through s1 (egress port 2)
   then s2 (egress port 1), bumping `byte_cnt_reg` at both ports.
2. **Probe along [2, 1].** Walks s1:p2 → s2:p1. Expected telemetry:
   - hop[0] (s2:p1): `bc=3`, `last=0`
   - hop[1] (s1:p2): `bc=3`, `last=0`
3. **2 more IPv4 packets h1→h2.** Both counters now sit at 2.
4. **Probe along [2, 1] again.** Telemetry:
   - hop[0] (s2:p1): `bc=2`, `last=` probe 2's `cur` at s2 (6200)
   - hop[1] (s1:p2): `bc=2`, `last=` probe 2's `cur` at s1 (5600)
5. **3-hop detour probe along [3, 3, 1]** (s1:p3 → s3:p3 → s2:p1).
   All `bc=0` because no IPv4 traffic ever traversed s1:p3 or s3:p3.
   `last` for s2:p1 is non-zero (set by probe 4).

The `printf` `probe DONE` block at the final hop dumps the full chain
in push-front order (most recent first).

## Notable Lucid details

- The `probe` event is just a regular Lucid event with vector args
  carrying both stacks as fixed-size `int<32>[4]` arrays, so we don't need 
  a parser. This treats probes as a *control protocol* rather than a wire-format
  packet. If you ever need a real wire format (to talk to non-Lucid endpoints), 
  you'd recover the per-depth event variant approach from `source_routing`/`mri`.
- Probes are injected via the JSON spec's `"events"` list — same way you'd
  inject any non-packet event in any other Lucid program. `generate_port`
  ferries them between switches at runtime. At the last hop the event is
  emitted out a host port and lands in the `Exits` list.
- **Global declaration order matters across handlers.**
  `ipv4_lpm → byte_cnt_reg → last_time_reg`. Both handlers (`ipv4_pkt`
  and `probe`) access only some of these but in declaration order, so
  the typechecker is happy. The `probe` handler skips `ipv4_lpm`
  (allowed); the `ipv4_pkt` handler skips `last_time_reg` (allowed).
- **`Array.update(arr, idx, get_val, _, set_to_arg, now)`** is the
  natural Lucid idiom for "atomically read the old value and write a
  new value." We use it for both sample-and-reset (`zero_out` as the
  set memop) and sample-and-replace (`set_to_arg`).

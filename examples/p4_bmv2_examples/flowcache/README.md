# `flowcache`

An exact-match flow cache keyed on `(protocol, src_ip, dst_ip)`. On a
hit, the cached `(dmac, port)` is used to forward. On a miss, the
switch emits a **PacketIn** control event to the controller and drops
the original packet; the controller is expected to install a matching
rule (via `Table.install`) and from then on packets in that flow are
forwarded by the data plane.

## Files
- [flowcache.dpt](flowcache.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [flowcache.json](flowcache.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt flowcache.dpt --spec flowcache.json --silent
```

## The "controller" is the JSON spec

Lucid's interpreter lets test specifications be used to model the controller. 

- The data plane emits PacketIn events to a designated controller port
  (`CONTROLLER_PORT = 99`). The port has no link, so the events land
  in the `Exits` list — observable by the test.
- The spec mixes packet events with `Table.install` commands. A typical
  flow:
    1. Send a burst of packets in flow A → they miss → PacketIn events
       show up in Exits.
    2. The spec issues `Table.install` for flow A.
    3. Subsequent flow-A packets hit the cache and forward.


## Test timeline (in `gen_spec.py`)

| `t`      | Event                                          | Expected |
|----------|------------------------------------------------|----------|
| 1000–1400 | 3 × TCP flow-A packets `10.0.1.1 → 10.0.2.2`  | 3 MISS, 3 `packet_in` in Exits |
| 1600     | `Table.install flow_cache key=(6, 10.0.1.1, 10.0.2.2)` | — |
| 1800–2200 | 3 × TCP flow-A packets, same key              | 3 HIT, 3 forwarded out port 2 |
| 2400     | 1 × TCP flow-B packet `10.0.1.1 → 10.0.3.3`   | MISS, 1 more `packet_in` in Exits |

End-state counters:
- `hit_count[2]  = 3`  (low nibble of `0x0a000202` = 2)
- `miss_count[2] = 3`
- `miss_count[3] = 1`

## Notable Lucid details

- **Record-typed table key.** `Table.t<<flow_key_t, ...>>` works
  cleanly with a record as the key type. In the JSON
  `Table.install`, the record is flattened to a list of width-tagged
  values: `"key": ["6<8>", "<src><32>", "<dst><32>"]` — in declaration
  order of the record's fields. Same flattening you'd see for record
  *data* (already used in `basic`, `basic_tunnel`, etc.).
- **PacketIn is a regular event with `{skip;}` body.** No wire format,
  no parser, no handler — it exists purely to be `generate_port`'d out
  the controller port so the test can observe its arguments in the
  Exits list. Same pattern as `link_monitor`.

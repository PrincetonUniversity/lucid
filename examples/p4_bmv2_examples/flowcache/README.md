# `flowcache`

An exact-match flow cache keyed on `(protocol, src_ip, dst_ip)`. On a
hit, the cached `(dmac, port)` is used to forward. On a miss, the
switch emits a **PacketIn** event to the controller and drops
the original packet; the controller is expected to install a matching
rule (via `Table.install`) to forward the rest of the packets in the flow.

## Files
- [flowcache.dpt](flowcache.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [flowcache.json](flowcache.json) — generated artifact.

## Running
```bash
./gen_spec.py
dpt flowcache.dpt --spec flowcache.json --silent
```

## The "controller" is the JSON spec
Test specifications can model controller operations. 

- The data plane emits PacketIn events to a designated controller port
  (`CONTROLLER_PORT = 99`). The port has no link, so the events land
  in the `Exits` list of interpreter output.
- `Table.install` commands in the test spec model controller actions.


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

## Notes

- **Record-typed table key.** `Table.t<<flow_key_t, ...>>` works
  cleanly with a record as the key type. In the JSON
  `Table.install`, the record is flattened to a list of width-tagged
  values: `"key": ["6<8>", "<src><32>", "<dst><32>"]` — in declaration
  order of the record's fields.
- **PacketIn is a regular event with `{skip;}` body.** It exists 
  purely to be `generate_port`'d out the controller port.
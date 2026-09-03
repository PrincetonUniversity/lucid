# `multicast`

An L2 switch with four host ports.

- **Known dst MAC** → unicast to its specific port.
- **Unknown dst MAC** → flood to every port except the ingress port.

## Files
- [multicast.dpt](multicast.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [multicast.json](multicast.json) — generated artifact.

## Running
```bash
./gen_spec.py
dpt multicast.dpt --spec multicast.json --silent
```

## Test cases (in `gen_spec.py`)

All packets originate at h1 (port 1). The `mac_lookup` table has
entries for h1–h4 installed before the burst.

| Input                            | Expected `Exits`                |
|----------------------------------|---------------------------------|
| h1 → h2 (known)                  | port 2 only                     |
| h1 → h3 (known)                  | port 3 only                     |
| h1 → `00:00:00:00:00:99` (unknown) | ports 2, 3, 4                 |
| h1 → `ff:ff:ff:ff:ff:ff` (bcast) | ports 2, 3, 4                   |

The flood cases also produce an abstract `eth_pkt(...) at port -2`
entry. That's the interpreter's internal record of the flood action
itself — `-2` decodes as `-(ingress + 1)`, i.e., "flood excluding
port 1". It's *not* a duplicate copy of the packet, just metadata.

## How `flood` works

`flood <port>` is a built-in expression that constructs a multicast
group of every declared port on the switch *except* `<port>`.
`generate_ports(flood ingress_port, ev)` then sends `ev` to each port
in that group.

Flood only considers declared ports, so the topology block 
of the interpreter spec declares all 4 host ports as link ports, 
even though they are not connected in the links block.

## Notes
- **Default action returns the "flood" sentinel.** Actions can't 
generate events, so the default action returns a  `fwd_t` with 
`fwd_flood = true`, and the handler then decides between 
`generate_port` and `generate_ports`.

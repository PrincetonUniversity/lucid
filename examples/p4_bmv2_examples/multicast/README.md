# `multicast` — Lucid port of the P4 multicast / L2-flooding tutorial

An L2 switch with four host ports.

- **Known dst MAC** → unicast to its specific port.
- **Unknown dst MAC** → flood to every port except the ingress port.

## Files
- [multicast.dpt](multicast.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [multicast.json](multicast.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt multicast.dpt --spec multicast.json --silent
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

For the example to behave as expected, the topology block has to
declare all four host ports — `flood` enumerates the switch's declared
ports, not "every conceivable port number." We use four `link`-type
ports with no `links` entries; the interpreter picks them up in 
the flood enumeration. Packets emitted to them land in `Exits`.

## Notable Lucid details
- **Default action returns the "flood" sentinel.** Rather than calling
  flood from inside the action (actions can't generate events), the
  default action returns a `fwd_t` with `fwd_flood = true`, and the
  handler then decides between `generate_port` and `generate_ports`.
  Same pattern we used for `fwd_hit` in earlier examples.

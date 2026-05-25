# `source_routing`

Packets with ether-type 0x1234 carry a stack of {bos:1, port:15} labels
between ethernet and IPv4. Each switch on the route pops the top label 
and forwards on the encoded port. The label with bos=1 marks the 
last hop, which strips the source-route header and emits the inner 
IPv4 packet plain.

## Files
- [source_routing.dpt](source_routing.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator. Topology + test packets.
- [source_routing.json](source_routing.json) — committed artifact; regenerate
  with `python gen_spec.py`.

## Running
```bash
python3 gen_spec.py
../../../dpt source_routing.dpt --spec source_routing.json --silent
```

## Test cases (defined in `gen_spec.py`)
| # | Route                                   | Labels       | Expected exit |
|---|-----------------------------------------|--------------|---------------|
| 1 | h1 → h2 via s1, s2                      | `[2, 1]`     | `1:1`         |
| 2 | h1 → h3 via s1, s3                      | `[3, 1]`     | `2:1`         |
| 3 | h1 → h2 indirect via s1, s3, s2         | `[3, 3, 1]`  | `1:1`         |
| 4 | h1 → h2 via s1, s2, s3, s2 (MAX_HOPS)   | `[2, 3, 3, 1]`| `1:1`        |
| 5 | stack overflow (5 labels, no bos=1)     | —            | drop          |

The exit packets are byte-identical (eth dst/src/ety, plain IPv4) — all the
stack handling is the parser/handler's work; the final wire packet has no
source-route header.

## Topology
3-switch triangle, one host per switch. Same shape as `load_balance`.

```
   h1 - 1 [s1=0] 2 ---------- 2 [s2=1] 1 - h2
                3                    3
                |                    |
                2                    3
              [s3=2] 1 - h3 ----------
```

## Lucid notes
- **Parser slot analysis** requires that each positional event arg
  resolve to a *distinct* variable. Passing the same literal/variable to
  two arg positions in `generate(...)` is rejected with an error of the
  form "Parameter `pX` and `pY` ... must share the same slot."
  Specifically, sharing a single `zero` variable across two padding slots
  in an event constructor doesn't compile — each slot needs its own
  named local. (This was a 30-minute mystery the first time.)
- **Parser event args must be bare variables, `Payload.parse(pkt)`, or tuples.**
  Literals, vector expressions, and record constructors in a parser-side
  `generate(...)` all get rejected for now.
- **Polymorphic tuples for generic handlers.** The combination of handlers 
  with polymorphic arguments and tuples make it clean to express 
  handlers that are generic to parts of the header stack.
- **Vectors in packet-event args do not currently work.**  Use scalar 
   fields instead. Note: non-packet events handle vector args fine.
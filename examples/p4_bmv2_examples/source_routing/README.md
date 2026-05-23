# `source_routing`

Packets with ether-type `0x1234` carry a *stack* of `(bos:1, port:15)` labels
between ethernet and IPv4. Each switch on the route reads the top label,
forwards on the encoded port, and pops the label. Whichever label has
`bos=1` marks the *last* hop — that switch strips the source-route header
entirely and emits the inner IPv4 packet plain.

No tables. No control plane. The source route is in the packet.

## Files
- [source_routing.dpt](source_routing.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator. Topology + test packets.
- [source_routing.json](source_routing.json) — committed artifact; regenerate
  with `python gen_spec.py`.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt source_routing.dpt --spec source_routing.json --silent
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
- **Parser event args must be bare variables (or `Payload.parse(pkt)`).**
  Literals, vector expressions, and record constructors in a parser-side
  `generate(...)` all get rejected. Hoist them into locals first.
  Discovered while trying to pass `[p0; p1; 0; 0]` as a single event arg.
- **Vectors in packet-event args don't survive slot analysis.** A field
  of type `int<15>[4]` on a `packet event` lowers through vector→tuple→
  flat-args, and the analyzer trips on the flattened literal-zero
  positions. Use explicit scalar fields instead. (Non-packet events
  appear to handle vector args fine — see
  `sources/lucid/examples/publications/popl22/starflow.dpt`.)
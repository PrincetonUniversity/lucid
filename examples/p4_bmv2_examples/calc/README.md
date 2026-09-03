# `calc`

A host sends a packet with ethertype `0x1234` and a 16-byte calculator
header `(P, 4, ver, op, operand_a, operand_b, res)`. The switch performs
the requested arithmetic on `(operand_a, operand_b)`, writes the result
into `res`, swaps the source/destination MAC addresses, and reflects the
packet back out the ingress port. Malformed packets (bad magic, unknown
op) are silently dropped.

## Files
- [calc.dpt](calc.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy-based test case generator, produces 
  [calc.json](calc.json). Edit the `TESTS` list to add cases; do **not**
  hand-edit `calc.json`.
- [calc.json](calc.json) — committed for reproducibility.

## Running
```bash
./gen_spec.py     # if you changed TESTS
dpt calc.dpt --spec calc.json --silent
```

`gen_spec.py` needs scapy (`pip install scapy`).

## Test cases (defined in `gen_spec.py`)
| Input          | Expected `res` in reflected packet |
|----------------|------------------------------------|
| `5 + 3`        | `8`                                |
| `10 - 4`       | `6`                                |
| `0xF & 0xA`    | `0xA`                              |
| `5 \| 3`       | `7`                                |
| `5 ^ 3`        | `6`                                |
| `1 * 1` (bad op `'*'`) | dropped, no exit           |
| `1 + 1` with `p='Q'` (bad magic) | dropped, no exit |

Each reflected packet should appear in `Exits` at port 1 with the
ethernet src/dst swapped relative to the input.

## Notes
- **Bitwise XOR is `^^`.** Single `^` in Lucid is bitstring concat (so
  beware of the shape `a ^ b` ever silently meaning the wrong thing).
- **No `lookahead` in the parser.** Lucid has no lookahead,
  so we extract first and validate in the handler.
- **No early `return` from handlers.** Lucid handlers don't support
  early-exit, so we use a flag and `if/else`.
- **`printf` only supports `%d`.** Op bytes are
  printed in decimal — `+` shows as `43`, `-` as `45`, etc.

## Generating spec files with scapy

This example uses a Python script to generate the test json. 
The pattern:
1. Define each header type as a tiny scapy `Packet` subclass with
   `fields_desc` whose field widths match the Lucid `type` declarations.
2. Construct test packets by composing `Ether() / MyHeader(...)` and
   calling `bytes(...).hex()`.
3. Append the resulting strings into the `events` list and
   `json.dump` to `<name>.json`.

This helps with more complicated programs and tests.
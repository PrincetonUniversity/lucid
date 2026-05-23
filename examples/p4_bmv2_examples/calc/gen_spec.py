#!/usr/bin/env python3
"""Generate calc.json: the interpreter spec for the Lucid calc example.

Run with `python gen_spec.py`. Overwrites calc.json next to this script.
Edit the `TESTS` list below to add or change test cases — packet bytes
and the spec scaffolding are generated from those entries, so there is
no opportunity for hand-counted hex strings to drift out of sync with
the program's record layout.
"""

import json
from pathlib import Path

from scapy.all import Ether, Packet, ByteField, IntField, bind_layers

# ---- protocol layout ------------------------------------------------------

ETY_CALC = 0x1234

# 16-byte calc header — must match the `calc_t` record in calc.dpt
# (p, four, ver, op, operand_a, operand_b, res), all big-endian on the wire.
class P4Calc(Packet):
    name = "P4Calc"
    fields_desc = [
        ByteField("p",         ord("P")),
        ByteField("four",      ord("4")),
        ByteField("ver",       0x01),
        ByteField("op",        0),
        IntField("operand_a",  0),
        IntField("operand_b",  0),
        IntField("res",        0),
    ]

bind_layers(Ether, P4Calc, type=ETY_CALC)

# ---- test scaffolding -----------------------------------------------------

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:01:02"

OP = {c: ord(c) for c in "+-&|^"}

def calc_bytes(op, a, b, *,
               src=H1_MAC, dst=H2_MAC,
               p=ord("P"), four=ord("4"), ver=0x01):
    """Construct a calc packet on the wire and return its hex string."""
    eth = Ether(dst=dst, src=src, type=ETY_CALC)
    body = P4Calc(p=p, four=four, ver=ver, op=op,
                  operand_a=a, operand_b=b, res=0)
    return bytes(eth / body).hex()

# Each entry: (label, op-byte, operand_a, operand_b, kwargs)
TESTS = [
    ("5 + 3 = 8",         OP["+"], 5,        3,        {}),
    ("10 - 4 = 6",        OP["-"], 10,       4,        {}),
    ("0xF & 0xA = 0xA",   OP["&"], 0xF,      0xA,      {}),
    ("5 | 3 = 7",         OP["|"], 5,        3,        {}),
    ("5 ^ 3 = 6",         OP["^"], 5,        3,        {}),
    ("bad op '*' (drop)", ord("*"), 1,       1,        {}),
    ("bad magic p='Q'",   OP["+"], 1,        1,        {"p": ord("Q")}),
]

# ---- spec assembly --------------------------------------------------------

events = []
ts = 1000
for _label, op, a, b, kw in TESTS:
    events.append({
        "type": "packet",
        "bytes": calc_bytes(op, a, b, **kw),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 1000

spec = {
    "max time": 20000,
    "default_input_gap": 100,
    "events": events,
}

out = Path(__file__).with_name("calc.json")
out.write_text(json.dumps(spec, indent=2) + "\n")

print(f"wrote {out} with {len(events)} packet events")
for (label, *_), ev in zip(TESTS, events):
    print(f"  t={ev['timestamp']:>5}  {label}")

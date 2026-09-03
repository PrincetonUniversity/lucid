#!/usr/bin/env python3
"""Generate multicast.json for the Lucid multicast example.

Single switch, four host ports. All four ports are declared in the
topology block (as `link` type with no actual link) so that the
`flood ingress_port` builtin can enumerate them on a cache miss.
Unlinked declared ports produce exit events on emission — the test
inspects Exits to confirm fan-out.
"""

import json
from pathlib import Path

from scapy.all import Ether

H1, H2, H3, H4 = (
    "08:00:00:00:01:11",
    "08:00:00:00:02:22",
    "08:00:00:00:03:33",
    "08:00:00:00:04:44",
)
HOSTS = [("h1", H1, 1), ("h2", H2, 2), ("h3", H3, 3), ("h4", H4, 4)]
BCAST = "ff:ff:ff:ff:ff:ff"
UNKNOWN = "00:00:00:00:00:99"  # not in the install list

def mac_int(s): return int(s.replace(":", ""), 16)

def install_mac(mac, port):
    return {
        "type": "command", "name": "Table.install",
        "args": {
            "table": "mac_lookup",
            "key": [f"{mac_int(mac)}<48>"],
            "action": "mac_lookup.mac_forward",
            "args": [f"{port}<32>"],
        },
    }

def eth_packet(src_mac, dst_mac, payload_hex="cafebabe"):
    pkt = Ether(dst=dst_mac, src=src_mac, type=0x9999)  # arbitrary non-IP
    return (bytes(pkt) + bytes.fromhex(payload_hex)).hex()

# Declare all 4 host ports as link-type (with no actual links) so flood
# enumerates them. The simulator emits exit events on unlinked declared
# ports, which is exactly what we want for the test.
TOPOLOGY = {
    "nodes": {
        "0": {
            "ports": {
                "1": {"type": "link"},
                "2": {"type": "link"},
                "3": {"type": "link"},
                "4": {"type": "link"},
            }
        }
    },
    "links": [],
}

events = []

# Install entries for h1..h4.
for _name, mac, port in HOSTS:
    events.append(install_mac(mac, port))

# Test packets, all originating at h1 (port 1):
TESTS = [
    # Known dst → unicast.
    ("h1 → h2 (known unicast, expect Exit at port 2)",         H1, H2),
    ("h1 → h3 (known unicast, expect Exit at port 3)",         H1, H3),
    # Unknown dst → flood except ingress (ports 2, 3, 4).
    ("h1 → 00:..:99 (unknown, expect Exits at 2, 3, 4)",       H1, UNKNOWN),
    # Broadcast → also unknown → flood.
    ("h1 → ff:ff:ff:ff:ff:ff (bcast, expect Exits at 2, 3, 4)", H1, BCAST),
]

ts = 5000
for label, src, dst in TESTS:
    events.append({
        "type": "packet",
        "bytes": eth_packet(src, dst),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 1000

spec = {
    "max time": 15000,
    "default_input_gap": 100,
    "topology": TOPOLOGY,
    "events": events,
}

out = Path(__file__).with_name("multicast.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events ({len(HOSTS)} installs + {len(TESTS)} packets)")
for label, *_ in TESTS:
    print(f"  - {label}")

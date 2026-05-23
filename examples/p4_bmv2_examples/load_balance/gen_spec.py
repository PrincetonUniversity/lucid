#!/usr/bin/env python3
"""Generate load_balance.json for the Lucid load_balance example.

Run with `python gen_spec.py`. Overwrites load_balance.json next to this
script. Scapy builds the wire-format packets (ethernet/IPv4/TCP with valid
checksums); this file also generates the topology block and the table
install events (a lot of repetition is easier to maintain in Python).
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import Ether, IP, TCP

# ---- topology ------------------------------------------------------------
#
# Node IDs map: 0=s1, 1=s2, 2=s3. Host-facing ports (each switch's port 1)
# are left undeclared so forwarded packets show up in Exits.

TOPOLOGY = {
    "nodes": {
        "0": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
        "1": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
        "2": {"ports": {"2": {"type": "link"}, "3": {"type": "link"}}},
    },
    "links": [
        {"0:2": "1:2"},
        {"0:3": "2:2"},
        {"1:3": "2:3"},
    ],
}

# ---- helpers -------------------------------------------------------------

def ipv4_int(s):
    return int(ipaddress.IPv4Address(s))

def mac_int(s):
    return int(s.replace(":", ""), 16)

def install_ecmp_group(node, dst_ip_str, base, count, action="ecmp_group.set_ecmp_params"):
    return {
        "type": "command", "name": "Table.install", "locations": [node],
        "args": {
            "table": "ecmp_group",
            "key": [f"{ipv4_int(dst_ip_str)}<32>"],
            "action": action,
            "args": [f"{base}<16>", f"{count}<32>"],
        },
    }

def install_ecmp_nhop(node, select, dmac_str, nhop_ip_str, port):
    return {
        "type": "command", "name": "Table.install", "locations": [node],
        "args": {
            "table": "ecmp_nhop",
            "key": [f"{select}<16>"],
            "action": "ecmp_nhop.set_nhop",
            "args": [f"{mac_int(dmac_str)}<48>",
                     f"{ipv4_int(nhop_ip_str)}<32>",
                     f"{port}<32>"],
        },
    }

def install_send_frame(node, port, smac_str):
    return {
        "type": "command", "name": "Table.install", "locations": [node],
        "args": {
            "table": "send_frame",
            "key": [f"{port}<32>"],
            "action": "send_frame.rewrite_mac",
            "args": [f"{mac_int(smac_str)}<48>"],
        },
    }

def tcp_packet_bytes(src_ip, dst_ip, src_port, dst_port,
                     src_mac="08:00:00:00:01:01",   # h1
                     dst_mac="08:00:00:00:01:00",   # h1's gateway (s1)
                     ttl=64, seq=0):
    """Build a TCP packet on the wire, with a valid IPv4 checksum."""
    p = (Ether(dst=dst_mac, src=src_mac, type=0x0800) /
         IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=0, len=40) /
         TCP(sport=src_port, dport=dst_port, seq=seq, ack=0,
             dataofs=5, reserved=0, flags=0, window=0, urgptr=0))
    # Force scapy to compute the IPv4 checksum.
    raw = bytes(p)
    return raw.hex()

# ---- control events: table installs -------------------------------------

events = []

# s1 (node 0): load-balance 10.0.0.1 across {h2, h3}.
events += [
    install_ecmp_group(0, "10.0.0.1", base=0, count=2),
    install_ecmp_nhop(0, select=0, dmac_str="08:00:00:00:02:02",
                      nhop_ip_str="10.0.2.2", port=2),
    install_ecmp_nhop(0, select=1, dmac_str="08:00:00:00:03:03",
                      nhop_ip_str="10.0.3.3", port=3),
    install_send_frame(0, port=2, smac_str="08:00:00:00:01:00"),
    install_send_frame(0, port=3, smac_str="08:00:00:00:01:00"),
]

# s2 (node 1): trivial single-path to h2.
events += [
    install_ecmp_group(1, "10.0.2.2", base=0, count=1),
    install_ecmp_nhop(1, select=0, dmac_str="08:00:00:00:02:02",
                      nhop_ip_str="10.0.2.2", port=1),
    install_send_frame(1, port=1, smac_str="08:00:00:00:02:00"),
]

# s3 (node 2): trivial single-path to h3.
events += [
    install_ecmp_group(2, "10.0.3.3", base=0, count=1),
    install_ecmp_nhop(2, select=0, dmac_str="08:00:00:00:03:03",
                      nhop_ip_str="10.0.3.3", port=1),
    install_send_frame(2, port=1, smac_str="08:00:00:00:03:00"),
]

# ---- test packets --------------------------------------------------------
#
# Several flows from h1 → 10.0.0.1 with different TCP src ports. The
# expectation is that s1's hash splits these across (h2, h3); we cannot
# control which port any individual flow lands on, but with enough flows we
# should see both buckets exercised.

TESTS = [
    ("flow A: h1->10.0.0.1, sport=1111", "10.0.1.1", "10.0.0.1", 1111, 80),
    ("flow B: h1->10.0.0.1, sport=2222", "10.0.1.1", "10.0.0.1", 2222, 80),
    ("flow C: h1->10.0.0.1, sport=3333", "10.0.1.1", "10.0.0.1", 3333, 80),
    ("flow D: h1->10.0.0.1, sport=4444", "10.0.1.1", "10.0.0.1", 4444, 80),
    ("flow E: h1->10.0.0.1, sport=5555", "10.0.1.1", "10.0.0.1", 5555, 80),
    ("flow F: h1->10.0.0.1, sport=6666", "10.0.1.1", "10.0.0.1", 6666, 80),
    ("direct: h1->h2 (s1 has no entry for 10.0.2.2 -> drop)",
     "10.0.1.1", "10.0.2.2", 1000, 80),
    ("unroutable: h1->10.99.99.99 (drop at ecmp_group)",
     "10.0.1.1", "10.99.99.99", 1000, 80),
]

ts = 5000
for label, src, dst, sport, dport in TESTS:
    events.append({
        "type": "packet",
        "bytes": tcp_packet_bytes(src, dst, sport, dport),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 500

# ---- assemble + write ---------------------------------------------------

spec = {
    "max time": 30000,
    "default_input_gap": 100,
    "topology": TOPOLOGY,
    "events": events,
}

out = Path(__file__).with_name("load_balance.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events "
      f"(installs + {len(TESTS)} packets)")
for label, *_ in TESTS:
    print(f"  - {label}")

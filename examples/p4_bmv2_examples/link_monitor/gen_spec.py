#!/usr/bin/env python3
"""Generate link_monitor.json for the Lucid link_monitor example.

Probe events are regular (non-packet) Lucid events: we inject them
directly from the spec rather than building wire-format packets and
running them through a parser. IPv4 packets are still real on-the-wire
packets (built with scapy).
"""

import ipaddress
import json
from pathlib import Path

from scapy.all import Ether, IP

# ---- topology (same triangle as source_routing / mri) -------------------

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

# ---- helpers ------------------------------------------------------------

def ipv4_int(s): return int(ipaddress.IPv4Address(s))
def mac_int(s):  return int(s.replace(":", ""), 16)

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:02:02"
H3_MAC = "08:00:00:00:03:03"
S1_MAC = "08:00:00:00:01:00"
S2_MAC = "08:00:00:00:02:00"
S3_MAC = "08:00:00:00:03:00"

def install_lpm(node, dst_ip, dmac, port):
    return {
        "type": "command", "name": "Table.install", "locations": [node],
        "args": {
            "table": "ipv4_lpm",
            "key": [f"{ipv4_int(dst_ip)}<32>"],
            "action": "ipv4_lpm.ipv4_forward",
            "args": [f"{mac_int(dmac)}<48>", f"{port}<32>"],
        },
    }

def ipv4_packet(src_ip, dst_ip, ttl=64, src_mac=H1_MAC, dst_mac=S1_MAC):
    p = (Ether(dst=dst_mac, src=src_mac, type=0x0800) /
         IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=0, len=20))
    return bytes(p).hex()

def probe_event(route, n_data=0,
                swids=(0,)*4, ports=(0,)*4,
                byte_cnts=(0,)*4, last_times=(0,)*4, cur_times=(0,)*4,
                location_node=0, location_port=1, timestamp=None):
    """Build a probe event for the JSON spec.

    `route` is a list of upcoming egress ports (max 4 entries). It's
    zero-padded on the right.
    """
    assert 1 <= len(route) <= 4
    route_padded = list(route) + [0] * (4 - len(route))
    n_route = len(route)
    args = (
        [n_route, n_data] +
        list(route_padded) +
        list(swids) +
        list(ports) +
        list(byte_cnts) +
        list(last_times) +
        list(cur_times)
    )
    ev = {
        "name": "probe",
        "args": args,
        "locations": [f"{location_node}:{location_port}"],
    }
    if timestamp is not None:
        ev["timestamp"] = timestamp
    return ev

# ---- control plane: ipv4_lpm install on all 3 switches ------------------

events = []

# s1 (node 0)
events += [
    install_lpm(0, "10.0.1.1", H1_MAC, port=1),
    install_lpm(0, "10.0.2.2", S2_MAC, port=2),
    install_lpm(0, "10.0.3.3", S3_MAC, port=3),
]
# s2 (node 1)
events += [
    install_lpm(1, "10.0.1.1", S1_MAC, port=2),
    install_lpm(1, "10.0.2.2", H2_MAC, port=1),
    install_lpm(1, "10.0.3.3", S3_MAC, port=3),
]
# s3 (node 2)
events += [
    install_lpm(2, "10.0.1.1", S1_MAC, port=2),
    install_lpm(2, "10.0.2.2", S2_MAC, port=3),
    install_lpm(2, "10.0.3.3", H3_MAC, port=1),
]

# ---- traffic + probes ---------------------------------------------------
#
# Plan:
#  1. Send a handful of IPv4 packets h1→h2 to bump byte_cnt on s1:p2 and
#     s2:p1. Each ipv4_pkt forward at egress port P increments
#     byte_cnt_reg[P] by 1.
#  2. Send a probe along the same path (s1:p2 → s2:p1). It should sample
#     the accumulated counts, reset them, and emit a DONE log with the
#     telemetry at the end.
#  3. Send a second probe along the same path. byte_cnt was reset, so its
#     captured values should be near 0.
#  4. Send a 3-hop probe through s1, s3, s2.

# (1) some IPv4 traffic
ts = 5000
for _ in range(3):
    events.append({
        "type": "packet",
        "bytes": ipv4_packet("10.0.1.1", "10.0.2.2"),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 200

# (2) probe along s1→s2 (route = [2, 1])
events.append(probe_event(route=[2, 1], location_node=0, location_port=1,
                          timestamp=ts))
ts += 500

# small spacer + a few more IPv4 packets (these only refill byte_cnt on
# s1:p2 — not on s2:p1, because the probe already reset s2:p1 *after*
# they would have passed through; but the probe runs *after* these, so
# they do count for s2:p1 too).
for _ in range(2):
    events.append({
        "type": "packet",
        "bytes": ipv4_packet("10.0.1.1", "10.0.2.2"),
        "locations": ["0:1"],
        "timestamp": ts,
    })
    ts += 200

# (3) second probe along the same route. byte_cnt should be small (only
# the 2 packets since the first probe).
events.append(probe_event(route=[2, 1], location_node=0, location_port=1,
                          timestamp=ts))
ts += 500

# (4) 3-hop probe through s1, s3, s2 (route = [3, 3, 1]).
#  s1 → port 3 (s3 link)
#  s3 → port 3 (s2 link)
#  s2 → port 1 (host h2)
events.append(probe_event(route=[3, 3, 1], location_node=0, location_port=1,
                          timestamp=ts))

spec = {
    "max time": 30000,
    "default_input_gap": 100,
    "topology": TOPOLOGY,
    "events": events,
}

out = Path(__file__).with_name("link_monitor.json")
out.write_text(json.dumps(spec, indent=2) + "\n")
print(f"wrote {out} with {len(events)} events")

#!/usr/bin/env python3
"""Dynamic controller for the Lucid p4runtime example.

Spawns `dpt --interactive`, injects test packets on stdin, watches for
`packet_in` notifications on stdout, and installs flow-cache rules in
response. Implements a small "learn from src" policy: when we see a
packet_in with src S arriving on port P, we install a rule that
forwards future packets to S out port P. Subsequent packets in either
direction then hit the cache.

This is the Lucid analog of `advanced_tunnel.p4` + `mycontroller.py`
from the upstream P4 tutorial — same architecture (data-plane miss
notifies the controller, controller installs a rule, data plane
forwards on hit), but the channel is stdin/stdout JSON rather than
P4Runtime gRPC.
"""

import ipaddress
import json
import os
import select
import subprocess
import sys
import time
from pathlib import Path

from scapy.all import Ether, IP

HERE   = Path(__file__).parent
DPT    = HERE / "../../../sources/lucid/dpt"
PROG   = HERE / "p4runtime.dpt"
SPEC   = HERE / "p4runtime.json"

H1_MAC = "08:00:00:00:01:01"
H2_MAC = "08:00:00:00:02:02"
H3_MAC = "08:00:00:00:03:03"
S1_MAC = "08:00:00:00:01:00"

HOST_BY_IP = {
    "10.0.1.1": {"mac": H1_MAC, "port": 1},
    "10.0.2.2": {"mac": H2_MAC, "port": 2},
    "10.0.3.3": {"mac": H3_MAC, "port": 3},
}

def ipv4_int(s): return int(ipaddress.IPv4Address(s))
def mac_int(s):  return int(s.replace(":", ""), 16)

def build_ipv4(src_ip, dst_ip, src_mac=H1_MAC, dst_mac=S1_MAC, ttl=64):
    p = (Ether(dst=dst_mac, src=src_mac, type=0x0800) /
         IP(src=src_ip, dst=dst_ip, ttl=ttl, id=0, flags=0, frag=0,
            tos=0, len=20))
    return bytes(p).hex()

# ---- JSON helpers --------------------------------------------------------

def pkt_event(src_ip, dst_ip, ingress_port=1, ts=None):
    ev = {
        "type": "packet",
        "bytes": build_ipv4(src_ip, dst_ip),
        "locations": [f"0:{ingress_port}"],
    }
    if ts is not None:
        ev["timestamp"] = ts
    return ev

def install_rule(dst_ip, dmac, port):
    return {
        "type": "command", "name": "Table.install",
        "args": {
            "table": "ipv4_lpm",
            "key": [f"{ipv4_int(dst_ip)}<32>"],
            "action": "ipv4_lpm.cached_action",
            "args": [f"{mac_int(dmac)}<48>", f"{port}<32>"],
        },
    }

# ---- subprocess plumbing -------------------------------------------------

def drain(fd, timeout=0.3):
    """Read everything available on `fd` within `timeout` seconds."""
    out = b""
    while True:
        r, _, _ = select.select([fd], [], [], timeout)
        if not r:
            break
        chunk = fd.read(4096)
        if not chunk:
            break
        out += chunk
    return out.decode(errors="replace")

def send(p, ev):
    line = json.dumps(ev) + "\n"
    p.stdin.write(line.encode())
    p.stdin.flush()

def parse_stdout(text):
    """Parse each non-empty line of `text` as JSON; return the records."""
    records = []
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            records.append(json.loads(line))
        except json.JSONDecodeError:
            # printf records etc. — we already saw them via stderr or as
            # text; skip for the structured-record pass.
            pass
    return records

# ---- "policy" ------------------------------------------------------------

def react_to_packet_in(rec, installed):
    """If this is a packet_in event, decide what (if anything) to install.

    Policy: when we see flow (src -> dst), install a forwarding rule for
    `dst` based on a static IP→host map. We could be smarter (e.g.,
    learn the egress port from the ingress side), but in this 3-host
    setup the topology is small enough that the static map is fine.
    Returns the install command, or None.
    """
    if rec.get("name") != "packet_in":
        return None
    src_int, dst_int, ingress = rec["args"]
    src_ip = str(ipaddress.IPv4Address(src_int))
    dst_ip = str(ipaddress.IPv4Address(dst_int))
    if dst_ip in installed:
        return None
    host = HOST_BY_IP.get(dst_ip)
    if host is None:
        print(f"  controller: no host info for {dst_ip}, ignoring", file=sys.stderr)
        return None
    print(f"  controller: learned {dst_ip} -> port {host['port']} (dmac {host['mac']})",
          file=sys.stderr)
    installed.add(dst_ip)
    return install_rule(dst_ip, host["mac"], host["port"])

# ---- main loop -----------------------------------------------------------

def main():
    p = subprocess.Popen(
        [str(DPT), str(PROG), "--spec", str(SPEC), "--interactive"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        bufsize=0,
    )
    installed = set()
    ts = 1000

    def cycle(label, ev):
        nonlocal ts
        print(f"\n>>> {label}", file=sys.stderr)
        ev_with_ts = dict(ev, timestamp=ts)
        send(p, ev_with_ts)
        ts += 500
        time.sleep(0.3)
        out = drain(p.stdout)
        for line in out.splitlines():
            print(f"  dpt: {line}", file=sys.stderr)
        for rec in parse_stdout(out):
            rule = react_to_packet_in(rec, installed)
            if rule is not None:
                rule_with_ts = dict(rule, timestamp=ts)
                ts += 500
                print(f"  controller -> dpt: {json.dumps(rule)}", file=sys.stderr)
                send(p, rule_with_ts)
                time.sleep(0.2)
                # drain again — but installs don't produce stdout records
                drain(p.stdout)

    # Scenario:
    #   1. h1→h2: MISS → controller installs rule for 10.0.2.2.
    #   2. h1→h2: HIT now that the rule is in.
    #   3. h1→h3: MISS → controller installs rule for 10.0.3.3.
    #   4. h1→h3: HIT.
    cycle("h1->h2 #1 (expect MISS + controller install)",
          pkt_event("10.0.1.1", "10.0.2.2"))
    cycle("h1->h2 #2 (expect HIT)",
          pkt_event("10.0.1.1", "10.0.2.2"))
    cycle("h1->h3 #1 (expect MISS + controller install)",
          pkt_event("10.0.1.1", "10.0.3.3"))
    cycle("h1->h3 #2 (expect HIT)",
          pkt_event("10.0.1.1", "10.0.3.3"))

    # Done — close stdin and let the subprocess exit. The "stdin eof"
    # error on stderr is benign; the run is complete.
    time.sleep(0.3)
    print("\n--- final stderr from dpt: ---", file=sys.stderr)
    print(drain(p.stderr, 0.5), file=sys.stderr)
    p.stdin.close()
    p.terminate()
    try:
        p.wait(timeout=1)
    except subprocess.TimeoutExpired:
        p.kill()

if __name__ == "__main__":
    main()

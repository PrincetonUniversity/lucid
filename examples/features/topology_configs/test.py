#!/usr/bin/env python3
"""Test script for the lucid virtual switch.

Commands:
  vethsup       - create and bring up veth/feth interface pair
  vethsdown     - tear down veth/feth interface pair
  send          - send one Eth/IP/UDP packet on SEND_IFACE
  send_wait <n>        - send one packet, sniff on SEND_IFACE for n seconds, print received packets
  send_event <event>          - send an event packet, e.g. send_event "1(10u32,5u16)"
  send_wait_event <event> <n> - send an event packet, wait up to n seconds for a reply
"""

import subprocess
import sys
import os
import platform

import re
import struct
import threading
from scapy.all import Ether, IP, UDP, Raw, sendp, sniff

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

SEND_IFACE   = "feth1"
SWITCH_IFACE = "feth0"
SRC_MAC      = "02:00:00:00:00:01"  # locally-administered, used to filter out sent packets in sniff

IS_LINUX = platform.system() == "Linux"


def cmd_vethsup():
    if IS_LINUX:
        subprocess.run(
            ["sudo", "ip", "link", "add", SWITCH_IFACE, "type", "veth", "peer", "name", SEND_IFACE],
            capture_output=True,
        )
        subprocess.run(["sudo", "ip", "link", "set", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ip", "link", "set", SEND_IFACE,    "up"], check=True)
    else:
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE,   "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "peer", SEND_IFACE], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE,   "up"], check=True)
    print(f"[+] {SWITCH_IFACE} and {SEND_IFACE} are up")


def cmd_vethsdown():
    if IS_LINUX:
        subprocess.run(["sudo", "ip", "link", "delete", SWITCH_IFACE], capture_output=True)
    else:
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "destroy"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE,   "destroy"], capture_output=True)
    print(f"[+] {SWITCH_IFACE} and {SEND_IFACE} torn down")


def parse_event(event_str):
    """Parse '<tag>(<args>)' into (tag, [(value, bits), ...]).
    Each arg is '<value>u<bits>' or just '<value>' (defaults to 32 bits)."""
    m = re.fullmatch(r'(\d+)\(([^)]*)\)', event_str.strip())
    if not m:
        raise ValueError(f"Invalid event format: {event_str!r}  expected '<tag>(<args>)'")
    tag = int(m.group(1))
    args_str = m.group(2).strip()
    args = []
    if args_str:
        for part in args_str.split(','):
            part = part.strip()
            if 'u' in part:
                val_s, bits_s = part.split('u', 1)
                args.append((int(val_s), int(bits_s)))
            else:
                args.append((int(part), 32))
    return tag, args


def decode_event_pkt(pkt, arg_bits=None):
    """Decode a raw packet as an event frame, returning a readable string.
    arg_bits: list of bit widths for each arg, e.g. [32, 16]. If None, prints raw hex."""
    raw = bytes(pkt)
    if len(raw) < 16:  # 14 ethernet + 2 tag minimum
        return f"(too short: {raw.hex()})"
    tag = struct.unpack('>H', raw[14:16])[0]
    payload = raw[16:]
    if arg_bits is None:
        args_str = payload.hex() if payload else "(no args)"
    else:
        args = []
        offset = 0
        for bits in arg_bits:
            nbytes = bits // 8
            val = int.from_bytes(payload[offset:offset + nbytes], 'big')
            args.append(f"{val}u{bits}")
            offset += nbytes
        args_str = ", ".join(args)
    return f"{tag}({args_str})"


def build_event_pkt(event_str):
    tag, args = parse_event(event_str)
    payload = struct.pack('>H', tag)
    for val, bits in args:
        payload += val.to_bytes(bits // 8, 'big')
    return Ether(src=SRC_MAC, dst="ff:ff:ff:ff:ff:ff", type=0x029A) / Raw(load=payload)


def cmd_send_event(event_str):
    pkt = build_event_pkt(event_str)
    print(f"[+] Sending event: {event_str}")
    sendp(pkt, iface=SEND_IFACE, verbose=False)


def cmd_send_wait_event(event_str, n):
    _, args = parse_event(event_str)
    arg_bits = [bits for _, bits in args]

    captured = []

    def do_sniff():
        captured.extend(sniff(iface=SEND_IFACE, timeout=n, count=1,
                              lfilter=lambda p: p.src != SRC_MAC))

    t = threading.Thread(target=do_sniff, daemon=True)
    t.start()

    import time; time.sleep(0.1)

    pkt = build_event_pkt(event_str)
    print(f"[+] Sending event: {event_str}")
    sendp(pkt, iface=SEND_IFACE, verbose=False)
    print(f"[+] Waiting up to {n}s for a reply on {SEND_IFACE}...")

    t.join()

    if captured:
        print(f"[+] Received: {decode_event_pkt(captured[0], arg_bits)}")
    else:
        print("[-] No packet received")


def build_pkt():
    return Ether(dst="ff:ff:ff:ff:ff:ff") / IP(dst="10.0.0.1") / UDP(dport=5000, sport=12345)


def cmd_send():
    pkt = build_pkt()
    print(f"[+] Sending: {pkt.summary()}")
    sendp(pkt, iface=SEND_IFACE, verbose=False)


def cmd_send_wait(n):
    captured = []

    def do_sniff():
        captured.extend(sniff(iface=SEND_IFACE, timeout=n, count=1,
                              lfilter=lambda p: p.src != SRC_MAC))

    t = threading.Thread(target=do_sniff, daemon=True)
    t.start()

    # Small delay so the sniffer is listening before the packet goes out
    import time; time.sleep(0.1)

    pkt = build_pkt()
    print(f"[+] Sending: {pkt.summary()}")
    sendp(pkt, iface=SEND_IFACE, verbose=False)
    print(f"[+] Waiting up to {n}s for a reply on {SEND_IFACE}...")

    t.join()

    if captured:
        print(f"[+] Received: {captured[0].summary()}")
    else:
        print("[-] No packet received")


COMMANDS = {
    "vethsup":   (cmd_vethsup,   0),
    "vethsdown": (cmd_vethsdown, 0),
    "send":      (cmd_send,      0),
    "send_wait":   (cmd_send_wait,   1),
    "send_event":       (cmd_send_event,      1),
    "send_wait_event":  (cmd_send_wait_event, 2),
}

if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
        print(f"Usage: {sys.argv[0]} <command> [args]")
        print(f"Commands: {', '.join(COMMANDS)}")
        sys.exit(1)

    cmd, nargs = COMMANDS[sys.argv[1]]
    args = sys.argv[2:]
    if len(args) != nargs:
        print(f"[-] '{sys.argv[1]}' expects {nargs} argument(s), got {len(args)}")
        sys.exit(1)

    if sys.argv[1] == "send_wait":
        cmd(int(args[0]))
    elif sys.argv[1] == "send_event":
        cmd(args[0])
    elif sys.argv[1] == "send_wait_event":
        cmd(args[0], int(args[1]))
    else:
        cmd()

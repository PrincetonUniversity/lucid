#!/usr/bin/env python3
# Regression test for the raw-socket driver's dispatch SCHEDULING (not just plumbing).
#
# scanloop.dpt has a `tick` handler that recirculates its own event forever (a stand-in
# for a periodic table scanner), plus a `pkt_in` handler that emits one port output per
# input packet. We feed [tick, pkt_in(1..N)] on the wire and expect all N pkt_out frames
# back -- proving the driver's bounded-burst pipeline INTERLEAVES the endless recirc with
# fresh input instead of draining it to exhaustion (which would loop on `tick` forever
# and starve pkt_in -> ZERO outputs). This is the rawsock counterpart of
# test_dpdk_scanloop.py, and it exercises the slab pool + non-blocking recirculation.
#
# Topology mirrors test_rawsock.py: a veth pair (SWITCH_IFACE <-> SEND_IFACE). The switch
# binds port 0 to SWITCH_IFACE; we send the crafted frames on SEND_IFACE and capture the
# pkt_out frames (emitted to port 0) coming back inbound on SEND_IFACE.
#
# Two-phase, like the other driver tests:
#   sudo python3 test_rawsock_scanloop.py --gen   # once, where lucidcc is built
#   sudo python3 test_rawsock_scanloop.py         # gcc + run + check
#
# Requires root (raw sockets / tcpdump / veth): run with sudo.

import subprocess
import sys
import os
import re
import struct
import time
import platform

from scapy.all import Ether, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "scanloop.dpt")
BUILD_DIR = os.path.join(SCRIPT_DIR, "_rawsock_scanloop_build")
PCAP_DIR = os.path.join(SCRIPT_DIR, "pcaps")
SEND_PCAP = os.path.join(PCAP_DIR, "rawsock_scanloop.send.pcap")
RECV_PCAP = os.path.join(PCAP_DIR, "rawsock_scanloop.recv.pcap")
NUM_INPUTS = 10          # pkt_in packets fed after the tick
SWITCH_PORT = 0

LUCID_ETY = 666
DST_MAC = bytes.fromhex("000000000001")
SRC_MAC = bytes.fromhex("000000000002")

SWITCH_IFACE, SEND_IFACE = ("veth0", "veth1") if platform.system() == "Linux" else ("feth0", "feth1")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_c():
    """Regenerate the raw-socket C from scanloop.dpt via lucidcc --rawsock (opt-in,
    the only step that needs the compiler; commit the result)."""
    lucidcc = os.path.join(repo_root(), "lucidcc")
    cfile = os.path.join(BUILD_DIR, "lucidprog.c")
    os.makedirs(BUILD_DIR, exist_ok=True)
    print(f"[+] lucidcc --rawsock {os.path.basename(DPT_FILE)} -> {os.path.relpath(cfile)}")
    subprocess.run([lucidcc, DPT_FILE, "-o", cfile, "--rawsock"], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def build_switch():
    cfile = os.path.join(BUILD_DIR, "lucidprog.c")
    binfile = os.path.join(BUILD_DIR, "lucidprog")
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run `sudo python3 {os.path.basename(__file__)} --gen` first.")
    print("[+] gcc")
    subprocess.run(["gcc", "-O2", "-o", binfile, cfile], check=True)
    return binfile


def event_tags():
    c = open(os.path.join(BUILD_DIR, "lucidprog.c")).read()
    tags = {m.group(1): int(m.group(2)) for m in re.finditer(r"(\w+)_tag\s*=\s*(\d+);", c)}
    for name in ("tick", "pkt_in", "pkt_out"):
        if name not in tags:
            sys.exit(f"[-] could not find {name}_tag in generated C")
    return tags


def hdr(tag):
    return DST_MAC + SRC_MAC + struct.pack(">H", LUCID_ETY) + struct.pack(">H", tag)


def build_pcap(tags):
    frames = [hdr(tags["tick"])]  # the endless self-recirc, fed first
    frames += [hdr(tags["pkt_in"]) + struct.pack(">I", i) for i in range(1, NUM_INPUTS + 1)]
    wrpcap(SEND_PCAP, [Ether(f) for f in frames])
    print(f"[+] wrote 1 tick + {NUM_INPUTS} pkt_in frames to {os.path.basename(SEND_PCAP)}")


def ensure_veths():
    if platform.system() == "Linux":
        subprocess.run(["sudo", "ip", "link", "add", SWITCH_IFACE, "type", "veth", "peer", "name", SEND_IFACE], capture_output=True)
        for iface in (SWITCH_IFACE, SEND_IFACE):
            subprocess.run(["sudo", "sysctl", "-w", f"net.ipv6.conf.{iface}.disable_ipv6=1"], capture_output=True)
        subprocess.run(["sudo", "ip", "link", "set", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ip", "link", "set", SEND_IFACE, "up"], check=True)
    else:
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE, "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "peer", SEND_IFACE], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE, "up"], check=True)
    print(f"[+] {SWITCH_IFACE} <-> {SEND_IFACE} up")


def run_test(switch_bin):
    if os.path.exists(RECV_PCAP):
        os.remove(RECV_PCAP)
    # capture ONLY inbound (-Q in) frames on SEND_IFACE: the pkt_out reflections. The
    # tick/pkt_in we send are outbound here, so they're excluded; pkt_out is filtered
    # from any residual noise in check() by its header. ipv6 is disabled on the veths.
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SEND_IFACE, "-Q", "in", "-w", RECV_PCAP, "-B", "4096"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1)
    switch = subprocess.Popen(
        ["sudo", switch_bin, "--interface", f"{SWITCH_PORT}:{SWITCH_IFACE}"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    deadline = time.time() + 30
    while time.time() < deadline:
        line = switch.stdout.readline().decode()
        if not line:
            rc = switch.poll()
            print(f"[-] switch exited early (rc={rc}): {switch.stderr.read().decode()}")
            sys.exit(1)
        print("    switch: " + line.strip())
        if "Init complete." in line:
            time.sleep(1)
            break
    else:
        raise TimeoutError("switch did not print 'Init complete.' within 30s")

    subprocess.run(["sudo", "tcpreplay", "--pps=1000", "--intf1=" + SEND_IFACE, SEND_PCAP],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"[+] sent 1 tick + {NUM_INPUTS} pkt_in on {SEND_IFACE}")
    # give the pipeline time to serve all pkt_in despite the endless tick recirc
    time.sleep(3)
    try:
        tcpdump.terminate(); tcpdump.wait(timeout=5)
    except subprocess.TimeoutExpired:
        tcpdump.kill(); tcpdump.wait()
    switch.terminate(); switch.wait()


def check(tags):
    # expected pkt_out frames carry x = 1..N (order-independent check on the set)
    expected = set(range(1, NUM_INPUTS + 1))
    try:
        recv = [bytes(p) for p in rdpcap(RECV_PCAP)]
    except Exception:
        recv = []
    pkt_out_hdr = hdr(tags["pkt_out"])
    got = set()
    for r in recv:
        if r.startswith(pkt_out_hdr) and len(r) >= len(pkt_out_hdr) + 4:
            got.add(struct.unpack(">I", r[len(pkt_out_hdr):len(pkt_out_hdr) + 4])[0])
    print(f"[*] fed 1 endless tick + {NUM_INPUTS} pkt_in; got {len(got)} distinct pkt_out x")
    if got == expected:
        print("[+] PASS: all inputs served despite the endless recirc (no starvation)")
        return True
    missing = sorted(expected - got)
    print(f"[-] FAIL: missing pkt_out for x={missing} -- input starved by the recirc loop?")
    return False


if __name__ == "__main__":
    os.makedirs(PCAP_DIR, exist_ok=True)
    os.makedirs(BUILD_DIR, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen_c()
        print("[+] regenerated lucidprog.c -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch()
    tags = event_tags()
    build_pcap(tags)
    ensure_veths()
    run_test(switch_bin)
    sys.exit(0 if check(tags) else 1)

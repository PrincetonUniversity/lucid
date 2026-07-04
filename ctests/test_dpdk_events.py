#!/usr/bin/env python3
# Functional test for the C backend's DPDK driver on a MULTI-OUTPUT handler:
# recirculation (generate_self) + a port output (generate_port) from one event.
#
# The single-output reflector tests (test_dpdk.py / test_dpdk_afpacket.py) never
# exercise the interesting parts of the queue-based driver. events.dpt does: its
# pkt_in handler emits BOTH a recirc `bg_cmd` (generate_self) AND a port output
# `generate_port(1, pkt_out(..))`. So this checks:
#   - multi-out_event dispatch (one handler -> two out_events),
#   - the recirc queue drain (bg_cmd is queued, handled -- its body is `skip` -- and
#     drained, with no loop and no spurious output),
#   - send_port_event's framing + port routing (the pkt_out lands on port 1, framed
#     as a Lucid background event: dst_mac=1, src_mac=2, ethertype=666, tag, fields).
#
# Transport is DPDK's pcap PMD (net_pcap), like test_dpdk.py, but with TWO ports:
#   port 0 (net_pcap0): rx = the crafted pkt_in frames; tx should stay empty.
#   port 1 (net_pcap1): rx = empty; tx = where the pkt_out frames land (checked).
# generate_port(1, ..) targets port 1, so port 1 must exist -> two vdevs. The driver
# sizes the mbuf pool as 8191 * num_ports, which overflows --no-huge's default heap at
# 2 ports, so we pass -m 512.
#
# events.dpt has no explicit parser, so the auto (Lucid-framing) parser expects the
# input framed as a background event (ethertype 666 + 16-bit event tag + fields),
# NOT a raw packet -- hence we hand-build the frames rather than use scapy layers.
#
# Runs under emulation (--no-huge; DPDK built westmere). Needs root (DPDK EAL): sudo.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_dpdk_events.py --gen   # once, where lucidcc is built: .dpt -> build dir
#   sudo python3 test_dpdk_events.py         # normal loop: make + run + check

import subprocess
import sys
import os
import re
import struct
import time

from scapy.all import Ether, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "events.dpt")
BUILD_DIR = os.environ.get("DPDK_EVENTS_BUILD_DIR") or os.path.join(SCRIPT_DIR, "_dpdk_events_build")
PCAP_DIR = os.environ.get("DPDK_PCAP_DIR") or os.path.join(SCRIPT_DIR, "pcaps")
IN_PCAP = os.path.join(PCAP_DIR, "dpdk_events.in.pcap")
EMPTY_PCAP = os.path.join(PCAP_DIR, "dpdk_events.empty.pcap")
PORT0_PCAP = os.path.join(PCAP_DIR, "dpdk_events.port0.pcap")   # expected empty
PORT1_PCAP = os.path.join(PCAP_DIR, "dpdk_events.port1.pcap")   # the pkt_out frames
NUM_PACKETS = 5
RUN_TIMEOUT = 15  # backstop; we poll for completion and stop early

LUCID_ETY = 666
# Framing constants from the interpreter's deparser (InterpDeparsing.lucid_eth_fields):
# a Lucid background event is dst_mac=1 ++ src_mac=2 ++ ethertype=666 ++ tag ++ fields.
DST_MAC = bytes.fromhex("000000000001")
SRC_MAC = bytes.fromhex("000000000002")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_build_dir():
    """Regenerate the build dir from events.dpt via lucidcc --dpdk. Opt-in (`--gen`);
    the only step that invokes the Lucid compiler."""
    lucidcc = os.path.join(repo_root(), "lucidcc")
    print(f"[+] lucidcc --dpdk {os.path.basename(DPT_FILE)} --build {os.path.relpath(BUILD_DIR)}")
    subprocess.run([lucidcc, DPT_FILE, "--dpdk", "--build", BUILD_DIR], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def build_switch():
    """make the (pre-generated, committed) DPDK program. No lucidcc; needs DPDK."""
    cfile = os.path.join(BUILD_DIR, "lucidprog.c")
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run `sudo python3 {os.path.basename(__file__)} --gen` "
                 f"first (needs lucidcc) to generate it.")
    print("[+] make")
    subprocess.run(["make", "-C", BUILD_DIR], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    binfile = os.path.join(BUILD_DIR, "build", "lucidprog")
    if not os.path.exists(binfile):
        sys.exit(f"[-] {binfile} not built")
    return binfile


def event_tags():
    """Read the event -> tag mapping straight from the generated C (`<name>_tag = N;`),
    so the test tracks lucidcc's numbering instead of hardcoding it."""
    c = open(os.path.join(BUILD_DIR, "lucidprog.c")).read()
    tags = {m.group(1): int(m.group(2)) for m in re.finditer(r"(\w+)_tag\s*=\s*(\d+);", c)}
    for name in ("pkt_in", "pkt_out"):
        if name not in tags:
            sys.exit(f"[-] could not find {name}_tag in generated C")
    return tags


def frame(tag, src_ip, src_port):
    return (DST_MAC + SRC_MAC + struct.pack(">H", LUCID_ETY) + struct.pack(">H", tag)
            + struct.pack(">I", src_ip) + struct.pack(">I", src_port))


def inputs():
    # vary the fields per packet so the check confirms field passthrough, not just shape
    return [(0x0A000001 + i, 5000 + i) for i in range(NUM_PACKETS)]


def build_pcaps(tags):
    frames = [frame(tags["pkt_in"], ip, port) for ip, port in inputs()]
    wrpcap(IN_PCAP, [Ether(f) for f in frames])
    # an empty pcap (global header, 0 packets) for port 1's rx
    with open(EMPTY_PCAP, "wb") as f:
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))
    print(f"[+] wrote {NUM_PACKETS} Lucid-framed pkt_in frames to {os.path.basename(IN_PCAP)}")


def stop_switch(switch, switch_bin):
    subprocess.run(["sudo", "pkill", "-INT", "-f", switch_bin], capture_output=True)
    try:
        switch.wait(timeout=5)
    except subprocess.TimeoutExpired:
        subprocess.run(["sudo", "pkill", "-9", "-f", switch_bin], capture_output=True)
        try:
            switch.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass


def run_test(switch_bin):
    for p in (PORT0_PCAP, PORT1_PCAP):
        if os.path.exists(p):
            os.remove(p)
    vdev0 = f"net_pcap0,rx_pcap={IN_PCAP},tx_pcap={PORT0_PCAP}"
    vdev1 = f"net_pcap1,rx_pcap={EMPTY_PCAP},tx_pcap={PORT1_PCAP}"
    cmd = ["sudo", switch_bin, "--no-huge", "-m", "512", "-l", "0", "-n", "1", "--no-pci",
           "--vdev", vdev0, "--vdev", vdev1]
    print(f"[+] run: {' '.join(cmd[1:])}")
    switch = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                              start_new_session=True)
    # poll port 1 until all pkt_out frames arrive (pcap PMD flushes per TX burst),
    # then stop -- so the run is fast instead of waiting the full timeout.
    deadline = time.time() + RUN_TIMEOUT
    while time.time() < deadline:
        if switch.poll() is not None:
            break
        time.sleep(0.5)
        try:
            if len(rdpcap(PORT1_PCAP)) >= NUM_PACKETS:
                break
        except Exception:
            pass
    if switch.poll() is not None:
        out = switch.stdout.read().decode(errors="replace")
        print("[-] switch exited early:\n" + "\n".join("    " + l for l in out.strip().splitlines()[-8:]))
        sys.exit(1)
    stop_switch(switch, switch_bin)


def check(tags):
    expected = [frame(tags["pkt_out"], ip, port) for ip, port in inputs()]
    try:
        recv = [bytes(p) for p in rdpcap(PORT1_PCAP)]
    except Exception:
        recv = []
    try:
        port0 = list(rdpcap(PORT0_PCAP))
    except Exception:
        port0 = []
    print(f"[*] sent {NUM_PACKETS} pkt_in, port1 got {len(recv)} pkt_out, port0 got {len(port0)}")
    if len(port0) != 0:
        print(f"[-] FAIL: port 0 emitted {len(port0)} packet(s) (expected none)"); return False
    if len(recv) != len(expected):
        print("[-] FAIL: port1 pkt_out count differs"); return False
    for i, (r, e) in enumerate(zip(recv, expected)):
        if r != e:
            print(f"[-] FAIL: pkt_out[{i}] framing mismatch\n    got {r.hex()}\n    exp {e.hex()}")
            return False
    print("[+] PASS: recirc drained, port1 pkt_out frames correct, port0 empty"); return True


if __name__ == "__main__":
    os.makedirs(PCAP_DIR, exist_ok=True)
    os.makedirs(BUILD_DIR, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen_build_dir()
        print("[+] regenerated build dir -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch()
    tags = event_tags()
    build_pcaps(tags)
    run_test(switch_bin)
    sys.exit(0 if check(tags) else 1)

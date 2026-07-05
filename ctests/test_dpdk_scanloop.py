#!/usr/bin/env python3
# Regression test for the DPDK driver's dispatch SCHEDULING (not just plumbing).
#
# scanloop.dpt has a `tick` handler that recirculates its own event forever (a
# stand-in for a periodic table scanner), plus a `pkt_in` handler that emits one
# port output per input packet. We feed [tick, pkt_in(1..N)] and expect all N
# pkt_out frames back.
#
# This distinguishes a correct scheduler from the old "drain the queue to empty per
# input packet" model: under full-drain, the first `tick` recirculates endlessly and
# `pkt_in` is never reached -> ZERO outputs (input starved). Under one-event-per-loop
# dispatch, `tick` interleaves with the pkt_in events, so all N outputs appear.
#
# Single pcap vdev (port 0): rx = the crafted input, tx = where pkt_out lands.
# events with no explicit parser use the auto Lucid-framing parser, so we hand-build
# the frames (ethertype 666 + event tag + fields).
#
# Runs under emulation (--no-huge). Needs root (DPDK EAL): run with sudo.
#   sudo python3 test_dpdk_scanloop.py --gen   # once, where lucidcc is built
#   sudo python3 test_dpdk_scanloop.py         # make + run + check

import subprocess
import sys
import os
import re
import struct
import time

from scapy.all import Ether, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "scanloop.dpt")
BUILD_DIR = os.environ.get("DPDK_SCANLOOP_BUILD_DIR") or os.path.join(SCRIPT_DIR, "_dpdk_scanloop_build")
PCAP_DIR = os.environ.get("DPDK_PCAP_DIR") or os.path.join(SCRIPT_DIR, "pcaps")
IN_PCAP = os.path.join(PCAP_DIR, "dpdk_scanloop.in.pcap")
OUT_PCAP = os.path.join(PCAP_DIR, "dpdk_scanloop.out.pcap")
NUM_INPUTS = 10          # pkt_in packets fed after the tick
RUN_TIMEOUT = 15

LUCID_ETY = 666
DST_MAC = bytes.fromhex("000000000001")
SRC_MAC = bytes.fromhex("000000000002")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_build_dir():
    lucidcc = os.path.join(repo_root(), "lucidcc")
    print(f"[+] lucidcc --dpdk {os.path.basename(DPT_FILE)} --build {os.path.relpath(BUILD_DIR)}")
    subprocess.run([lucidcc, DPT_FILE, "--dpdk", "--build", BUILD_DIR], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def build_switch():
    cfile = os.path.join(BUILD_DIR, "lucidprog.c")
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run `sudo python3 {os.path.basename(__file__)} --gen` first.")
    print("[+] make")
    subprocess.run(["make", "-C", BUILD_DIR], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    binfile = os.path.join(BUILD_DIR, "build", "lucidprog")
    if not os.path.exists(binfile):
        sys.exit(f"[-] {binfile} not built")
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
    wrpcap(IN_PCAP, [Ether(f) for f in frames])
    print(f"[+] wrote 1 tick + {NUM_INPUTS} pkt_in frames to {os.path.basename(IN_PCAP)}")


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
    if os.path.exists(OUT_PCAP):
        os.remove(OUT_PCAP)
    vdev = f"net_pcap0,rx_pcap={IN_PCAP},tx_pcap={OUT_PCAP}"
    cmd = ["sudo", switch_bin, "--no-huge", "-l", "0", "-n", "1", "--no-pci", "--vdev", vdev]
    print(f"[+] run: {' '.join(cmd[1:])}")
    switch = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                              start_new_session=True)
    # poll until all N outputs arrive (proves pkt_in wasn't starved by the tick loop)
    deadline = time.time() + RUN_TIMEOUT
    while time.time() < deadline:
        if switch.poll() is not None:
            break
        time.sleep(0.5)
        try:
            if len(rdpcap(OUT_PCAP)) >= NUM_INPUTS:
                break
        except Exception:
            pass
    if switch.poll() is not None:
        out = switch.stdout.read().decode(errors="replace")
        print("[-] switch exited early:\n" + "\n".join("    " + l for l in out.strip().splitlines()[-8:]))
        sys.exit(1)
    stop_switch(switch, switch_bin)


def check(tags):
    # expected pkt_out frames carry x = 1..N (order-independent check on the set)
    expected = {i for i in range(1, NUM_INPUTS + 1)}
    try:
        recv = [bytes(p) for p in rdpcap(OUT_PCAP)]
    except Exception:
        recv = []
    got = set()
    pkt_out_hdr = hdr(tags["pkt_out"])
    for r in recv:
        if r.startswith(pkt_out_hdr) and len(r) >= len(pkt_out_hdr) + 4:
            got.add(struct.unpack(">I", r[len(pkt_out_hdr):len(pkt_out_hdr) + 4])[0])
    print(f"[*] fed 1 endless tick + {NUM_INPUTS} pkt_in; got {len(recv)} pkt_out ({len(got)} distinct x)")
    if got == expected:
        print("[+] PASS: all inputs served despite the endless recirc (no starvation)"); return True
    missing = sorted(expected - got)
    print(f"[-] FAIL: missing pkt_out for x={missing} -- input starved by the recirc loop?")
    return False


if __name__ == "__main__":
    os.makedirs(PCAP_DIR, exist_ok=True)
    os.makedirs(BUILD_DIR, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen_build_dir()
        print("[+] regenerated build dir -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch()
    tags = event_tags()
    build_pcap(tags)
    run_test(switch_bin)
    sys.exit(0 if check(tags) else 1)

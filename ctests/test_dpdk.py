#!/usr/bin/env python3
# Functional test for the C backend's DPDK driver (lucidcc --dpdk).
#
# Sibling of test_rawsock.py, but for the DPDK driver. Instead of raw sockets on
# real interfaces, it runs the compiled switch under DPDK's *pcap PMD* (the
# net_pcap virtual device): DPDK reads the input frames straight from a pcap file
# and writes transmitted frames to another pcap file -- no veths, tcpdump, or
# tcpreplay needed. Performance is irrelevant here; this is a correctness/dev test.
#
# The switch binds DPDK port 0 to the net_pcap vdev; the reflector bounces each
# frame back out its ingress port (port 0), so reflected frames land in tx_pcap.
#
# Runs under emulation (x86-on-ARM): `--no-huge` avoids hugepage setup, and the
# in-container DPDK is built with a westmere CPU baseline (no AVX). Needs root
# (DPDK EAL): run with sudo.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_dpdk.py --gen   # once, where lucidcc is built: .dpt -> _dpdk_build/
#                                     # (commit the generated build dir)
#   sudo python3 test_dpdk.py         # normal loop: make + run + check

import subprocess
import sys
import os
import time

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "ethswaprefl.dpt")  # swaps src/dst MAC, reflects
BUILD_DIR = os.path.join(SCRIPT_DIR, "_dpdk_build")
SEND_PCAP = os.path.join(SCRIPT_DIR, "pcaps", "dpdk.send.pcap")
RECV_PCAP = os.path.join(SCRIPT_DIR, "pcaps", "dpdk.recv.pcap")
NUM_PACKETS = 100
RUN_TIMEOUT = 15  # seconds to let the switch drain the input pcap before we stop it


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_build_dir():
    """Regenerate _dpdk_build/ from DPT_FILE via lucidcc --dpdk. The ONLY step that
    invokes the Lucid compiler -- opt-in (`--gen`) so the normal test loop is just
    make + run against the committed lucidprog.c. Re-run with --gen (where lucidcc
    is built) whenever the compiler or the .dpt changes, then commit _dpdk_build."""
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


def build_pcap(path, n, pload_size=32):
    pkts = [Ether(dst="ff:ff:ff:ff:ff:ff", src="00:11:22:33:44:55") /
            IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
            for _ in range(n)]
    wrpcap(path, pkts)
    print(f"[+] wrote {n} packets to {os.path.basename(path)}")


def run_test(switch_bin):
    if os.path.exists(RECV_PCAP):
        os.remove(RECV_PCAP)
    # DPDK pcap PMD: port 0 reads frames from SEND_PCAP, writes TX frames to RECV_PCAP.
    # --no-huge: use normal memory (works under emulation / without hugepage setup).
    vdev = f"net_pcap0,rx_pcap={SEND_PCAP},tx_pcap={RECV_PCAP}"
    cmd = ["sudo", switch_bin, "--no-huge", "-l", "0", "-n", "1", "--no-pci", "--vdev", vdev]
    print(f"[+] run: {' '.join(cmd[1:])}")
    # The generated main() loops forever (rx_burst returns 0 after the pcap is
    # drained); there is no self-exit, so run it briefly then stop it. The pcap PMD
    # writes each TX burst out, so RECV_PCAP holds the reflected frames.
    switch = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    # Poll RECV_PCAP until all frames are reflected (the pcap PMD flushes per TX
    # burst), then stop -- so the run is fast instead of always waiting the full
    # timeout. RUN_TIMEOUT is the backstop if something stalls.
    deadline = time.time() + RUN_TIMEOUT
    while time.time() < deadline:
        if switch.poll() is not None:
            break  # exited on its own (e.g. EAL error)
        time.sleep(0.5)
        try:
            if len(rdpcap(RECV_PCAP)) >= NUM_PACKETS:
                break
        except Exception:
            pass  # file not ready / mid-write
    if switch.poll() is None:
        # sudo'd child: signal via sudo kill (we can't signal a root process directly)
        subprocess.run(["sudo", "kill", "-INT", str(switch.pid)], capture_output=True)
        try:
            switch.wait(timeout=5)
        except subprocess.TimeoutExpired:
            subprocess.run(["sudo", "kill", "-9", str(switch.pid)], capture_output=True)
            switch.wait()
    out = switch.stdout.read().decode(errors="replace")
    tail = "\n".join("    switch: " + l for l in out.strip().splitlines()[-4:])
    if tail:
        print(tail)


def check(send_pcap, recv_pcap):
    sent = rdpcap(send_pcap)
    try:
        recv = rdpcap(recv_pcap)
    except Exception:
        recv = []
    print(f"[*] sent {len(sent)}, received {len(recv)}")
    if len(recv) != len(sent):
        print("[-] FAIL: packet counts differ"); return False
    # the reflector swaps src/dst MAC; every received packet must be a swapped
    # copy of what we sent.
    s = sent[0]
    for i, r in enumerate(recv):
        if not (r[Ether].dst == s[Ether].src and r[Ether].src == s[Ether].dst):
            print(f"[-] FAIL: MACs not swapped on recv[{i}] "
                  f"(sent {s[Ether].src}->{s[Ether].dst}, recv {r[Ether].src}->{r[Ether].dst})")
            return False
    print("[+] PASS: counts match and MACs are swapped"); return True


if __name__ == "__main__":
    os.makedirs(os.path.join(SCRIPT_DIR, "pcaps"), exist_ok=True)
    if "--gen" in sys.argv[1:]:
        # (Re)generate _dpdk_build/ from the .dpt via lucidcc, then stop. Run this
        # where lucidcc is built; commit the result so plain runs skip the compiler.
        gen_build_dir()
        print("[+] regenerated _dpdk_build -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch()
    build_pcap(SEND_PCAP, NUM_PACKETS)
    run_test(switch_bin)
    sys.exit(0 if check(SEND_PCAP, RECV_PCAP) else 1)

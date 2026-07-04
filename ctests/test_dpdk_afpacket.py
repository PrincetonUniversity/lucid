#!/usr/bin/env python3
# Functional test for the C backend's DPDK driver over the AF_PACKET vdev.
#
# Sibling of test_dpdk.py, but instead of DPDK's pcap PMD (file in / file out) it
# runs the compiled switch on a *real interface* via DPDK's net_af_packet vdev --
# the same veth topology as test_rawsock.py. This exercises the DPDK driver's live
# rx/tx path (an AF_PACKET socket bound to an interface) rather than the offline
# pcap path, so it's the DPDK analogue of the raw-socket on-wire test.
#
# It reuses test_dpdk.py's build dir (_dpdk_build): same program, same DPDK driver,
# only the vdev (and thus the EAL --vdev arg) differs. Kept a separate script for
# now; we can consolidate with test_dpdk.py later if it's worth it.
#
# Topology: veth0 <-> veth1. The switch binds DPDK port 0 to veth0 (via
#   --vdev net_af_packet0,iface=veth0); we send packets on veth1 and the reflector
# bounces them back, captured inbound on veth1 (tcpdump -Q in, filtered to our UDP
# flow so link noise doesn't pollute the capture).
#
# Runs under emulation (--no-huge; DPDK built with a westmere baseline). Needs root
# (DPDK EAL, AF_PACKET, veth creation, tcpdump/tcpreplay): run with sudo.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_dpdk_afpacket.py --gen   # once, where lucidcc is built: .dpt -> _dpdk_build/
#   sudo python3 test_dpdk_afpacket.py         # normal loop: make + run + check

import subprocess
import sys
import os
import time

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "ethswaprefl.dpt")  # swaps src/dst MAC, reflects
# Shares test_dpdk.py's build dir (same program + DPDK driver). Overridable so a
# suite run can point it at a temp dir.
BUILD_DIR = os.environ.get("DPDK_BUILD_DIR") or os.path.join(SCRIPT_DIR, "_dpdk_build")
PCAP_DIR = os.environ.get("DPDK_PCAP_DIR") or os.path.join(SCRIPT_DIR, "pcaps")
SEND_PCAP = os.path.join(PCAP_DIR, "afpacket.send.pcap")
RECV_PCAP = os.path.join(PCAP_DIR, "afpacket.recv.pcap")
SWITCH_IFACE = "veth0"
SEND_IFACE = "veth1"
NUM_PACKETS = 100
READY_SLEEP = 5  # seconds to let EAL init + the af_packet port come up before sending


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_build_dir():
    """Regenerate _dpdk_build/ from DPT_FILE via lucidcc --dpdk. The ONLY step that
    invokes the Lucid compiler -- opt-in (`--gen`). Re-run where lucidcc is built
    whenever the compiler or .dpt changes, then commit _dpdk_build."""
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


def build_pcap(path, n, pload_size=64):
    pkts = [Ether(dst="ff:ff:ff:ff:ff:ff", src="00:11:22:33:44:55") /
            IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
            for _ in range(n)]
    wrpcap(path, pkts)
    print(f"[+] wrote {n} packets to {os.path.basename(path)}")


def ensure_veths():
    subprocess.run(["sudo", "ip", "link", "add", SWITCH_IFACE, "type", "veth", "peer", "name", SEND_IFACE],
                   capture_output=True)
    # Disable IPv6 (best-effort; /proc/sys may be read-only in the container) to cut
    # neighbor-discovery / MLD noise. The tcpdump `udp port 5000` filter is the real
    # guard regardless.
    for iface in (SWITCH_IFACE, SEND_IFACE):
        subprocess.run(["sudo", "sysctl", "-w", f"net.ipv6.conf.{iface}.disable_ipv6=1"], capture_output=True)
    subprocess.run(["sudo", "ip", "link", "set", SWITCH_IFACE, "up"], check=True)
    subprocess.run(["sudo", "ip", "link", "set", SEND_IFACE, "up"], check=True)
    print(f"[+] {SWITCH_IFACE} <-> {SEND_IFACE} up")


def stop_switch(switch, switch_bin):
    # The switch runs as root under sudo (which may re-parent it into its own
    # session), so signal it reliably by its unique binary path via pkill, then reap
    # our sudo Popen handle (sudo exits once its child dies).
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
    # Capture ONLY inbound (-Q in) reflected packets, filtered to our UDP flow so any
    # link noise (IPv6 ND, ARP) is excluded and doesn't consume the -c cap.
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SEND_IFACE, "-Q", "in", "-w", RECV_PCAP, "-c", str(NUM_PACKETS),
         "-B", "4096", "udp", "port", "5000"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1)

    # DPDK prints its readiness line once then poll-loops silently, and its stdout is
    # block-buffered through the sudo layers -- unreliable to wait on. So just give
    # EAL + the af_packet port a fixed moment to come up. Output -> a log file (not a
    # pipe we block-read) for diagnostics on early exit.
    vdev = f"net_af_packet0,iface={SWITCH_IFACE}"
    logf = open(os.path.join(BUILD_DIR, "afpacket.switch.log"), "w")
    switch = subprocess.Popen(
        ["sudo", switch_bin, "--no-huge", "-l", "0", "-n", "1", "--no-pci", "--vdev", vdev],
        stdout=logf, stderr=subprocess.STDOUT, start_new_session=True)
    time.sleep(READY_SLEEP)
    if switch.poll() is not None:
        logf.close()
        with open(logf.name) as f:
            print("[-] switch exited early:\n" + "".join("    " + l for l in f.readlines()[-8:]))
        tcpdump.terminate(); tcpdump.wait()
        sys.exit(1)
    print(f"[+] switch up (af_packet on {SWITCH_IFACE})")

    subprocess.run(["sudo", "tcpreplay", "--pps=1000", "--intf1=" + SEND_IFACE, SEND_PCAP],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"[+] sent {NUM_PACKETS} packets on {SEND_IFACE}")
    try:
        tcpdump.wait(timeout=15)
    except subprocess.TimeoutExpired:
        tcpdump.terminate(); tcpdump.wait()
    stop_switch(switch, switch_bin)
    logf.close()


def check(send_pcap, recv_pcap):
    sent = rdpcap(send_pcap)
    try:
        recv = rdpcap(recv_pcap)
    except Exception:
        recv = []
    print(f"[*] sent {len(sent)}, received {len(recv)}")
    if len(recv) != len(sent):
        print("[-] FAIL: packet counts differ"); return False
    # the reflector swaps src/dst MAC; every received packet must be a swapped copy.
    s = sent[0]
    for i, r in enumerate(recv):
        if not (r[Ether].dst == s[Ether].src and r[Ether].src == s[Ether].dst):
            print(f"[-] FAIL: MACs not swapped on recv[{i}] "
                  f"(sent {s[Ether].src}->{s[Ether].dst}, recv {r[Ether].src}->{r[Ether].dst})")
            return False
    print("[+] PASS: counts match and MACs are swapped"); return True


if __name__ == "__main__":
    os.makedirs(PCAP_DIR, exist_ok=True)
    os.makedirs(BUILD_DIR, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen_build_dir()
        print("[+] regenerated _dpdk_build -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch()
    build_pcap(SEND_PCAP, NUM_PACKETS)
    ensure_veths()
    run_test(switch_bin)
    sys.exit(0 if check(SEND_PCAP, RECV_PCAP) else 1)

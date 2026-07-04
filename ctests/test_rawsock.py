#!/usr/bin/env python3
# Functional test for the C backend's raw-socket driver (lucidcc --rawsock).
#
# Adapted from examples/features/lucidvswitch/test_reflector.py, but instead of running
# the lucidSwitch interpreter it runs a standalone raw-socket binary built from a Lucid
# program (lucidcc --rawsock -> gcc). The compiled binary takes the same
# `--interface PORT:IFNAME` args as lucidSwitch.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_rawsock.py --gen   # once, where lucidcc is built: .dpt -> lucidprog.c
#                                        # (commit the generated lucidprog.c)
#   sudo python3 test_rawsock.py         # normal loop: gcc lucidprog.c + run + check
#
# Topology: a veth/feth pair (SWITCH_IFACE <-> SEND_IFACE). The switch binds port 0 to
# SWITCH_IFACE; we send packets on SEND_IFACE and the reflector bounces them back, where
# we capture them on SEND_IFACE.
#
# IMPORTANT (fixes a bug in the original script): tcpdump is run with `-Q in` so it
# captures ONLY inbound packets on SEND_IFACE. Without it, tcpdump also captures the
# OUTBOUND packets tcpreplay is sending, double-counting and corrupting the results.
#
# Requires root (raw sockets / BPF / tcpdump / veth creation): run with sudo.

import subprocess
import sys
import os
import time
import platform

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DPT_FILE = os.path.join(SCRIPT_DIR, "programs", "ethswaprefl.dpt")  # swaps src/dst MAC, reflects
SEND_PCAP = os.path.join(SCRIPT_DIR, "pcaps", "rawsock.send.pcap")
RECV_PCAP = os.path.join(SCRIPT_DIR, "pcaps", "rawsock.recv.pcap")
SWITCH_IFACE = "feth0"
SEND_IFACE = "feth1"
NUM_PACKETS = 100
SWITCH_PORT = 0

IS_LINUX = platform.system() == "Linux"
if IS_LINUX:
    SWITCH_IFACE, SEND_IFACE = "veth0", "veth1"


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def gen_c(workdir):
    """Regenerate the raw-socket C from DPT_FILE via lucidcc --rawsock.

    This is the ONLY step that invokes the Lucid compiler. It is opt-in
    (`--gen`) so the normal in-container test loop is just gcc + run against
    the committed lucidprog.c -- no Lucid rebuild needed when iterating on the
    driver / harness. Re-run with --gen (where lucidcc is built) whenever the
    compiler or the .dpt changes, then commit the regenerated lucidprog.c."""
    lucidcc = os.path.join(repo_root(), "lucidcc")
    cfile = os.path.join(workdir, "lucidprog.c")
    print(f"[+] lucidcc --rawsock {os.path.basename(DPT_FILE)} -> {os.path.basename(cfile)}")
    subprocess.run([lucidcc, DPT_FILE, "-o", cfile, "--rawsock"], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def build_switch(workdir):
    """gcc the (pre-generated, committed) lucidprog.c into a binary. No lucidcc."""
    cfile = os.path.join(workdir, "lucidprog.c")
    binfile = os.path.join(workdir, "lucidprog")
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run `sudo python3 {os.path.basename(__file__)} --gen` "
                 f"first (needs lucidcc) to generate it.")
    print("[+] gcc")
    subprocess.run(["gcc", "-O2", "-o", binfile, cfile], check=True)
    return binfile


def build_pcap(path, n, pload_size=64):
    pkts = [Ether(dst="ff:ff:ff:ff:ff:ff", src="00:11:22:33:44:55") /
            IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
            for _ in range(n)]
    wrpcap(path, pkts)
    print(f"[+] wrote {n} packets to {os.path.basename(path)}")


def ensure_veths():
    if IS_LINUX:
        subprocess.run(["sudo", "ip", "link", "add", SWITCH_IFACE, "type", "veth", "peer", "name", SEND_IFACE], capture_output=True)
        # Disable IPv6 on both ends BEFORE bringing them up: a fresh veth otherwise
        # floods the link with neighbor-discovery / MLD / router-solicitation frames
        # (the 33:33:* multicast noise) that pollute the reflection capture.
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
    # Capture ONLY inbound (-Q in) reflected packets: filter to the UDP flow we
    # sent (`udp port 5000`) so any residual link noise (IPv6 ND, ARP, etc.) is
    # excluded and doesn't consume the -c packet cap or land at recv[0].
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SEND_IFACE, "-Q", "in", "-w", RECV_PCAP, "-c", str(NUM_PACKETS),
         "-B", "4096", "udp", "port", "5000"],
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
    print(f"[+] sent {NUM_PACKETS} packets on {SEND_IFACE}")
    try:
        tcpdump.wait(timeout=15)
    except subprocess.TimeoutExpired:
        tcpdump.terminate(); tcpdump.wait()
    switch.terminate(); switch.wait()


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
    work = os.path.join(SCRIPT_DIR, "_rawsock_build")
    os.makedirs(work, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        # (Re)generate lucidprog.c from the .dpt via lucidcc, then stop. Run this
        # where lucidcc is built; commit the result so plain runs skip the compiler.
        gen_c(work)
        print("[+] regenerated lucidprog.c -- commit it, then run without --gen")
        sys.exit(0)
    switch_bin = build_switch(work)
    build_pcap(SEND_PCAP, NUM_PACKETS)
    ensure_veths()
    run_test(switch_bin)
    sys.exit(0 if check(SEND_PCAP, RECV_PCAP) else 1)

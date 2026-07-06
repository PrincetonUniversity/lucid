#!/usr/bin/env python3
# Shared helpers for the C-backend driver tests (test_rawsock.py, test_dpdk.py).
#
# The driver tests are the same handful of shapes over different transports:
#   - reflector: send N UDP packets, check the count + MAC swap on the way back;
#   - scanloop:  feed one endless `tick` + N `pkt_in`, check all N `pkt_out` come
#                back (the driver interleaved recirc with fresh input -- no starvation);
#   - events:    feed N `pkt_in` on an ingress port, check the `pkt_out` frames on the
#                egress port (recirc drained, framing + port routing correct).
# This module factors out what they share: codegen (--gen) + build, Lucid frame
# building, the reflector traffic + check, and the veth / subprocess plumbing. Each
# test_<driver>.py supplies only its transport-specific run flow.

import os
import re
import struct
import subprocess
import sys
import time

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROGRAMS = os.path.join(SCRIPT_DIR, "programs")
PCAPS = os.path.join(SCRIPT_DIR, "pcaps")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()


def lucidcc():
    return os.path.join(repo_root(), "lucidcc")


# ---- codegen (--gen) + build --------------------------------------------------
# The tests are two-phase: `--gen` runs lucidcc (only where it's built) and commits
# the generated C / build dir; a plain run just gcc/make's that committed source, so
# the in-container loop never rebuilds Lucid.

def gen_single(dpt, driver_flag, cfile):
    """lucidcc <dpt> <driver_flag> -o cfile  (single-file drivers: --rawsock)."""
    os.makedirs(os.path.dirname(cfile), exist_ok=True)
    print(f"[+] lucidcc {driver_flag} {os.path.basename(dpt)} -> {os.path.relpath(cfile)}")
    subprocess.run([lucidcc(), dpt, "-o", cfile, driver_flag], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def gen_dpdk(dpt, build_dir):
    """lucidcc <dpt> --dpdk --build <build_dir>  (dpdk emits a whole build dir)."""
    os.makedirs(build_dir, exist_ok=True)
    print(f"[+] lucidcc --dpdk {os.path.basename(dpt)} --build {os.path.relpath(build_dir)}")
    subprocess.run([lucidcc(), dpt, "--dpdk", "--build", build_dir], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def build_gcc(cfile, binfile, libs=()):
    """gcc the (committed) single-file C into a binary. No lucidcc."""
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run with --gen first (needs lucidcc).")
    print("[+] gcc")
    subprocess.run(["gcc", "-O2", "-o", binfile, cfile, *libs], check=True)
    return binfile


def build_make(build_dir):
    """make the (committed) DPDK build dir. No lucidcc; needs DPDK."""
    cfile = os.path.join(build_dir, "lucidprog.c")
    if not os.path.exists(cfile):
        sys.exit(f"[-] {cfile} not found -- run with --gen first (needs lucidcc).")
    print("[+] make")
    subprocess.run(["make", "-C", build_dir], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    binfile = os.path.join(build_dir, "build", "lucidprog")
    if not os.path.exists(binfile):
        sys.exit(f"[-] {binfile} not built")
    return binfile


# ---- Lucid frame building -----------------------------------------------------
# A Lucid background event is framed (InterpDeparsing.lucid_eth_fields):
#   dst_mac=1 ++ src_mac=2 ++ ethertype=666 ++ 16-bit tag ++ fields (32-bit each).
LUCID_ETY = 666
DST_MAC = bytes.fromhex("000000000001")
SRC_MAC = bytes.fromhex("000000000002")


def lucid_frame(tag, *fields):
    return (DST_MAC + SRC_MAC + struct.pack(">H", LUCID_ETY) + struct.pack(">H", tag)
            + b"".join(struct.pack(">I", f) for f in fields))


def event_tags(build_dir_or_cfile, *required):
    """Read the event -> tag mapping straight from the generated C (`<name>_tag = N;`),
    so tests track lucidcc's numbering instead of hardcoding it."""
    cfile = build_dir_or_cfile
    if os.path.isdir(cfile):
        cfile = os.path.join(cfile, "lucidprog.c")
    c = open(cfile).read()
    tags = {m.group(1): int(m.group(2)) for m in re.finditer(r"(\w+)_tag\s*=\s*(\d+);", c)}
    for name in required:
        if name not in tags:
            sys.exit(f"[-] could not find {name}_tag in generated C ({cfile})")
    return tags


# ---- reflector traffic + check ------------------------------------------------

def reflector_pcap(path, n, pload_size=64):
    """N identical UDP packets (raw ethernet payload), the reflector's input."""
    pkts = [Ether(dst="ff:ff:ff:ff:ff:ff", src="00:11:22:33:44:55") /
            IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
            for _ in range(n)]
    wrpcap(path, pkts)
    print(f"[+] wrote {n} packets to {os.path.basename(path)}")


def reflector_check(send_pcap, recv_pcap):
    """The reflector swaps src/dst MAC: every received packet must be a swapped copy,
    and counts must match."""
    sent = rdpcap(send_pcap)
    try:
        recv = rdpcap(recv_pcap)
    except Exception:
        recv = []
    print(f"[*] sent {len(sent)}, received {len(recv)}")
    if len(recv) != len(sent):
        print("[-] FAIL: packet counts differ"); return False
    s = sent[0]
    for i, r in enumerate(recv):
        if not (r[Ether].dst == s[Ether].src and r[Ether].src == s[Ether].dst):
            print(f"[-] FAIL: MACs not swapped on recv[{i}] "
                  f"(sent {s[Ether].src}->{s[Ether].dst}, recv {r[Ether].src}->{r[Ether].dst})")
            return False
    print("[+] PASS: counts match and MACs are swapped"); return True


def empty_pcap(path):
    """A valid pcap file with a global header and zero packets (e.g. for an rx port
    that should receive nothing)."""
    with open(path, "wb") as f:
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))


def write_frames(path, frames):
    """Write raw frame bytes as an ethernet pcap."""
    wrpcap(path, [Ether(f) for f in frames])


# ---- scanloop (dispatch scheduling / no-starvation) ---------------------------
# scanloop.dpt: an endless self-recirculating `tick` plus a `pkt_in` handler that emits
# one `pkt_out` per input. Feed [tick, pkt_in(1..N)] and expect all N pkt_out back --
# proving the driver interleaves recirc with fresh input instead of looping on `tick`.

def scanloop_frames(tags, n):
    """[one endless tick] ++ [pkt_in(1), .. pkt_in(n)] as raw Lucid frames."""
    return [lucid_frame(tags["tick"])] + [lucid_frame(tags["pkt_in"], i) for i in range(1, n + 1)]


def scanloop_check(recv_pcap, pkt_out_tag, n):
    """Every pkt_out(x) for x in 1..N must have come back (order-independent)."""
    try:
        recv = [bytes(p) for p in rdpcap(recv_pcap)]
    except Exception:
        recv = []
    hdr = lucid_frame(pkt_out_tag)
    got = {struct.unpack(">I", r[len(hdr):len(hdr) + 4])[0]
           for r in recv if r.startswith(hdr) and len(r) >= len(hdr) + 4}
    print(f"[*] fed 1 endless tick + {n} pkt_in; got {len(got)} distinct pkt_out x")
    if got == set(range(1, n + 1)):
        print("[+] PASS: all inputs served despite the endless recirc (no starvation)"); return True
    print(f"[-] FAIL: missing pkt_out for x={sorted(set(range(1, n + 1)) - got)} "
          f"-- input starved by the recirc loop?")
    return False


# ---- veth / subprocess plumbing (Linux, root) ---------------------------------

def ensure_veths(a, b):
    """(Re)create + bring up a veth pair (a <-> b), IPv6 disabled to cut ND/MLD noise.
    Deletes any existing pair first so sub-tests sharing the pair start clean."""
    subprocess.run(["sudo", "ip", "link", "del", a], capture_output=True)
    subprocess.run(["sudo", "ip", "link", "add", a, "type", "veth", "peer", "name", b],
                   capture_output=True)
    for iface in (a, b):
        subprocess.run(["sudo", "sysctl", "-w", f"net.ipv6.conf.{iface}.disable_ipv6=1"],
                       capture_output=True)
    subprocess.run(["sudo", "ip", "link", "set", a, "up"], check=True)
    subprocess.run(["sudo", "ip", "link", "set", b, "up"], check=True)
    print(f"[+] {a} <-> {b} up")


def start_capture(iface, out_pcap, count=None, bpf="udp port 5000"):
    """tcpdump the INBOUND (-Q in) traffic on iface to out_pcap, filtered to bpf so link
    noise (IPv6 ND, ARP) is excluded. Returns the Popen. count -> -c (stop after N)."""
    if os.path.exists(out_pcap):
        os.remove(out_pcap)
    cmd = ["sudo", "tcpdump", "-i", iface, "-Q", "in", "-w", out_pcap, "-B", "4096"]
    if count is not None:
        cmd += ["-c", str(count)]
    cmd += bpf.split()
    p = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1)
    return p


def replay(iface, send_pcap, pps=1000):
    subprocess.run(["sudo", "tcpreplay", f"--pps={pps}", "--intf1=" + iface, send_pcap],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"[+] replayed {os.path.basename(send_pcap)} on {iface}")


def stop_switch(switch, switch_bin):
    """The switch runs as root under sudo (re-parented into its own session), so signal
    it by its unique binary path via pkill, then reap our Popen handle."""
    subprocess.run(["sudo", "pkill", "-INT", "-f", switch_bin], capture_output=True)
    try:
        switch.wait(timeout=5)
    except subprocess.TimeoutExpired:
        subprocess.run(["sudo", "pkill", "-9", "-f", switch_bin], capture_output=True)
        try:
            switch.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass


def wait_for_line(switch, needle, timeout=30):
    """Read switch stdout (echoing it) until `needle` appears; fail if it exits or times
    out first. Used for drivers that print a readiness line (e.g. rawsock's 'Init complete')."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        line = switch.stdout.readline().decode(errors="replace")
        if not line:
            rc = switch.poll()
            err = switch.stderr.read().decode(errors="replace") if switch.stderr else ""
            print(f"[-] switch exited early (rc={rc}): {err}")
            sys.exit(1)
        print("    switch: " + line.strip())
        if needle in line:
            return
    raise TimeoutError(f"switch did not print {needle!r} within {timeout}s")


def poll_until(pcap_path, n, switch, timeout=15):
    """Poll pcap_path until it holds >= n packets or the switch exits or timeout. Returns
    True if the switch is still running (drained N in time), False if it exited early.
    (Offline pcap-PMD transports flush per TX burst, so this ends the run promptly.)"""
    deadline = time.time() + timeout
    while time.time() < deadline:
        if switch.poll() is not None:
            return False
        time.sleep(0.5)
        try:
            if len(rdpcap(pcap_path)) >= n:
                break
        except Exception:
            pass
    return switch.poll() is None

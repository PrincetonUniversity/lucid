#!/usr/bin/env python3
# Shared helpers for the C-backend tests

import os
import platform
import re
import struct
import subprocess
import sys
import time

IS_LINUX = platform.system() == "Linux"

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROGRAMS = os.path.join(SCRIPT_DIR, "programs")
PCAPS = os.path.join(SCRIPT_DIR, "_pcaps")


def repo_root():
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()

def lucidcc():
    return os.path.join(repo_root(), "lucidcc")

def tail(text, n):
    return "\n".join(text.rstrip().splitlines()[-n:])


# ---- build, compile, report ----------------------------------------------------

def build_lucid():
    print("building lucidcc (make)...")
    r = subprocess.run(["make"], cwd=repo_root(), capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit("make failed:\n" + tail(r.stdout + r.stderr, 20))


def local_test_args(doc):
    import argparse
    return argparse.ArgumentParser(description=doc, formatter_class=argparse.RawDescriptionHelpFormatter)


def run_lucidcc(dpt, *args):
    return subprocess.run([lucidcc(), dpt, *args],
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)


def run_gcc(cfile, binfile, libs=()):
    return subprocess.run(["gcc", "-O2", "-o", binfile, cfile, *libs],
                          stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)


def gcc_errors(out, n=3):
    return "\n".join([l for l in out.splitlines() if "error:" in l][:n])


def report(results):
    """results: iterable of (name, ok, detail); returns an exit code"""
    passed = failed = 0
    for name, ok, detail in results:
        lines = detail.splitlines() if detail else []
        head = f"  ({lines[0]})" if lines else ""
        print(f"{'PASS' if ok else 'FAIL'}  {name}{head}")
        for l in lines[1:]:
            print("      | " + l)
        passed, failed = passed + ok, failed + (not ok)
    print("-----")
    print(f"{passed} passed, {failed} failed")
    return 1 if failed or not passed else 0


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
    from scapy.all import Ether, IP, UDP, Raw, wrpcap
    pkts = [Ether(dst="ff:ff:ff:ff:ff:ff", src="00:11:22:33:44:55") /
            IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
            for _ in range(n)]
    wrpcap(path, pkts)
    print(f"[+] wrote {n} packets to {os.path.basename(path)}")


def reflector_check(send_pcap, recv_pcap):
    """The reflector swaps src/dst MAC: every received packet must be a swapped copy,
    and counts must match."""
    from scapy.all import Ether, rdpcap
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


def write_pcap(path, frames):
    """Ethernet pcap with zero timestamps (what the pcap driver writes)."""
    with open(path, "wb") as f:
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))
        for p in frames:
            f.write(struct.pack("<IIII", 0, 0, len(p), len(p)))
            f.write(p)


def read_pcap(path):
    """Frames of a little-endian, microsecond pcap (the format write_pcap and the drivers use)."""
    data = open(path, "rb").read()
    frames, off = [], 24
    while off + 16 <= len(data):
        incl = struct.unpack("<I", data[off + 8:off + 12])[0]
        frames.append(data[off + 16:off + 16 + incl])
        off += 16 + incl
    return frames


def write_frames(path, frames):
    """Write raw frame bytes as an ethernet pcap."""
    from scapy.all import Ether, wrpcap
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
    from scapy.all import rdpcap
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
    missing = sorted(set(range(1, n + 1)) - got)
    print(f"[-] FAIL: missing pkt_out for {len(missing)} x, first {missing[:10]} "
          f"-- input starved by the recirc loop?")
    return False


# ---- veth / subprocess plumbing (Linux + macOS, root) --------------------------

def sudo(cmd):
    """Prefix cmd with sudo if not already root. Redundant sudos garble output."""
    return cmd if os.geteuid() == 0 else ["sudo"] + cmd


def ensure_veths(a, b):
    """(Re)create + bring up a veth(linux)/feth(macos) pair (a <-> b)."""
    if IS_LINUX:
        subprocess.run(sudo(["ip", "link", "del", a]), capture_output=True)
        subprocess.run(sudo(["ip", "link", "add", a, "type", "veth", "peer", "name", b]),
                       capture_output=True)
        for iface in (a, b):
            subprocess.run(sudo(["sysctl", "-w", f"net.ipv6.conf.{iface}.disable_ipv6=1"]),
                           capture_output=True)
        subprocess.run(sudo(["ip", "link", "set", a, "up"]), check=True)
        subprocess.run(sudo(["ip", "link", "set", b, "up"]), check=True)
    else:
        subprocess.run(sudo(["ifconfig", a, "destroy"]), capture_output=True)
        subprocess.run(sudo(["ifconfig", b, "destroy"]), capture_output=True)
        subprocess.run(sudo(["ifconfig", a, "create"]), capture_output=True)
        subprocess.run(sudo(["ifconfig", b, "create"]), capture_output=True)
        subprocess.run(sudo(["ifconfig", a, "peer", b]), capture_output=True)
        subprocess.run(sudo(["ifconfig", a, "up"]), check=True)
        subprocess.run(sudo(["ifconfig", b, "up"]), check=True)
    print(f"[+] {a} <-> {b} up")


def start_capture(iface, out_pcap, capture_filter, count=None):
    """tcpdump inbound traffic on iface matching capture_filter (tcpdump syntax) to
    out_pcap, stopping after `count` packets if given. Returns the Popen."""
    if os.path.exists(out_pcap):
        os.remove(out_pcap)
    cmd = sudo(["tcpdump", "-i", iface, "-Q", "in", "-w", out_pcap, "-B", "4096"])
    if count is not None:
        cmd += ["-c", str(count)]
    cmd += capture_filter.split()
    p = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1)
    return p


def replay(iface, send_pcap, pps=1000):
    subprocess.run(sudo(["tcpreplay", f"--pps={pps}", "--intf1=" + iface, send_pcap]),
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"[+] replayed {os.path.basename(send_pcap)} on {iface}")


def stop(proc, fn):
    """Stop the process running fn by sending it SIGINT (then SIGKILL if it ignores it)."""
    subprocess.run(sudo(["pkill", "-INT", "-f", fn]), capture_output=True)
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        subprocess.run(sudo(["pkill", "-9", "-f", fn]), capture_output=True)
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            pass

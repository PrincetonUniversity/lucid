#!/usr/bin/env python3
# Wire tests for the C backend's raw-socket driver (lucidcc --rawsock), over a veth pair.
#
# Two sub-tests, sharing the veth setup + build via driverlib:
#   - reflector: send 100 UDP packets, check the count + MAC swap on the way back (the
#     switch binds port 0 to veth0; we send/capture on veth1);
#   - scanloop:  feed one endless `tick` + 10 `pkt_in`, check all 10 `pkt_out` come back
#     -- proving the pipeline interleaves the endless recirc with fresh input (no
#     starvation), not just plumbing.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_rawsock.py --gen   # once, where lucidcc is built: .dpt -> lucidprog.c
#   sudo python3 test_rawsock.py         # normal loop: gcc + run + check
#
# Needs root (raw sockets, veth, tcpdump/tcpreplay): run with sudo.

import os
import sys
import time
import subprocess

import driverlib as dl

SWITCH_IFACE, SEND_IFACE = "veth0", "veth1"   # switch binds port 0 to veth0; we drive veth1
NUM = 100        # reflector packets
NUM_SCAN = 10    # scanloop pkt_in packets

BUILD = os.path.join(dl.SCRIPT_DIR, "_rawsock_build")   # one subdir per sub-test
REFL_BUILD = os.path.join(BUILD, "refl")
SCAN_BUILD = os.path.join(BUILD, "scanloop")


def _cfile(build): return os.path.join(build, "lucidprog.c")
def _bin(build):   return os.path.join(build, "lucidprog")


def gen():
    dl.gen_single(os.path.join(dl.PROGRAMS, "ethswaprefl.dpt"), "--rawsock", _cfile(REFL_BUILD))
    dl.gen_single(os.path.join(dl.PROGRAMS, "scanloop.dpt"), "--rawsock", _cfile(SCAN_BUILD))
    print("[+] regenerated lucidprog.c -- commit them, then run without --gen")


def _start_switch(binf):
    """Start the switch (port 0 -> veth0) and wait for its readiness line."""
    sw = subprocess.Popen(["sudo", binf, "--interface", f"0:{SWITCH_IFACE}"],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    dl.wait_for_line(sw, "Init complete.")
    time.sleep(1)
    return sw


def test_reflector():
    print("=== reflector ===")
    binf = dl.build_gcc(_cfile(REFL_BUILD), _bin(REFL_BUILD))
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "rawsock.send.pcap")
    recv = os.path.join(dl.PCAPS, "rawsock.recv.pcap")
    dl.reflector_pcap(send, NUM)
    cap = dl.start_capture(SEND_IFACE, recv, count=NUM)          # inbound UDP flow only
    sw = _start_switch(binf)
    dl.replay(SEND_IFACE, send)
    try:
        cap.wait(timeout=15)
    except subprocess.TimeoutExpired:
        cap.terminate(); cap.wait()
    dl.stop_switch(sw, binf)
    return dl.reflector_check(send, recv)


def test_scanloop():
    print("=== scanloop ===")
    binf = dl.build_gcc(_cfile(SCAN_BUILD), _bin(SCAN_BUILD))
    tags = dl.event_tags(SCAN_BUILD, "tick", "pkt_in", "pkt_out")
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "rawsock_scanloop.send.pcap")
    recv = os.path.join(dl.PCAPS, "rawsock_scanloop.recv.pcap")
    dl.write_frames(send, dl.scanloop_frames(tags, NUM_SCAN))
    print(f"[+] wrote 1 tick + {NUM_SCAN} pkt_in frames to {os.path.basename(send)}")
    cap = dl.start_capture(SEND_IFACE, recv, bpf="")            # all inbound (pkt_out is ethertype 666)
    sw = _start_switch(binf)
    dl.replay(SEND_IFACE, send)
    time.sleep(3)                                               # serve all pkt_in despite the endless tick
    cap.terminate(); cap.wait()
    dl.stop_switch(sw, binf)
    return dl.scanloop_check(recv, tags["pkt_out"], NUM_SCAN)


TESTS = [test_reflector, test_scanloop]

if __name__ == "__main__":
    os.makedirs(dl.PCAPS, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen(); sys.exit(0)
    ok = True
    for t in TESTS:
        ok = t() and ok
    sys.exit(0 if ok else 1)

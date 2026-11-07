#!/usr/bin/env python3
# Two tests for the raw-socket driver (lucidcc --rawsock):
#   - reflector: send 100 UDP packets, check the count + MAC swap on the way back 
#     (the switch binds port 0 to veth0; we send/capture on veth1);
#   - scanloop:  feed one recirculating `tick` + 10 `pkt_in`, check all 10 `pkt_out` come back
#     -- proving the pipeline interleaves the endless recirc with fresh input.
#
# To test in a container, run lucidcc on the host to generate lucidprog.c, then run the test:
#   sudo python3 test_rawsock.py --gen   # compile lucid to c
#   sudo python3 test_rawsock.py         # gcc + run + check
# Needs root.

import os
import sys
import time
import subprocess

import driverlib as dl

if dl.IS_LINUX:
    SWITCH_IFACE, SEND_IFACE = "veth0", "veth1"
else:
    SWITCH_IFACE, SEND_IFACE = "feth0", "feth1"
NUM = 100        # reflector packets
NUM_SCAN = 10    # scanloop pkt_in packets

BUILD = os.path.join(dl.SCRIPT_DIR, "_rawsock_build")   # one subdir per sub-test
REFL_BUILD = os.path.join(BUILD, "refl")
SCAN_BUILD = os.path.join(BUILD, "scanloop")

def _cfile(build): return os.path.join(build, "lucidprog.c")
def _bin(build):   return os.path.join(build, "lucidprog")

def _gen(prog, build):
    os.makedirs(build, exist_ok=True)
    r = dl.run_lucidcc(os.path.join(dl.PROGRAMS, prog), "-o", _cfile(build), "--rawsock")
    if r.returncode != 0:
        sys.exit(f"[-] lucidcc failed on {prog}:\n" + dl.tail(r.stdout, 5))
    print(f"[+] lucidcc --rawsock {prog} -> {os.path.relpath(_cfile(build))}")

def gen():
    _gen("ethswaprefl.dpt", REFL_BUILD)
    _gen("scanloop.dpt", SCAN_BUILD)
    print("[+] regenerated lucidprog.c, rerun without --gen to test")

def _build(build):
    if not os.path.exists(_cfile(build)):
        sys.exit(f"[-] {_cfile(build)} not found -- run with --gen first")
    r = dl.run_gcc(_cfile(build), _bin(build))
    if r.returncode != 0:
        sys.exit("[-] gcc failed:\n" + dl.gcc_errors(r.stdout))
    return _bin(build)

def _wait_for_line(switch, needle, timeout=30):
    """Echo switch stdout until `needle` appears; exit if the switch dies first."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        line = switch.stdout.readline().decode(errors="replace")
        if not line:
            err = switch.stderr.read().decode(errors="replace")
            sys.exit(f"[-] switch exited early (rc={switch.poll()}): {err}")
        print("    switch: " + line.strip())
        if needle in line:
            return
    raise TimeoutError(f"switch did not print {needle!r} within {timeout}s")

def _start_switch(binf):
    sw = subprocess.Popen(dl.sudo([binf, "--interface", f"0:{SWITCH_IFACE}"]),
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    _wait_for_line(sw, "Init complete.")
    time.sleep(1)
    return sw


def test_reflector():
    print("=== reflector ===")
    binf = _build(REFL_BUILD)
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "rawsock.send.pcap")
    recv = os.path.join(dl.PCAPS, "rawsock.recv.pcap")
    dl.reflector_pcap(send, NUM)
    cap = dl.start_capture(SEND_IFACE, recv, "udp port 5000", count=NUM)
    sw = _start_switch(binf)
    dl.replay(SEND_IFACE, send)
    try:
        cap.wait(timeout=15)
    except subprocess.TimeoutExpired:
        cap.terminate(); cap.wait()
    dl.stop(sw, binf)
    return dl.reflector_check(send, recv)


def test_scanloop():
    print("=== scanloop ===")
    binf = _build(SCAN_BUILD)
    tags = dl.event_tags(SCAN_BUILD, "tick", "pkt_in", "pkt_out")
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "rawsock_scanloop.send.pcap")
    recv = os.path.join(dl.PCAPS, "rawsock_scanloop.recv.pcap")
    dl.write_frames(send, dl.scanloop_frames(tags, NUM_SCAN))
    print(f"[+] wrote 1 tick + {NUM_SCAN} pkt_in frames to {os.path.basename(send)}")
    cap = dl.start_capture(SEND_IFACE, recv, "")   # everything: pkt_out is ethertype 666, not UDP
    sw = _start_switch(binf)
    dl.replay(SEND_IFACE, send)
    time.sleep(3)                                               # serve all pkt_in despite the endless tick
    cap.terminate(); cap.wait()
    dl.stop(sw, binf)
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

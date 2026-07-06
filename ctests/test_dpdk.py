#!/usr/bin/env python3
# Wire tests for the C backend's DPDK driver (lucidcc --dpdk).
#
# Four sub-tests, sharing frame-building / checks / setup via driverlib:
#   - reflector: reflect 100 UDP packets over the pcap PMD (net_pcap: file in/out);
#   - af_packet: the same reflector over a real interface (net_af_packet vdev on a veth),
#                the DPDK analogue of the raw-socket on-wire test;
#   - events:    events.dpt -- one handler emits a recirc (generate_self) AND a port
#                output (generate_port); ingress on port 1, egress on port 0. Checks
#                multi-out dispatch, recirc drain, framing, and multi-port rx;
#   - scanloop:  endless `tick` + 10 `pkt_in`, check all 10 `pkt_out` (no starvation).
#
# Transports: the pcap-PMD sub-tests read/write pcap files (no veth); af_packet uses a
# veth pair + tcpdump/tcpreplay. Runs under emulation (--no-huge; the in-container DPDK is
# built with a westmere baseline). Needs root (DPDK EAL): run with sudo.
#
# Two-phase, so the in-container loop never rebuilds Lucid:
#   sudo python3 test_dpdk.py --gen   # once, where lucidcc is built: .dpt -> build dirs
#   sudo python3 test_dpdk.py         # normal loop: make + run + check

import os
import sys
import time
import subprocess

import driverlib as dl

SWITCH_IFACE, SEND_IFACE = "veth0", "veth1"
NUM = 100          # reflector packets
NUM_EVENTS = 5     # events pkt_in packets
NUM_SCAN = 10      # scanloop pkt_in packets

REFL_BUILD = os.path.join(dl.SCRIPT_DIR, "_dpdk_build")            # ethswaprefl (reflector + af_packet)
EVENTS_BUILD = os.path.join(dl.SCRIPT_DIR, "_dpdk_events_build")   # events.dpt
SCAN_BUILD = os.path.join(dl.SCRIPT_DIR, "_dpdk_scanloop_build")   # scanloop.dpt


def gen():
    dl.gen_dpdk(os.path.join(dl.PROGRAMS, "ethswaprefl.dpt"), REFL_BUILD)
    dl.gen_dpdk(os.path.join(dl.PROGRAMS, "events.dpt"), EVENTS_BUILD)
    dl.gen_dpdk(os.path.join(dl.PROGRAMS, "scanloop.dpt"), SCAN_BUILD)
    print("[+] regenerated build dirs -- run without --gen to make + run + check")


def _run_pcap_pmd(binf, vdevs, poll_pcap, n, extra_eal=()):
    """Run the switch under the pcap PMD (file in/out): start it, poll poll_pcap until it
    holds n frames (the PMD flushes per TX burst), then stop. Exits on early switch death."""
    if os.path.exists(poll_pcap):
        os.remove(poll_pcap)
    cmd = ["sudo", binf, "--no-huge", *extra_eal, "-l", "0", "-n", "1", "--no-pci"]
    for v in vdevs:
        cmd += ["--vdev", v]
    print(f"[+] run: {' '.join(cmd[1:])}")
    sw = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True)
    if not dl.poll_until(poll_pcap, n, sw):
        out = sw.stdout.read().decode(errors="replace")
        print("[-] switch exited early:\n" + "\n".join("    " + l for l in out.strip().splitlines()[-8:]))
        sys.exit(1)
    dl.stop_switch(sw, binf)


def test_reflector():
    print("=== reflector (pcap PMD) ===")
    binf = dl.build_make(REFL_BUILD)
    send = os.path.join(dl.PCAPS, "dpdk.send.pcap")
    recv = os.path.join(dl.PCAPS, "dpdk.recv.pcap")
    dl.reflector_pcap(send, NUM)
    _run_pcap_pmd(binf, [f"net_pcap0,rx_pcap={send},tx_pcap={recv}"], recv, NUM)
    return dl.reflector_check(send, recv)


def test_afpacket():
    print("=== af_packet (live veth) ===")
    binf = dl.build_make(REFL_BUILD)   # same reflector program, different vdev
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "afpacket.send.pcap")
    recv = os.path.join(dl.PCAPS, "afpacket.recv.pcap")
    dl.reflector_pcap(send, NUM)
    cap = dl.start_capture(SEND_IFACE, recv, count=NUM)
    # DPDK's stdout is block-buffered through sudo; just give EAL + the af_packet port a
    # fixed moment to come up (log to a file, not a pipe we block-read).
    logf = open(os.path.join(REFL_BUILD, "afpacket.switch.log"), "w")
    sw = subprocess.Popen(
        ["sudo", binf, "--no-huge", "-l", "0", "-n", "1", "--no-pci",
         "--vdev", f"net_af_packet0,iface={SWITCH_IFACE}"],
        stdout=logf, stderr=subprocess.STDOUT, start_new_session=True)
    time.sleep(5)
    if sw.poll() is not None:
        logf.close()
        print("[-] switch exited early:\n" + "".join("    " + l for l in open(logf.name).readlines()[-8:]))
        cap.terminate(); cap.wait(); sys.exit(1)
    print(f"[+] switch up (af_packet on {SWITCH_IFACE})")
    dl.replay(SEND_IFACE, send)
    try:
        cap.wait(timeout=15)
    except subprocess.TimeoutExpired:
        cap.terminate(); cap.wait()
    dl.stop_switch(sw, binf)
    logf.close()
    return dl.reflector_check(send, recv)


def test_events():
    print("=== events (recirc + port, multi-port rx) ===")
    binf = dl.build_make(EVENTS_BUILD)
    tags = dl.event_tags(EVENTS_BUILD, "pkt_in", "pkt_out")
    in_pcap = os.path.join(dl.PCAPS, "dpdk_events.in.pcap")
    empty = os.path.join(dl.PCAPS, "dpdk_events.empty.pcap")
    port0 = os.path.join(dl.PCAPS, "dpdk_events.port0.pcap")   # egress (checked)
    port1 = os.path.join(dl.PCAPS, "dpdk_events.port1.pcap")   # ingress port, must stay empty
    ins = [(0x0A000001 + i, 5000 + i) for i in range(NUM_EVENTS)]  # vary fields to check passthrough
    dl.write_frames(in_pcap, [dl.lucid_frame(tags["pkt_in"], ip, port) for ip, port in ins])
    dl.empty_pcap(empty)
    if os.path.exists(port1):
        os.remove(port1)
    print(f"[+] wrote {NUM_EVENTS} Lucid-framed pkt_in frames")
    # ingress on port 1, egress on port 0 -> exercises multi-port rx. -m 512 for the
    # 2-port mbuf pool (overflows --no-huge's default heap otherwise).
    _run_pcap_pmd(binf,
                  [f"net_pcap0,rx_pcap={empty},tx_pcap={port0}",
                   f"net_pcap1,rx_pcap={in_pcap},tx_pcap={port1}"],
                  port0, NUM_EVENTS, extra_eal=["-m", "512"])
    expected = [dl.lucid_frame(tags["pkt_out"], ip, port) for ip, port in ins]
    try:
        recv = [bytes(p) for p in dl.rdpcap(port0)]
    except Exception:
        recv = []
    try:
        p1 = list(dl.rdpcap(port1))
    except Exception:
        p1 = []
    print(f"[*] fed {NUM_EVENTS} pkt_in on port 1, port0 got {len(recv)} pkt_out, port1 got {len(p1)}")
    if len(p1) != 0:
        print(f"[-] FAIL: port 1 emitted {len(p1)} packet(s) (expected none)"); return False
    if len(recv) != len(expected):
        print("[-] FAIL: port0 pkt_out count differs"); return False
    for i, (r, e) in enumerate(zip(recv, expected)):
        if r != e:
            print(f"[-] FAIL: pkt_out[{i}] framing mismatch\n    got {r.hex()}\n    exp {e.hex()}"); return False
    print("[+] PASS: port-1 ingress dispatched, recirc drained, port0 pkt_out frames correct, port1 empty")
    return True


def test_scanloop():
    print("=== scanloop ===")
    binf = dl.build_make(SCAN_BUILD)
    tags = dl.event_tags(SCAN_BUILD, "tick", "pkt_in", "pkt_out")
    in_pcap = os.path.join(dl.PCAPS, "dpdk_scanloop.in.pcap")
    out_pcap = os.path.join(dl.PCAPS, "dpdk_scanloop.out.pcap")
    dl.write_frames(in_pcap, dl.scanloop_frames(tags, NUM_SCAN))
    print(f"[+] wrote 1 tick + {NUM_SCAN} pkt_in frames")
    _run_pcap_pmd(binf, [f"net_pcap0,rx_pcap={in_pcap},tx_pcap={out_pcap}"], out_pcap, NUM_SCAN)
    return dl.scanloop_check(out_pcap, tags["pkt_out"], NUM_SCAN)


TESTS = [test_reflector, test_afpacket, test_events, test_scanloop]

if __name__ == "__main__":
    os.makedirs(dl.PCAPS, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen(); sys.exit(0)
    ok = True
    for t in TESTS:
        ok = t() and ok
    sys.exit(0 if ok else 1)

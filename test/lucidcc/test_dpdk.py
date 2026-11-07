#!/usr/bin/env python3
# A few tests for the C backend's DPDK driver (lucidcc --dpdk).
#   - reflector: reflect 100 UDP packets over the pcap PMD (net_pcap: file in/out);
#   - af_packet: reflector over a net_af_packet vdev on a veth,
#   - events:    events.dpt -- one handler does a generate_self and a generate_port; 
#                ingress on port 1, egress on port 0. 
#                Checks multi-out dispatch, recirc drain, framing, and multi-port rx;
#   - scanloop:  looping `tick` event + 10 `pkt_in`, check all 10 `pkt_out` (no starvation).
#   - recircstorm: a `tick` that recirculates twice per event, so the recirculation
#                queue stays full, + 2000 `pkt_in`: all 2000 `pkt_out` must come back
#                (overload drops recirculations, never arrivals).
#
# Runs with hugepages disabled (--no-huge), as root in the docker/dpdk image.
#
# Usage:
#   python3 test_dpdk.py --gen                                # host: .dpt -> build dirs
#   docker/dpdk/run.sh test/lucidcc python3 test_dpdk.py      # image: make + run + check

import os
import sys
import time
import subprocess

import driverlib as dl

SWITCH_IFACE, SEND_IFACE = "veth0", "veth1"
NUM = 100          # reflector packets
NUM_EVENTS = 5     # events pkt_in packets
NUM_SCAN = 10      # scanloop pkt_in packets
NUM_STORM = 2000   # recircstorm pkt_in packets: enough to keep arriving while the storm runs

BUILD = os.path.join(dl.SCRIPT_DIR, "_dpdk_build")   # one subdir per sub-test
REFL_BUILD = os.path.join(BUILD, "refl")             # ethswaprefl (reflector + af_packet)
EVENTS_BUILD = os.path.join(BUILD, "events")         # events.dpt
SCAN_BUILD = os.path.join(BUILD, "scanloop")         # scanloop.dpt
STORM_BUILD = os.path.join(BUILD, "recircstorm")     # recircstorm.dpt


def _gen(prog, build):
    os.makedirs(build, exist_ok=True)
    r = dl.run_lucidcc(os.path.join(dl.PROGRAMS, prog), "--dpdk", "--build", build)
    if r.returncode != 0:
        sys.exit(f"[-] lucidcc failed on {prog}:\n" + dl.tail(r.stdout, 5))
    print(f"[+] lucidcc --dpdk {prog} --build {os.path.relpath(build)}")

def gen():
    _gen("ethswaprefl.dpt", REFL_BUILD)
    _gen("events.dpt", EVENTS_BUILD)
    _gen("scanloop.dpt", SCAN_BUILD)
    _gen("recircstorm.dpt", STORM_BUILD)
    print("[+] regenerated build dirs -- run without --gen to make + run + check")


def _build(build):
    if not os.path.exists(os.path.join(build, "lucidprog.c")):
        sys.exit(f"[-] {build}/lucidprog.c not found -- run with --gen first")
    print("[+] make")
    subprocess.run(["make", "-C", build], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    binfile = os.path.join(build, "build", "lucidprog")
    if not os.path.exists(binfile):
        sys.exit(f"[-] {binfile} not built")
    return binfile


def _poll_until(pcap_path, n, switch, timeout=15):
    """Wait until pcap_path holds >= n packets; False if the switch exited first."""
    from scapy.all import rdpcap
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


def _run_pcap_pmd(binf, vdevs, poll_pcap, n, extra_eal=()):
    """Run the switch under the pcap PMD (file in/out): start it, poll poll_pcap until it
    holds n frames (the PMD flushes per TX burst), then stop. Exits on early switch death."""
    if os.path.exists(poll_pcap):
        os.remove(poll_pcap)
    cmd = dl.sudo([binf, "--no-huge", *extra_eal, "-l", "0", "-n", "1", "--no-pci"])
    for v in vdevs:
        cmd += ["--vdev", v]
    print(f"[+] run: {' '.join(cmd[1:])}")
    sw = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True)
    if not _poll_until(poll_pcap, n, sw):
        out = sw.stdout.read().decode(errors="replace")
        print("[-] switch exited early:\n" + "\n".join("    " + l for l in out.strip().splitlines()[-8:]))
        sys.exit(1)
    dl.stop(sw, binf)


def test_reflector():
    print("=== reflector (pcap PMD) ===")
    binf = _build(REFL_BUILD)
    send = os.path.join(dl.PCAPS, "dpdk.send.pcap")
    recv = os.path.join(dl.PCAPS, "dpdk.recv.pcap")
    dl.reflector_pcap(send, NUM)
    _run_pcap_pmd(binf, [f"net_pcap0,rx_pcap={send},tx_pcap={recv}"], recv, NUM)
    return dl.reflector_check(send, recv)


def test_afpacket():
    print("=== af_packet (live veth) ===")
    binf = _build(REFL_BUILD)   # same reflector program, different vdev
    dl.ensure_veths(SWITCH_IFACE, SEND_IFACE)
    send = os.path.join(dl.PCAPS, "afpacket.send.pcap")
    recv = os.path.join(dl.PCAPS, "afpacket.recv.pcap")
    dl.reflector_pcap(send, NUM)
    cap = dl.start_capture(SEND_IFACE, recv, "udp port 5000", count=NUM)

    logf = open(os.path.join(REFL_BUILD, "afpacket.switch.log"), "w")
    sw = subprocess.Popen(
        dl.sudo([binf, "--no-huge", "-l", "0", "-n", "1", "--no-pci",
                 "--vdev", f"net_af_packet0,iface={SWITCH_IFACE}"]),
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
    dl.stop(sw, binf)
    logf.close()
    return dl.reflector_check(send, recv)


def test_events():
    print("=== events (recirc + port, multi-port rx) ===")
    from scapy.all import rdpcap
    binf = _build(EVENTS_BUILD)
    tags = dl.event_tags(EVENTS_BUILD, "pkt_in", "pkt_out")
    in_pcap = os.path.join(dl.PCAPS, "dpdk_events.in.pcap")
    empty = os.path.join(dl.PCAPS, "dpdk_events.empty.pcap")
    port0 = os.path.join(dl.PCAPS, "dpdk_events.port0.pcap")   # output
    port1 = os.path.join(dl.PCAPS, "dpdk_events.port1.pcap")   # input
    ins = [(0x0A000001 + i, 5000 + i) for i in range(NUM_EVENTS)]  # vary fields to check passthrough
    dl.write_frames(in_pcap, [dl.lucid_frame(tags["pkt_in"], ip, port) for ip, port in ins])
    dl.write_pcap(empty, [])
    if os.path.exists(port1):
        os.remove(port1)
    print(f"[+] wrote {NUM_EVENTS} Lucid-framed pkt_in frames")
    # ingress on port 1, egress on port 0 -> exercises multi-port rx. -m 512 memory for mbufs
    _run_pcap_pmd(binf,
                  [f"net_pcap0,rx_pcap={empty},tx_pcap={port0}",
                   f"net_pcap1,rx_pcap={in_pcap},tx_pcap={port1}"],
                  port0, NUM_EVENTS, extra_eal=["-m", "512"])
    expected = [dl.lucid_frame(tags["pkt_out"], ip, port) for ip, port in ins]
    try:
        recv = [bytes(p) for p in rdpcap(port0)]
    except Exception:
        recv = []
    try:
        p1 = list(rdpcap(port1))
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
    binf = _build(SCAN_BUILD)
    tags = dl.event_tags(SCAN_BUILD, "tick", "pkt_in", "pkt_out")
    in_pcap = os.path.join(dl.PCAPS, "dpdk_scanloop.in.pcap")
    out_pcap = os.path.join(dl.PCAPS, "dpdk_scanloop.out.pcap")
    dl.write_frames(in_pcap, dl.scanloop_frames(tags, NUM_SCAN))
    print(f"[+] wrote 1 tick + {NUM_SCAN} pkt_in frames")
    _run_pcap_pmd(binf, [f"net_pcap0,rx_pcap={in_pcap},tx_pcap={out_pcap}"], out_pcap, NUM_SCAN)
    return dl.scanloop_check(out_pcap, tags["pkt_out"], NUM_SCAN)


def test_recircstorm():
    print("=== recircstorm ===")
    binf = _build(STORM_BUILD)
    tags = dl.event_tags(STORM_BUILD, "tick", "pkt_in", "pkt_out")
    in_pcap = os.path.join(dl.PCAPS, "dpdk_recircstorm.in.pcap")
    out_pcap = os.path.join(dl.PCAPS, "dpdk_recircstorm.out.pcap")
    dl.write_frames(in_pcap, dl.scanloop_frames(tags, NUM_STORM))
    print(f"[+] wrote 1 multiplying tick + {NUM_STORM} pkt_in frames")
    _run_pcap_pmd(binf, [f"net_pcap0,rx_pcap={in_pcap},tx_pcap={out_pcap}"], out_pcap, NUM_STORM,
                  extra_eal=["-m", "512"])
    return dl.scanloop_check(out_pcap, tags["pkt_out"], NUM_STORM)


TESTS = [test_reflector, test_afpacket, test_events, test_scanloop, test_recircstorm]

if __name__ == "__main__":
    os.makedirs(dl.PCAPS, exist_ok=True)
    if "--gen" in sys.argv[1:]:
        gen(); sys.exit(0)
    ok = True
    for t in TESTS:
        ok = t() and ok
    sys.exit(0 if ok else 1)

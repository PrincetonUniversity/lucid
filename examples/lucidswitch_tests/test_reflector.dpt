# A few simple tests for the lucid interpreter switch using reflector.dpt
# This creates a veth pair (feth0, feth1) and runs the switch on feth0, 
# then sends packets on feth1 and captures on feth0 to verify they go through the switch.

# lucidSwitch usage:
# $REPO_ROOT/lucidSwitch reflector.dpt --interface 0:feth0
# the args are: --interface <port>:<interface name>
# you can add multiple interfaces like: --interface 0:feth0 --interface 1:otheriface

import subprocess
import sys
import time
import os
import platform

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

DPT_FILE = os.path.join(SCRIPT_DIR, "reflector.dpt")
SEND_PCAP = os.path.join(SCRIPT_DIR, "send.pcap")
RECV_PCAP = os.path.join(SCRIPT_DIR, "recv.pcap")
SEND_IFACE = "feth1"
SWITCH_IFACE = "feth0"
NUM_PACKETS = 10000
REPLAY_PPS = 250000

def repo_root():
    return subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"], text=True
    ).strip()

SWITCH_BIN = os.path.join(repo_root(), "lucidSwitch")
IS_LINUX = platform.system() == "Linux"

def cleanup_pcaps():
    """Delete old pcaps if they exist."""
    for p in [SEND_PCAP, RECV_PCAP]:
        if os.path.exists(p):
            os.remove(p)
            print(f"[+] Removed old pcap: {p}")

def build_pcap(path, n, pload_size=1024 - 14 - 20 - 8):  # 1024 total - Ethernet(14) - IP(20) - UDP(8)
    """Build a pcap with n generic UDP packets."""
    pkts = []
    for i in range(n):
        pkt = Ether(dst="ff:ff:ff:ff:ff:ff") / IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
        pkts.append(pkt)
    wrpcap(path, pkts)
    print(f"[+] Wrote {n} packets to {path}")

def ensure_veths():
    """Ensure veth interfaces are up."""
    if IS_LINUX:
        subprocess.run(["sudo", "ip", "link", "add", SWITCH_IFACE, "type", "veth", "peer", "name", SEND_IFACE], capture_output=True)
        subprocess.run(["sudo", "ip", "link", "set", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ip", "link", "set", SEND_IFACE, "up"], check=True)
    else:
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE, "create"], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "peer", SEND_IFACE], capture_output=True)
        subprocess.run(["sudo", "ifconfig", SWITCH_IFACE, "up"], check=True)
        subprocess.run(["sudo", "ifconfig", SEND_IFACE, "up"], check=True)
    print(f"[+] {SWITCH_IFACE} and {SEND_IFACE} are up")

def run_test():
    """Run the main test: start tcpdump, start switch, send packets."""
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SEND_IFACE, "-w", RECV_PCAP, "-c", str(NUM_PACKETS), "-B", "4096"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )
    time.sleep(1)  # let tcpdump settle
    print("[+] Started tcpdump on %s, waiting for switch to initialize..." % SEND_IFACE)
    switch = subprocess.Popen(
        ["sudo", SWITCH_BIN, DPT_FILE, "--interface", f"0:{SWITCH_IFACE}"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    # Wait for the switch to finish initialization
    deadline = time.time() + 30
    while time.time() < deadline:
        line = switch.stdout.readline().decode()
        if not line:
            # readline returns empty only if the pipe is closed (process exited)
            rc = switch.poll()
            stderr_out = switch.stderr.read().decode() if rc is not None else ""
            print(f"[-] Switch exited early (rc={rc})")
            print(f"[-] stderr: {stderr_out}")
            exit(1)
        if "Init complete." in line:
            print("[+] Switch initialized")
            time.sleep(1)
            break
    else:
        raise TimeoutError("Switch did not print 'Init complete.' within 30s")

    # Send packets via tcpreplay
    subprocess.run(["sudo", "tcpreplay", f"--pps={REPLAY_PPS}", "--intf1=" + SEND_IFACE, SEND_PCAP],
                    check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"[+] Sent {NUM_PACKETS} packets on {SEND_IFACE}")

    # Wait for tcpdump to finish (it exits after -c packets) with a timeout
    try:
        tcpdump.wait(timeout=30)
    except subprocess.TimeoutExpired:
        tcpdump.terminate()
        tcpdump.wait()

    # Stop the switch
    switch.terminate()
    switch.wait()

def check_packet_count():
    """Compare the number of packets sent vs received."""
    sent = rdpcap(SEND_PCAP)
    try:
        recv = rdpcap(RECV_PCAP)
    except Exception:
        recv = []

    print(f"[*] Sent: {len(sent)} packets, Received: {len(recv)} packets")
    if len(sent) == len(recv):
        print("[+] PASS: packet counts match")
    else:
        print("[-] FAIL: packet counts do not match")

def measure_throughput():
    """Measure throughput in packets per second and Mbps based on the recv pcap timestamps."""
    recv = rdpcap(RECV_PCAP)
    if len(recv) < 2:
        print("[*] Not enough packets to measure throughput")
        return
    duration = float(recv[-1].time - recv[0].time)
    if duration <= 0:
        print("[*] Duration is zero, cannot compute throughput")
        return
    pps = (len(recv) - 1) / duration
    total_bytes = sum(len(p) for p in recv)
    mbps = total_bytes * 8 / duration / 1e6
    print(f"[*] Throughput: {pps:.0f} pps, {mbps:.2f} Mbps (over {duration:.4f}s)")

def measure_tcpreplay_throughput():
    """Measure raw tcpreplay->tcpdump throughput without the switch in the loop."""
    print("[*] Measuring tcpreplay throughput (no switch)...")
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SWITCH_IFACE, "-w", RECV_PCAP, "-c", str(NUM_PACKETS), "-B", "65536"],
        stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
    )
    time.sleep(1)  # let tcpdump settle

    # Send from SEND_IFACE, tcpdump captures on SWITCH_IFACE (direct veth pair, no switch)
    replay = subprocess.run(
        ["sudo", "tcpreplay", f"--pps={REPLAY_PPS}", "--intf1=" + SEND_IFACE, SEND_PCAP],
        check=True, capture_output=True, text=True,
    )
    print(f"[+] Sent {NUM_PACKETS} packets on {SEND_IFACE}")
    print(f"[*] tcpreplay stdout: {replay.stdout.strip()}")
    print(f"[*] tcpreplay stderr: {replay.stderr.strip()}")

    try:
        tcpdump.wait(timeout=30)
    except subprocess.TimeoutExpired:
        tcpdump.terminate()
        tcpdump.wait()

    tcpdump_stderr = tcpdump.stderr.read().decode()
    print(f"[*] tcpdump stderr: {tcpdump_stderr.strip()}")

    recv = rdpcap(RECV_PCAP)
    if len(recv) < 2:
        print("[*] Not enough packets captured to measure throughput")
        return
    duration = float(recv[-1].time - recv[0].time)
    if duration <= 0:
        print("[*] Duration is zero, cannot compute throughput")
        return
    pps = (len(recv) - 1) / duration
    total_bytes = sum(len(p) for p in recv)
    mbps = total_bytes * 8 / duration / 1e6
    print(f"[*] tcpreplay throughput: {pps:.0f} pps, {mbps:.2f} Mbps (over {duration:.4f}s)")
    print(f"[*] Captured {len(recv)}/{NUM_PACKETS} packets")

# --- Main ---
if __name__ == "__main__":
    cleanup_pcaps()
    build_pcap(SEND_PCAP, NUM_PACKETS)
    ensure_veths()
    if "--tcpreplay-throughput" in sys.argv:
        measure_tcpreplay_throughput()
    else:
        run_test()
        check_packet_count()
        measure_throughput()
    


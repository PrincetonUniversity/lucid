# This is a test script for the lucidSoftSwitch using reflector.dpt.
# The test will involve sending packets through a virtual Ethernet interface (veth)
# and verifying that they are correctly reflected by the switch.

# lucidSoftSwitch usage:
# ../lucidSoftSwitch reflector.dpt --interface 0:0:feth0
# the args are: --interface <port>:<interface name>

# 1. construct a pcap file with n packets, 64B ethernet frames. Contents don't matter, so make it generic UDP.
# 2. make sure the veth interfaces are up. We are on macos, so will use feth0 and feth1 as the veth pair.
# 3. start tcpdump on feth1 to capture the reflected packets.
# 4. start lucidSoftSwitch with reflector.dpt on feth0
# 5. send packets on feth1 using tcpreplay
# 6. sanity check: compare length of sent pcap with length of captured pcap. They should match.

import subprocess
import sys
import time
import os
import platform

from scapy.all import Ether, IP, UDP, Raw, wrpcap, rdpcap

IS_LINUX = platform.system() == "Linux"

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
NUM_PACKETS = 10000
SEND_PCAP = os.path.join(SCRIPT_DIR, "send.pcap")
RECV_PCAP = os.path.join(SCRIPT_DIR, "recv.pcap")
SWITCH_BIN = os.path.join(SCRIPT_DIR, "..", "lucidSoftSwitch")
DPT_FILE = os.path.join(SCRIPT_DIR, "reflector.dpt")
SEND_IFACE = "feth1"
SWITCH_IFACE = "feth0"

# --- Step 0: Delete old pcaps if they exist ---
def cleanup_pcaps():
    for p in [SEND_PCAP, RECV_PCAP]:
        if os.path.exists(p):
            os.remove(p)
            print(f"[+] Removed old pcap: {p}")

# --- Step 1: Build a pcap with n generic UDP packets ---
def build_pcap(path, n, pload_size=1024 - 14 - 20 - 8):  # 1024 total - Ethernet(14) - IP(20) - UDP(8)
    pkts = []
    for i in range(n):
        pkt = Ether(dst="ff:ff:ff:ff:ff:ff") / IP(dst="10.0.0.1") / UDP(dport=5000) / Raw(load=bytes(pload_size))
        pkts.append(pkt)
    wrpcap(path, pkts)
    print(f"[+] Wrote {n} packets to {path}")

# --- Step 2: Ensure veth interfaces are up ---
def ensure_veths():
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

# --- Steps 3-5: Run the test ---
def run_test():
    # Step 3: Start tcpdump on feth1 to capture reflected packets
    tcpdump = subprocess.Popen(
        ["sudo", "tcpdump", "-i", SEND_IFACE, "-w", RECV_PCAP, "-c", str(NUM_PACKETS), "-B", "4096"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )
    time.sleep(1)  # let tcpdump settle

    # Step 4: Start lucidSoftSwitch on feth0
    switch = subprocess.Popen(
        [SWITCH_BIN, DPT_FILE, "--interface", f"0:{SWITCH_IFACE}"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    time.sleep(2)  # let the switch start up

    # Step 5: Send packets on feth1 via tcpreplay
    subprocess.run(["sudo", "tcpreplay", "--topspeed", "--intf1=" + SEND_IFACE, SEND_PCAP],
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

# --- Step 6: Compare packet counts ---
def check_packet_count():
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
        sys.exit(1)

# --- Measure throughput from recv pcap timestamps ---
def measure_throughput():
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

# --- Main ---
if __name__ == "__main__":
    cleanup_pcaps()
    build_pcap(SEND_PCAP, NUM_PACKETS)
    ensure_veths()
    run_test()
    check_packet_count()
    measure_throughput()
    


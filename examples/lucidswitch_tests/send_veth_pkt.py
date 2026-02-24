from scapy.all import *
import time
import sys

import platform

if platform.system() != "Darwin":
    sys.exit("This script only supports macOS (BPF sockets).")

import struct, fcntl
from scapy.arch.bpf.supersocket import L2bpfSocket



# macOS BPF direction ioctl
BIOCSDIRECTION = 0x80044277
BPF_D_IN = 0  # incoming only



# Configuration
TEST_IFACE = "feth1"
TARGET_MAC = "aa:bb:cc:dd:ee:ff"
SRC_MAC = get_if_hwaddr(TEST_IFACE)

print(f"[*] Configuration: {TEST_IFACE} ({SRC_MAC}) -> Target {TARGET_MAC}")

# 1. Craft the packet
payload = "Hello OCaml! " + ("A" * 50)  # Add 50 'A's to ensure it's long enough
packet = Ether(src=SRC_MAC, dst=TARGET_MAC, type=0x1234) / Raw(load=payload)
print(f"[*] Sending {len(packet)} byte packet...")

# 2. Define a BPF Filter (Kernel level)
# We want to capture packets that:
#   - Are destined FOR us (dst host <our_mac>)
#   - Have the specific EtherType (ether proto 0x1234)
bpf_filter = f"ether proto 0x1234"
# bpf_filter = f"ether dst {SRC_MAC} and ether proto 0x1234"

# 3. Start the Sniffer in the background
print("[*] Starting background listener...")

# Open socket and set to incoming-only
sock = L2bpfSocket(iface=TEST_IFACE, filter=bpf_filter)
fcntl.ioctl(sock.fileno(), BIOCSDIRECTION, struct.pack('I', BPF_D_IN))
# Pass pre-configured socket to sniffer
sniffer = AsyncSniffer(opened_socket=sock, count=0, timeout=5)
sniffer.start()

# IMPORTANT: Wait a split second to ensure the sniffer is actually 
# bound to the interface before we send data.
time.sleep(2)

# 4. Send the packet
print("[*] Sending packet...")
sendp(packet, iface=TEST_IFACE, verbose=False)

time.sleep(1)

# 5. Wait for the result
print("[*] Waiting for response...")
sniffer.join() # This blocks until timeout or packet received

# 6. Check results
if sniffer.results:
    for response in sniffer.results:
        print(f"\n[+] Success! Received response:")
        response.show()
        
        # Try to decode payload
        if Raw in response:
            print(f"Payload data: {response[Raw].load.decode('utf-8', errors='ignore')}")
else:
    print("\n[-] Timeout: No response received.")
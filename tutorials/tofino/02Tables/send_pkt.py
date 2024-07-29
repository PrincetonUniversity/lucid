#!/usr/bin/env python3
from scapy.all import Ether, IP, sendp
import sys

def send_packet(port):
    # Create an Ethernet/IP packet
    packet = Ether() / IP(dst="1.2.3.4")  # Replace "1.2.3.4" with the actual destination IP

    # Send the packet
    sendp(packet, iface=port)

if __name__ == "__main__":
    # Get the port from the command-line arguments
    port = sys.argv[1]

    # Send the packet
    send_packet(port)
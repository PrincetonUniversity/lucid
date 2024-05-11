import json
import socket
import struct
from scapy.all import rdpcap, IP, TCP

# NOTE: Change pcap names as needed, especially if you are using a specialized dataset
packets = rdpcap("smallFlows.pcap")

events = []

# Iterate over packets to extract details from those with an IP layer
for packet in packets:
    if packet.haslayer(IP):
        src = socket.inet_aton(packet[IP].src)
        dst = socket.inet_aton(packet[IP].dst)
        src_int = struct.unpack("I", src)[0]
        dst_int = struct.unpack("I", dst)[0]
        packet_size = len(packet)
        if packet.haslayer(TCP):
            payload = packet[TCP].payload.original  # Get the payload as bytes
            if len(payload) >= 4:
                payload_int = struct.unpack("I", payload[0:4])[0]
            else:
                payload_int = 0
            payload_hex = payload.hex() if payload else ""  # Convert payload to hex
        else:
            payload_hex = ""  # No payload for non-TCP packets

        # Extract Ethernet header fields
        eth_dst_oui = int.from_bytes(bytes.fromhex(packet.dst.replace(':', ''))[:3], byteorder='big')
        eth_dst_id = int.from_bytes(bytes.fromhex(packet.dst.replace(':', ''))[3:], byteorder='big')
        eth_src_oui = int.from_bytes(bytes.fromhex(packet.src.replace(':', ''))[:3], byteorder='big')
        eth_src_id = int.from_bytes(bytes.fromhex(packet.src.replace(':', ''))[3:], byteorder='big')
        eth_type = packet.type

        # Extract IPv4 header fields
        ip_version = packet[IP].version
        ip_ihl = packet[IP].ihl
        ip_diffserv = packet[IP].tos
        ip_total_len = packet[IP].len
        ip_id = packet[IP].id
        ip_flags = packet[IP].flags.value
        ip_frag_offset = packet[IP].frag
        ip_ttl = packet[IP].ttl
        ip_protocol = packet[IP].proto
        ip_hdr_csum = packet[IP].chksum
        ip_src = src_int
        ip_dst = dst_int

        # Append the details to our event (without the timestamp)
        events.append({
            "name": "eth_ip",
            "args": [
                eth_dst_oui, eth_dst_id, eth_src_oui, eth_src_id, eth_type,
                ip_version, ip_ihl, ip_diffserv, ip_total_len, ip_id,
                ip_flags, ip_frag_offset, ip_ttl, ip_protocol, ip_hdr_csum,
                ip_src, ip_dst
            ]
        })

dict_to_json = {
    "max_time": 99999999,  # I just used the default one I was given, idk what this means 
    "events": events
}

json_output = json.dumps(dict_to_json, indent=2)

# Saving the JSON output to a file
with open('packet_data.json', 'w') as file:
    file.write(json_output)

# Optionally, print a message indicating success
print("Packet data has been saved to 'packet_data.json'")
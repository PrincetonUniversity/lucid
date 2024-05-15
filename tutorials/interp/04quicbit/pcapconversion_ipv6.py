import json
from scapy.all import rdpcap, IPv6, UDP
import ipaddress

def ipv6_to_int(ipv6_addr):
    ip_obj = ipaddress.ip_address(ipv6_addr)
    return int(ip_obj)

def track_quic_spin_bits(packets):
    connection_spin_bits = {}
    events = []

    for packet in packets:
        if packet.haslayer(IPv6) and packet.haslayer(UDP):
            
            timestamp = int((packet.time) * 1000)

            src = ipv6_to_int(packet[IPv6].src)
            dst = ipv6_to_int(packet[IPv6].dst)

            connection_id = f"{src}-{dst}"
            payload = bytes(packet[UDP].payload)

            if len(payload) > 0:
                first_byte = payload[0]
                # Check if it's a short header (most significant bit should be 0)
                if (first_byte & 0x80) == 0:
                    # Extract the spin bit (third most significant bit)
                    spin_bit = (first_byte & 0x04) >> 2

                    # Tracking and comparing spin bit flips
                    if connection_id in connection_spin_bits:
                        last_spin_bit = connection_spin_bits[connection_id]
                        spin_flipped = (last_spin_bit != spin_bit)
                    else:
                        spin_flipped = False  # No flip on first observation

                    # Update the last known state of spin bit for this connection
                    connection_spin_bits[connection_id] = spin_bit
                else:
                    spin_flipped = False
            else:
                spin_flipped = False

            events.append({
                "name": "quic_packet_alt",
                "args": [src, dst, spin_flipped, timestamp]
            })

    return events

packets = rdpcap("aioquic_spinbit_traffic.pcapng")
events = track_quic_spin_bits(packets)
dict_to_json = {
    "max_time": 10000,
    "events": events
}

json_output = json.dumps(dict_to_json, indent=2)

with open('quicbit4.json', 'w') as file:
    file.write(json_output)

print("Packet data has been saved to 'packet_data.json'")

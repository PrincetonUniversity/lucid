import json
import socket
import struct
from scapy.all import rdpcap, IP, TCP


# NOTE: Change pcap names as needed. especially if you are using a specialized dataset
packets = rdpcap("jumphostssh.pcap")

events = []
normalize = -1;

# Iterate over packets to extract details from those with an IP layer
for packet in packets:
    if normalize == -1:
        normalize = packet.time;
    packet_time = int((packet.time - normalize) * 1000);
    #print(packet_time);
    if packet.haslayer(IP):
        src = socket.inet_aton(packet[IP].src)
        dst = socket.inet_aton(packet[IP].dst)
        src_int = struct.unpack("I", src)[0];
        dst_int = struct.unpack("I", dst)[0];
        #print(src_int, dst_int);
        packet_size = len(packet)
        # Check if the packet is TCP to attempt extracting payload . NOTE: FOR ME (HIEU), I NEED TO CHANGE THIS TO UDP
        if packet.haslayer(TCP):
            payload = packet[TCP].payload.original  # Get the payload as bytes
            if len(payload) >= 4:
                payload_int = struct.unpack("I", payload[0:4])[0];
                #print(payload_int, src, dst);
            else:
                payload_int = 0;

        # --------------------------- TO EDIT WITH JOHN SONCHACK

            payload_hex = payload.hex() if payload else ""  # Convert payload to hex
        else:
            payload_hex = ""  # No payload for non-TCP packets

        # END OF TO EDIT WITH JOHN SONCHACK

        # Append the details to our event NOTE: PLEASE CHANGE THIS.
        events.append({
            "name": "eth_ip",
            # THIS IS HOW I set it up, please change at your own discretion.
            "args": [11, 22, 2048, packet_size, payload_int, src_int, dst_int,128],
            "timestamp": packet_time
        })

dict_to_json = {
    "max time": 999999,  # I just used the default one i was given, idk what this means 
    "events": events
}

json_output = json.dumps(dict_to_json, indent=2)

# Saving the JSON output to a file
with open('pivot.json', 'w') as file:
    file.write(json_output)

# Optionally, print a message indicating success
print("Packet data has been saved to 'pivot.json'")

def get_memop(mem_val, passed_val):
    if mem_val < 1000:
        return True
    return False;

def set_memop(mem_val, new_val):
    if mem_val < 1000:
        return new_val
    else:
        return mem_val
    
def update(arr, idx, get_memop, get_memop_arg, set_memop, set_memop_arg):
    ## to check whether arr[idx] is smaller than 1000
    return_value = get_memop(arr[idx], get_memop_arg)
    arr[idx] = set_memop(arr[idx], set_memop_arg)
    return return_value


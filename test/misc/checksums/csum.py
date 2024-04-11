# checksum calculation test

import dpkt, json, subprocess, re, struct, random

def test(ip_dst):
    print ("Testing checksum calculation for IP packet with destination IP address " + str(ip_dst))
    # create an Eth/IP packet with no payload 
    ip_packet = dpkt.ip.IP()
    ip_packet.hl = 5
    ip_packet.dst = struct.pack("!I", ip_dst)
    ip_packet.ttl = 0
    ip_str = ip_packet.pack()
    expected_csum = ip_packet.sum
    print("expected csum = " + str(expected_csum))
    eth_packet = dpkt.ethernet.Ethernet()
    eth_packet.data = ip_str
    eth_str = eth_packet.pack()
    input_pkt_str = eth_str.hex()
    # create the input json for the test
    input_json = {
        "events" : [
        {
            "type" : "packet",
            "bytes" : input_pkt_str
        }
        ]
    }
    json.dump(input_json, open("checksum.json", "w"))
    # run "../dpt checksum.dpt" and get the output string. use subprocess.POpen
    process = subprocess.Popen(["../../dpt", "checksum.dpt"], stdout=subprocess.PIPE)
    output, _ = process.communicate()
    output_str = output.decode()

    # parse the exit event bytestring it will be on a line like this:
    #     bits(<<34B unparsed bytestring=0x0000000000000000000000000800450000140000000040007aeb0000000000000000>>) at port 128, t=600
    regex = r".*?bits.*?unparsed bytestring=0x([0-9a-fA-F]+)>>"
    match = re.search(regex, output_str)
    if (match):
        output_pkt_str = match.group(1)
        if (output_pkt_str == input_pkt_str):
            print ("Checksum test passed")
        else:
            print ("Checksum test failed (wrong checksum)")
    else:
        print("Checksum test failed (no output found)")
        print(output_str)
        exit(1)
    # output: <re.Match object; span=(19, 120), match='bits(<<34B unparsed bytestring=0x0000000000000000>
    # time.sleep(1)

import math
for i in range(100):
    dst = random.randint(0, 2**32-1)
    test(dst)


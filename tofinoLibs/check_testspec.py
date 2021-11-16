#!/usr/bin/env python3

import sys, time, random, socket, os, struct, json, copy, re, socket, string
import binascii
import dpkt
from collections import namedtuple

""" 
    Analyze the tofino-model's output log to see if the test spec passed. 
"""
def main():
    if (len(sys.argv) != 3):
        print ("usage: check_testspec.py testspec.json model_output.log")
        return
    # 1. load the test spec. 
    specfn = sys.argv[1]
    modellog_fn = sys.argv[2]
    traceJson = json.load(open(specfn, "r"))
    specRecs = traceJson["model-output"]
    # print (specRecs)    
    # 2. extract the output packet records from the log.
    modelRecs = parse_tx_pkts_from_model_log(modellog_fn)
    # 3. check if every spec rec is represented in the model output
    match = compare_recs(specRecs, modelRecs)
    print (match)
    return


def parse_tx_pkts_from_model_log(log_fn):
    """ extract from the model log all the packets that were sent out, 
        along with the port that they were sent out of. """
    start_pkt_regex = r':\s*?:.*?:(.*?):(<.*?>):=* Tx Pkt to port (\d*) \(.*='
    # r = r'Egress Pkt from TM to port (\d*).*?:Packet :.*?(.*?):='
    packets = []
    log_lines = open(log_fn, "r").readlines()
    for line in log_lines:
        # check if a packet is starting
        start_info = re.findall(start_pkt_regex, line)
        if (len(start_info) == 1):
            new_pkt_key = {"pid": start_info[0][0], "tid": start_info[0][1]}
            new_pkt_rec = {"port": start_info[0][2], "bytes": b''}
            packets.append((new_pkt_key, new_pkt_rec))
        else:
            # check if this line contains data from any output packets
            updated_packets = []
            for (pkt_key, pkt_rec) in packets:
                pkt_bytes_regex = r':%s:%s:(.*)'%(pkt_key["pid"], pkt_key["tid"])
                bytes_found = re.findall(pkt_bytes_regex, line)
                if (len(bytes_found) > 0):
                    bytes_found = bytes_found[0].replace(" ", "")
                    if (all(c in string.hexdigits for c in bytes_found)):
                        new_bytes = binascii.unhexlify(bytes_found)
                        pkt_rec["bytes"] += new_bytes
                updated_packets.append((pkt_key, pkt_rec))
            packets = updated_packets
    # print ("found %s packets"%(len(packets)))
    parsed_packets = [parse_single_packet(p[1]["port"], p[1]["bytes"]) for p in packets]
    return parsed_packets

def parse_single_packet(port, pkt):
    """ Parse a single packet into a record """
    pkt = dpkt.ethernet.Ethernet(pkt)
    rec = {
        "port":int(port),
        "eth.src":(":".join('{:02x}'.format(x) for x in pkt.src)),
        "eth.dst":(":".join('{:02x}'.format(x) for x in pkt.dst))
    }
    if (pkt.type == dpkt.ethernet.ETH_TYPE_IP): 
        ip_src = socket.inet_ntoa(pkt.ip.src)
        ip_dst = socket.inet_ntoa(pkt.ip.dst)
        ip_tos = int(pkt.ip.tos)
        rec["ip.src"] = ip_src    
        rec["ip.dst"] = ip_dst   
        rec["ip.tos"] = ip_tos    
    # print ("--- packet --- ")
    # for (k, v) in rec.items():
    #     print ("%s : %s"%(k, v))
    return rec


def rec_is_subset(reca, recb):
    is_subset = True
    for (k, v) in reca.items():
        if (k in recb):
            if (recb[k] != v):
                is_subset = False
        else:
            is_subset = False
    return is_subset
def rec_is_subset_of_one(reca, recbs):
    for recb in recbs:
        found = rec_is_subset(reca, recb)
        if (found):
            return True
    return False

def compare_recs(specRecs, modelRecs):
    """is every spec rec represented by a packet in the modelRecs? """
    match = True
    for srec in specRecs:
        this_match = rec_is_subset_of_one(srec, modelRecs)
        # if (not this_match):
        #     print ("missing in output: "+str(srec))
        match = match & this_match
    return match




if __name__ == '__main__':
    main()
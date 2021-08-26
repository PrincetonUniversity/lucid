#!/usr/bin/env python3

import sys, time, random, socket, os, struct, json, copy
import binascii
import dpkt
from collections import namedtuple

"""Testspec utilities."""
usage = "usage: testspec_info.py testspec.json <command args>"
def main():
    cmd = sys.argv[1]
    if (cmd == "input_port"):
        traceJson = json.load(open(sys.argv[2], "r"))
        print (traceJson["input_port"])
    elif (cmd == "len_packets"):
        traceJson = json.load(open(sys.argv[2], "r"))
        print (len(traceJson["packets"]))
    elif (cmd == "gen_pcap"):
        generatePcap(sys.argv[2], sys.argv[3])

defaultPktRec = {
    "eth.src":"12:12:12:12:12:12",
    "eth.dst":"34:34:34:34:34:34",
    "eth.type":dpkt.ethernet.ETH_TYPE_IP,
    "ip.src" : "12.12.12.12",
    "ip.dst" : "34.34.34.34",
    "ip.id"  : 0,
    "ip.tos" : 0,
    "ip.proto" : dpkt.ip.IP_PROTO_UDP,
    "udp.src" : 10,
    "udp.dst" : 20,
    "payload.length" : 128
}
def generatePcap(testspecfn, pcapFn):
    # parse trace json file.
    traceJson = json.load(open(testspecfn))
    specRecs = traceJson["packets"]
    packetRecs = []
    for specRec in specRecs:
        # grab defaults for any fields that are not filled in. 
        rec = {field:specRec.get(field, defaultPktRec[field]) for field in defaultPktRec.keys()}
        packetRecs.append(rec)
    pkts = [pktFromRec(p) for p in packetRecs]

    # write the packets to the output pcap file.
    pw = PcapWriter()
    pw.create(pcapFn)
    for (i, pkt) in enumerate(pkts):
        pw.write(pkt, i*4) # packets are 4 seconds apart
    pw.close()


def pktFromRec(rec):
    """ convert a pktRec into a packet """
    # craft a L4 packet (tcp / udp)
    L4Packet = None
    if (rec["eth.type"] == dpkt.ethernet.ETH_TYPE_IP):
        if (rec["ip.proto"] == dpkt.ip.IP_PROTO_UDP):
            payload = b'X'*int(rec["payload.length"])
            udpPkt = dpkt.udp.UDP(
                sport = int(rec["udp.src"]), 
                dport = int(rec["udp.dst"]))
            udpPkt.data = payload
            udpPkt.ulen += len(udpPkt.data)
            L4Packet = bytes(udpPkt)
        elif (rec["ip.proto"] == dpkt.ip.IP_PROTO_TCP):
            print ("ERROR: TCP PACKETS NOT IMPLEMENTED")
            quit()
        else:
            print ("ERROR: NON TCP/UDP PACKETS NOT IMPLEMENTED")
            quit()

    L3Packet = None

    # craft a L3 packet (ip)
    if (rec["eth.type"] == dpkt.ethernet.ETH_TYPE_IP):
        ipPkt = dpkt.ip.IP(v=4, 
            id = int(rec["ip.id"]), 
            src = socket.inet_aton(rec["ip.src"]), 
            dst = socket.inet_aton(rec["ip.dst"]), 
            p   = int(rec["ip.proto"]),
            tos = int(rec["ip.tos"])
        )
        ipPkt.data = L4Packet
        ipPkt.len += len(L4Packet)
        L3Packet = bytes(ipPkt)
    else:
        print ("ERROR: NON IP PACKETS NOT IMPLEMENTED")
        quit()

    # craft a L2 frame (ethernet)
    hexStrToBin = lambda ethAddr : binascii.unhexlify(ethAddr.replace(":", ""))
    L2Frame = None
    ethFrame = dpkt.ethernet.Ethernet(
        src=hexStrToBin(rec["eth.src"]), 
        dst=hexStrToBin(rec["eth.dst"]), 
        type=int(rec["eth.type"])
    )
    ethFrame.data = L3Packet
    L2Frame = bytes(ethFrame)    
    return L2Frame


class PcapWriter(object):
    """ Write bytestring packets into a PCAP file."""
    # global header for a version 2.4 PCAP file.
    pcap_global_header=bytes.fromhex('d4c3b2a1') + struct.pack("H",2) + struct.pack("H",4) + struct.pack("I", 0) + struct.pack("I", 0) + struct.pack("I", 1600) + struct.pack("I", 1)
    def __init__(self, f = None):
        if (f):
            self.f = f
            self.f.write(self.pcap_global_header)            
        return

    def create(self, filename):
        self.f = open(filename, "wb")
        self.f.write(self.pcap_global_header)

    def write(self, pkt, tsSec):
        """
        Writes an ethernet packet to a file. Prepends the pcap packet header.
        """
        pcap_len = len(pkt)
        seconds = int(tsSec)
        microseconds = int((tsSec - int(tsSec)) * 1000000)
        pktBytes = struct.pack("<i",seconds) + struct.pack("<i",microseconds) + struct.pack("<i", pcap_len) + struct.pack("<i", pcap_len) + bytes(pkt)
        self.f.write(pktBytes)

    def close(self):
        self.f.close()




if __name__ == '__main__':
    main()
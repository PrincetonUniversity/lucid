#!/usr/bin/env python3
"""
Functions to generate test pcaps
"""
import sys, time, random, socket, os, struct
import threading
import binascii
import dpkt
import subprocess
from collections import namedtuple

sfw_stream_file = 'sfw_test_stream.pcap'
default_pktTmpFile = "sample.pap"

udp_dport = 55000

def main():
    gen_sfw_test_stream(10, 5)
    # generateRandomIpPackets()

def gen_sfw_test_stream(n_pkts, n_installs):
    # generate n_pkts, the first n_installs of which will 
    # induce flow rule installations in the stateful firewall. 
    print ("generating test packet: %s packets from %s flows --> %s"%(n_pkts, n_installs, sfw_stream_file))
    gen = PktGenerator()
    pcapWriter = PcapWriter()
    pcapWriter.create(sfw_stream_file)
    cur_src = 1 # start at 1... why not...
    dst = 666# leave dst empty. It will get filled in by the P4 program.
    pkts = []
    # generate n_installs pkts with different flows
    for i in range(n_installs):
        ethPktStr = gen.makeUdp(ipSrc = cur_src, ipDst = 0, udpDst = udp_dport)
        cur_src += 1
        pkts.append(ethPktStr)
    # generate (n_pkts - n_installs) pkts from the last flow.
    for i in range(n_installs, n_pkts):
        ethPktStr = gen.makeUdp(ipSrc = cur_src, ipDst = 0, udpDst = udp_dport)
        pkts.append(ethPktStr)
    # save the packets
    for pkt in pkts:
        pcapWriter.write(pkt, 0.0)
    pcapWriter.close()



def generateDelayTestPcap(delay=1000, pcapFn=default_pktTmpFile, pktCt = 100):
    rate  = 1
    print ("generating pcap to test delay = %s us"%delay)
    gen = PktGenerator()
    pcapWriter = PcapWriter()
    # fixed ethernet addresses

    pcapWriter.create(pcapFn)
    current_time = 0.0

    for i in range(1, pktCt+1):
        current_time += 1.0 / rate
        # select a flow.
        flowId = i
        pktId =  i
        ethPktStr = gen.makeUdp(ethDst = delay)
        pcapWriter.write(ethPktStr, current_time)
        current_time += 1.0 / rate
    pcapWriter.close()



def generateSequentialUdpPcap(duration = 5, rate = 10000, dumpFile = default_pktTmpFile):
    """
    Generates TCP packets with sequential source and destination IP addresses.
    """
    gen = PktGenerator()
    pcapWriter = PcapWriter()
    # fixed ethernet addresses
    pktCt = rate * duration 

    pcapWriter.create(dumpFile)
    current_time = 0.0

    for i in range(1, pktCt+1):
        current_time += 1.0 / rate
        # select a flow.
        flowId = i
        pktId =  i
        ethPktStr = gen.makeUdp(ipSrc = i, ipDst = i, udpDst = udp_dport)
        pcapWriter.write(ethPktStr, current_time)
        current_time += 1.0 / rate
    pcapWriter.close()

class PktGenerator(object):
    """Generate Packets"""
    EthHdr = namedtuple("EthHdr", "dstAddr srcAddr eType")
    EthHdr.fmt = "!6s6s2s"

    IpHdr = namedtuple("IpHdr", "vihl tos len id fragoff ttl proto csum src dst")
    IpHdr.fmt = "!BBHHHBBH4s4s"

    TcpHdr = namedtuple("TcpHdr", "sport dport seqnum acknum hlFlags wnd csum uptr")
    TcpHdr.fmt = "!HHIIHHHH"

    UdpHdr = namedtuple("UdpHdr", "sport dport len csum")
    UdpHdr.fmt = "!HHHH"

    ReportHdr = namedtuple("ReportHdr", "appid len seqnum timestamp")
    ReportHdr.fmt = "!HHIQ"

    Report = namedtuple("Report", "src_ip dst_ip")
    Report.fmt = "!II"


    def __init__(self):
        return

    cur_seqnum = 0
    def makeReport(self, appid = 1, ipSrc=1, ipDst=2, timestamp = int(0xffffffffffffffff)):
        report = self.Report(ipSrc, ipDst)
        report_bin = struct.pack(report.fmt, *report)
        report_hdr = self.ReportHdr(appid, len(report_bin), self.cur_seqnum, timestamp)
        self.cur_seqnum+=1
        report_hdr_bin = struct.pack(report_hdr.fmt, *report_hdr)
        return (report_hdr_bin + report_bin)

    def makeUdp(self, ethSrc="00:00:00:00:00:00", ethDst="00:00:00:00:00:00", 
        ipSrc=0, ipDst=0, 
        udpSrc=10, udpDst=20,
        totalLen = 128):

        # craft report header + report
        payload = self.makeReport(ipSrc=ipSrc, ipDst=ipDst) 

        ethSrc = fieldToBin(ethSrc, 6)
        ethDst = fieldToBin(ethDst, 6)
        ipSrc = fieldToBin(ipSrc, 4)
        ipDst = fieldToBin(ipDst, 4)
        # craft udp packet.
        udpPkt = dpkt.udp.UDP(sport=udpSrc, dport=udpDst, data = payload)
        udpPkt.ulen = len(bytes(udpPkt))
        udp = bytes(udpPkt)
        # craft ip packet.
        ipPkt = dpkt.ip.IP(v=4, p=dpkt.ip.IP_PROTO_UDP, src=ipSrc, dst=ipDst, hl=5, data = udp)
        ipPkt.len = len(bytes(ipPkt))
        ip = bytes(ipPkt)
        # craft ethernet packet.
        ethPkt = dpkt.ethernet.Ethernet(src=ethSrc, dst=ethDst,type=dpkt.ethernet.ETH_TYPE_IP, data = ip)
        return bytes(ethPkt)

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


def fieldToBin(field, width):
    intToBin = lambda val, fieldWidth : struct.pack("!Q", val)[(struct.calcsize("Q")-fieldWidth):]
    hexStrToBin = lambda ethAddr : binascii.unhexlify(ethAddr.replace(":", ""))
    if (type(field) == str):
        return hexStrToBin(field)
    elif (type(field) == int):
        return intToBin(field, width)

if __name__ == '__main__':
    main()


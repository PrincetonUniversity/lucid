#!/usr/bin/env python3
# import time

import socket, time, os, random, binascii, struct, argparse, signal
from threading import Thread, Event
import pickle as pkl
import dpkt
# import crcmod

parser = argparse.ArgumentParser(description='Parse input arguments')
parser.add_argument('input',  nargs="?", default = None)
parser.add_argument('output', nargs="?", default = None)

def main():
    args = parser.parse_args()
    if None in [args.input, args.output]:
        print ("usage: wire.py <input dpid> <output dpid>")
        return
    print ("wire from {0} --> {1}".format(args.input, args.output))

    # implement a single direction wire from arg1 --> arg2
    print ("wire.py starting.")
    inVeth = "veth%s"%str((int(args.input) * 2) + 1)
    outVeth = "veth%s"%str((int(args.output) * 2) + 1)
    W1 = Wire("wire1", inVeth, outVeth, None)
    print ("wire from {0} --> {1} running. Press ctrl+c or hangup to exit.".format(args.input, args.output))
    signal.pause()



class Endpoint(object):
    """ An endpoint connected to a single veth """
    def __init__(self, name, veth, rxHandler=None):
        self.name = name
        self.pkts = []
        self.veth = veth
        self.rxHandler = rxHandler
        self.rxThread = Thread(target = rxPktLoop, args = (veth, self.getPacket))
        self.rxThread.daemon = True
        self.rxThread.start()


    def getPacket(self, packet):
        self.pkts.append(packet)
        if (self.rxHandler != None):
            self.rxHandler(packet)
        self.sendPacket(packet)

    def sendPacket(self, packet):
        txPkt(self.veth, packet)

    def reset(self):
        self.pkts = []


class Wire(object):
    """ A wire from vethA to vethB """
    def __init__(self, name, vethA, vethB, rxHandler=None):
        self.name = name
        self.pkts = []
        self.vethA = vethA
        self.vethB = vethB
        self.rxHandler = rxHandler
        self.rxThread = Thread(target = rxPktLoop, args = (vethA, self.getPacket))
        self.rxThread.daemon = True
        self.rxThread.start()

    def getPacket(self, packet):
        self.pkts.append(packet)
        if (self.rxHandler != None):
            self.rxHandler(packet)
        self.sendPacket(packet)

    def sendPacket(self, packet):
        txPkt(self.vethA, packet)

    def reset(self):
        self.pkts = []


# =====================================================
# =           Raw RX / TX                             =
# =====================================================

def txPkt(iface, packet):
    # print ("sending packet (%s bytes) out of %s"%(len(pkt), iface))
    s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(0x0003))
    s.bind((iface, 0))
    s.send(packet)
    s.close()

def rxPktLoop(iface, handler, isRecirc=False):
    """
    read packets from interface.
    """
    try:
        s = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, socket.htons(0x0003))
        # s.settimeout(3)
        s.bind((iface, 0))
        pktCt = 0
        while (True):
            packet, addr = s.recvfrom(2048)
            # only handle packets that are _not_ outgoing.
            if (addr[2] != socket.PACKET_OUTGOING):
                handler(packet)
            pktCt += 1
    except socket.timeout:
        print ("socket timed out.") 


if __name__ == '__main__':
    main()
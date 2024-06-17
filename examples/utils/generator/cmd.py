#!/usr/bin/env python3

import sys, time, random, socket, os, struct, json, copy
from dataclasses import dataclass
import binascii
import dpkt
from collections import namedtuple
from scapy.all import sendp, Raw, Ether, sniff
from threading import Thread

iface = "ens1"

#### program-specific handler and generator for test
def handle_report(raw_ev):
    reqid, txct, rxct = struct.unpack('!III', raw_ev[:12])
    print(f"report id = {reqid} txct = {txct} rxct = {rxct}")

def ev_send_pkt(ct, src="00:11:22:33:44:55", dst="07:08:09:10:11:12", et="08:00", data="bb:aa:dd:aa:ff"):
    print(f"generating command to send {ct} packets")
    ct_bytes = unbbytes(ct.to_bytes(2, byteorder='big'))
    # send packet is event number 1
    return (1, [ct_bytes, dst, src, et, data])

# ev_eth is a packet that returned
def ev_eth(src="01:02:03:04:05:06", dst="07:08:09:10:11:12", et="08:00", data="bb:aa:dd:aa:ff"):
    # eth is the default / packet event
    return (None, [dst, src, et, data])

def ev_query(reqid):
    reqid_bytes = unbbytes(reqid.to_bytes(4, byteorder='big'))
    # query is event number 3
    return (3, [reqid_bytes])

def start_flow(flow_id, max_pkts, src="00:11:22:33:44:55", dst="07:08:09:10:11:12", et="08:00", data="bb:aa:dd:aa:ff"):
    flow_id_bytes = unbbytes(flow_id.to_bytes(1, byteorder='big'))
    max_pkts_bytes = unbbytes(max_pkts.to_bytes(4, byteorder='big'))
    return (5, [flow_id_bytes, max_pkts_bytes, dst, src, et, data])

def stop_flow(flow_id):
    flow_id_bytes = unbbytes(flow_id.to_bytes(1, byteorder='big'))
    return (6, [flow_id_bytes])

def reset_counters():
    return (7, [])

def main():
    interface = iface
    if len(sys.argv) < 2:
        print("Usage: ./cmd.py <command> [cmd args]")
        sys.exit(1)
    cmd = sys.argv[1]
    if cmd == "send":
        if (len(sys.argv) > 2):
            generate_port(interface, ev_send_pkt(int(sys.argv[2])))
        else:
            generate_port(interface, ev_send_pkt(1))
    elif cmd == "start":
        generate_port(interface, start_flow(1, (2^32)-1))
    elif cmd == "stop":
        generate_port(interface, stop_flow(1))
    elif cmd == "query":
        global handlers
        global stop 
        stop = False
        handlers["00:02"] = handle_report
        handle_port(interface)
        time.sleep(2)
        generate_port(interface, ev_query(4))
        time.sleep(2)
        stop = True
    elif cmd == "reset":
        generate_port(interface, reset_counters())
    else:
        print("Invalid command")
    return


#### HELPERS 

def bbytes(hex):
    """convert hex string into bytes"""
    return binascii.unhexlify(hex.replace(':', ''))

def unbbytes(bytestring):
    """convert bytes into hex string"""
    hex_str = binascii.hexlify(bytestring).decode()
    return ':'.join(hex_str[i:i+2] for i in range(0, len(hex_str), 2))

def bint(val : int, num_bytes):
    """pack an integer to bytes"""
    return val.to_bytes(num_bytes, byteorder='big')

def generate_port(port_dev : str, ev_args):
    (evnum, args) = ev_args
    # evnum means its a non-packet event
    hdr_fields = []
    if (evnum):
        hdr_fields = [bbytes("00:00:00:00:00:01"), bbytes("00:00:00:00:00:02"), bint(666, 2), bint(evnum, 2)]
    arg_fields = [bbytes(arg) if isinstance(arg, str) else bint(*arg) for arg in args]
    pkt_val = b''.join(hdr_fields + arg_fields)
    pkt = Raw(pkt_val)
    sendp(pkt, iface=port_dev, verbose=0)

stop = False
def start_sniffer(port_dev, fcn):
    # The filter argument can be used to specify a BPF filter. 
    # The 'in' keyword restricts the sniffer to incoming packets.
    global stop
    while (not stop):
        sniff(iface=port_dev, prn=fcn, filter="inbound", store=0, timeout=5)


# handle events from the device
handlers = {}

LUCID_ETY = b'\x02\x9A'
def dispatcher(packet): 
    raw_bytes = bytes(packet)
    if(raw_bytes[12:14] == LUCID_ETY):
        raw_event = raw_bytes[14::]
        event_ty = unbbytes(raw_event[0:2])
        if (event_ty in handlers):
            handlers[event_ty](raw_event[2::])
        else:
            if ("packet" in handlers):
                handlers["packet"](raw_bytes)

def handle_port(port_dev : str):
    # run handlers on port
    print("starting dispatcher on port", port_dev)
    sniffer_thread = Thread(target=start_sniffer, args=(port_dev, dispatcher))
    sniffer_thread.start()
    time.sleep(1)
    return


if __name__ == '__main__':
    main()
#!/usr/bin/env python3

import sys, time, random, socket, os, struct, json, copy
import binascii
import dpkt
from collections import namedtuple
from scapy.all import sendp, Raw, Ether, sniff
from threading import Thread

# the interface of the 
# dataplane from the management cpu
iface = "ens1" # use for real tofino bf100 32x
# iface = "enp5s0"
iface = "veth257" # use for debugging in sim

# event defs
# (this should line up with event numbers in lucid program)
evnums = {
    "send_pkt": 1,
    "query": 3,
}

def rawtime_to_us(rawtime):
    # Converts raw time to microseconds, given that each unit of raw time is 64 microseconds
    return rawtime * 64.0

def rawtime_pps(start, end, pktct):
    start_us = rawtime_to_us(start)
    end_us = rawtime_to_us(end)
    duration_us = end_us - start_us
    if duration_us > 0:
        return (pktct * 10**6) / duration_us
    else:
        return None
    
def handle_report(raw_ev):
    port, reqid, txct, rxct, tx_start, tx_end, rx_start, rx_end = struct.unpack('!HIIIIIII', raw_ev[:30])
    tx_dur_us = rawtime_to_us(tx_end) - rawtime_to_us(tx_start)
    rx_dur_us = rawtime_to_us(rx_end) - rawtime_to_us(rx_start)
    rx_rate = rawtime_pps(rx_start, rx_end, rxct)
    tx_rate = rawtime_pps(tx_start, tx_end, txct)
    print("port = %s" % port)
    print("reqid = %s" % reqid)
    print("tx ct = %s rx ct = %s" % (txct, rxct))
    print("tx dur (us) = %s rx dur (us) = %s" % (tx_dur_us, rx_dur_us))
    print("rx rate (pps) = %s tx rate (pps) = %s" % (rx_rate, tx_rate))
    if rxct > 0:
        loss = 1.0 - (float(rxct) / float(txct))
        print("loss = %s" % loss)
    
def send_pkt(port, ct, src="00:11:22:33:44:55", dst="07:08:09:10:11:12", et="08:00", data="bb:aa:dd:aa:ff"):
    print("generating command to send %s packets" % ct)
    ct_bytes = unbbytes(ct.to_bytes(2, byteorder='big'))
    port_bytes = unbbytes(port.to_bytes(2, byteorder='big'))
    # send packet is event number 1
    return (evnums["send_pkt"], [port_bytes, ct_bytes, dst, src, et, data])

def query(port, reqid):
    reqid_bytes = unbbytes(reqid.to_bytes(4, byteorder='big'))
    port_bytes = unbbytes(port.to_bytes(2, byteorder='big'))
    # query is event number 3
    return (evnums["query"], [port_bytes, reqid_bytes])

def main():
    interface = iface
    if len(sys.argv) < 2:
        print("Usage: ./cmd.py <command> [cmd args]")
        sys.exit(1)
    cmd = sys.argv[1]
    if cmd == "send":
        if (len(sys.argv) == 4):
            # send <port> <ct>
            generate_port(interface, send_pkt(int(sys.argv[2]), int(sys.argv[3])))
        elif (len(sys.argv) == 3):
            # send <port>
            generate_port(interface, send_pkt(int(sys.argv[2]), 1))
        else:
            # send
            generate_port(interface, send_pkt(148, 1))
    elif cmd == "query":
        global handlers
        global stop 
        stop = False
        handlers["00:02"] = handle_report
        handle_port(interface)
        time.sleep(2)
        if (len(sys.argv) == 3):
            # query <port>
            generate_port(interface, query(int(sys.argv[2]), 0))
        else:
            generate_port(interface, query(148, 0))
        time.sleep(2)
        stop = True
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

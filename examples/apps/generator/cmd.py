#!/usr/bin/env python3

# interface to traffic generator -- sends command event packets to the dataplane
# usage: 
# ./cmd.py init <port> <ct> -- initialize a flow on a port to send ct packets
# ./cmd.py start <port> -- start all flows on a port
# ./cmd.py stop <port> -- stop (early) all flows on a port
# ./cmd.py query <port> -- query the status of a port
# notes: 
# sending stops after the flow with the smallest number of packets is finished
# once flows stop, they need to be re-initialized to start again
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
    "init_flow": 1,
    "start": 4,
    "stop": 5,
    "query": 3,
    "report": 2
}

def init_flow(port, ct, src="00:11:22:33:44:55", dst="01:01:01:01:01:01", et="08:00", data="bb:aa:dd:aa:ff"):
    print("generating command to send %s packets" % ct)
    ct_bytes = bytes_to_hex(ct.to_bytes(4, byteorder='big'))
    port_bytes = bytes_to_hex(port.to_bytes(2, byteorder='big'))
    # send packet is event number 1
    return (evnums["init_flow"], [port_bytes, ct_bytes, dst, src, et, data])

def query(port, reqid):
    reqid_bytes = bytes_to_hex(reqid.to_bytes(4, byteorder='big'))
    port_bytes = bytes_to_hex(port.to_bytes(2, byteorder='big'))
    # query is event number 3
    return (evnums["query"], [port_bytes, reqid_bytes])

def start(port):
    port_bytes = bytes_to_hex(port.to_bytes(2, byteorder='big'))
    # start is event number 4
    return (evnums["start"], [port_bytes])

def stop(port):
    port_bytes = bytes_to_hex(port.to_bytes(2, byteorder='big'))
    # stop is event number 5
    return (evnums["stop"], [port_bytes])


def handle_report(raw_ev):
    port, reqid, txct, rxct, tx_start, tx_end, rx_start, rx_end = struct.unpack('!HIIIIIII', raw_ev[:30])
    tx_dur_us = rawtime_to_us(tx_end) - rawtime_to_us(tx_start)
    rx_dur_us = rawtime_to_us(rx_end) - rawtime_to_us(rx_start)
    rx_rate = rawtime_pps(rx_start, rx_end, rxct)
    tx_rate = rawtime_pps(tx_start, tx_end, txct)
    print("port = %s" % port)
    print("reqid = %s" % reqid)
    print("tx ct = %s rx ct = %s" % (txct, rxct))
    print("tx start = %s" % tx_start)
    print("tx dur (us) = %s rx dur (us) = %s" % (tx_dur_us, rx_dur_us))
    print("rx rate (pps) = %s tx rate (pps) = %s" % (rx_rate, tx_rate))
    if rxct > 0:
        loss = 1.0 - (float(rxct) / float(txct))
        print("loss = %s" % loss)
    

def main():
    interface = iface
    if len(sys.argv) < 2:
        print("Usage: ./cmd.py <command> [cmd args]")
        sys.exit(1)
    cmd = sys.argv[1]
    if cmd == "init":
        if (len(sys.argv) == 4):
            # init <port> <ct>
            generate_port(interface, init_flow(int(sys.argv[2]), int(sys.argv[3])))
        elif (len(sys.argv) == 3):
            # init <port>
            generate_port(interface, init_flow(int(sys.argv[2]), 1))
        else:
            # init
            generate_port(interface, init_flow(148, 1))
    elif cmd == "start":
        if (len(sys.argv) == 3):
            # start <port>
            generate_port(interface, start(int(sys.argv[2])))
        else:
            # start
            generate_port(interface, start(148))
    elif cmd == "stop":
        if (len(sys.argv) == 3):
            # stop <port>
            generate_port(interface, stop(int(sys.argv[2])))
        else:
            # stop
            generate_port(interface, stop(148))
    elif cmd == "query":
        handle_port(interface)
        time.sleep(2)
        if (len(sys.argv) == 3):
            # query <port>
            generate_port(interface, query(int(sys.argv[2]), 0))
        else:
            generate_port(interface, query(148, 0))
        time.sleep(2)
        global listener_stop
        listener_stop = True
    else:
        print("Invalid command")
    return


#### HELPERS 

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
    
def hex_to_bytes(hex):
    """convert hex string into bytes"""
    return binascii.unhexlify(hex.replace(':', ''))

def bytes_to_hex(bytestring):
    """convert bytes into hex string"""
    hex_str = binascii.hexlify(bytestring).decode()
    return ':'.join(hex_str[i:i+2] for i in range(0, len(hex_str), 2))

def int_to_bytes(val : int, num_bytes):
    """pack a positive integer to bytes"""
    if val < 0:
        raise ValueError("val must be positive")
    if val.bit_length() > 8*num_bytes:
        raise ValueError("val too large to pack into num_bytes")
    return val.to_bytes(num_bytes, byteorder='big')

def bytes_to_int(bytestring):
    """unpack bytes into integer"""
    return int.from_bytes(bytestring, byteorder='big', signed=False)

### LUCID RUNTIME

# global state: register handlers
handlers = {}
handlers[evnums["report"]] = handle_report

listener_stop = False

# generate port helper
def generate_port(port_dev : str, ev_args):
    (evnum, args) = ev_args
    # evnum means its a non-packet event
    hdr_fields = []
    if (evnum):
        hdr_fields = [hex_to_bytes("00:00:00:00:00:01"), hex_to_bytes("00:00:00:00:00:02"), int_to_bytes(666, 2), int_to_bytes(evnum, 2)]
    arg_fields = [hex_to_bytes(arg) if isinstance(arg, str) else int_to_bytes(*arg) for arg in args]
    pkt_val = b''.join(hdr_fields + arg_fields)
    pkt = Raw(pkt_val)
    sendp(pkt, iface=port_dev, verbose=0)

def start_sniffer(port_dev, fcn):
    # The filter argument can be used to specify a BPF filter. 
    # The 'in' keyword restricts the sniffer to incoming packets.
    global listener_stop
    while (not listener_stop):
        sniff(iface=port_dev, prn=fcn, filter="inbound", store=0, timeout=1)



LUCID_ETY = b'\x02\x9A'
def dispatcher(packet): 
    raw_bytes = bytes(packet)
    if(raw_bytes[12:14] == LUCID_ETY):
        raw_event = raw_bytes[14::]
        event_ty = bytes_to_int(raw_event[0:2])
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

#!/usr/bin/env python3
# Generate the pcap test cases for test_pcap.py into _pcaps/.
# Each test case is an input and output pcap pair: <program>.in.pcap + <program>.expected.pcap. 
# They are both generated from scratch here. 
import struct
import os

from driverlib import PCAPS, write_pcap, lucid_frame

def test_case(name, ins, outs, out_times=None):
    """out_times: the outputs' departure times (ns), when the test checks them"""
    write_pcap(os.path.join(PCAPS, name + ".in.pcap"), ins)
    write_pcap(os.path.join(PCAPS, name + ".expected.pcap"), outs, out_times)

def eth(dst, src, ethertype, payload=b"PAYLOAD-DATA"):
    return bytes.fromhex(dst) + bytes.fromhex(src) + struct.pack(">H", ethertype) + payload

def swap_macs(frame):
    return frame[6:12] + frame[0:6] + frame[12:]

def ethertype(frame):
    return struct.unpack(">H", frame[12:14])[0]

os.makedirs(PCAPS, exist_ok=True)

# ethswaprefl: swap src/dst MACs on every frame (any ethertype).
ins = [
    eth("aabbccddeeff", "112233445566", 0x0800),   # IPv4
    eth("001122334455", "665544332211", 0x86dd),   # IPv6
]
test_case("ethswaprefl", ins, [swap_macs(f) for f in ins])

# ipv4refl: reflect only IPv4 (ethertype 0x0800); drop everything else. Tests
# that the ethertype is compared in network byte order.
ins = [
    eth("aabbccddeeff", "112233445566", 0x0800),   # IPv4  -> reflected
    eth("001122334455", "665544332211", 0x0806),   # ARP   -> dropped
]
test_case("ipv4refl", ins, [swap_macs(f) for f in ins if ethertype(f) == 0x0800])

# bitfields: a 4-bit `ver` + 12-bit `len` packed into the first 2 bytes (len straddles
# the byte boundary, R1' aligned-end), then dst/src/ety. ver=0x5, len=0x123 ->
# byte0 = (5<<4)|(0x123>>8) = 0x51, byte1 = 0x123 & 0xff = 0x23. The handler does
# len+1, so the output has byte1 = 0x24 and byte0 unchanged.
def bitfields_pkt(ver, length, dst, src, ety, payload=b"PAYLOAD-DATA"):
    b0 = ((ver & 0xF) << 4) | ((length >> 8) & 0xF)
    b1 = length & 0xFF
    return bytes([b0, b1]) + bytes.fromhex(dst) + bytes.fromhex(src) + struct.pack(">H", ety) + payload

test_case("bitfields",
    [bitfields_pkt(0x5, 0x123, "aabbccddeeff", "112233445566", 0x0800)],
    [bitfields_pkt(0x5, 0x124, "aabbccddeeff", "112233445566", 0x0800)])

# headeronly: no Payload.t -> the output keeps only the 14-byte eth header, dropping
# the input payload.
ins = [eth("aabbccddeeff", "112233445566", 0x0800)]
test_case("headeronly", ins, [f[:14] for f in ins])

# csum: recompute the IPv4 header checksum (hash<16>(checksum, ...)). The input's
# checksum field is wrong (0xdead); the program zeroes and recomputes it.
def ipv4(ver=4, ihl=5, dscp=0, ecn=0, total_len=32, pkt_id=0x1c46, flags=2, frag=0,
         ttl=64, proto=17, csum=0, src="0a350101", dst="0a350105"):
    return struct.pack(">BBHHHBBH", (ver << 4) | ihl, (dscp << 2) | ecn, total_len,
                       pkt_id, (flags << 13) | frag, ttl, proto, csum) \
        + bytes.fromhex(src) + bytes.fromhex(dst)

def csum_rfc1071(data):
    s = sum(struct.unpack(">%dH" % (len(data) // 2), data))
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    return ~s & 0xFFFF

arp = eth("aabbccddeeff", "112233445566", 0x0806, b"arp-payload.")  # non-IP -> reflected as-is
test_case("csum",
    [eth("aabbccddeeff", "112233445566", 0x0800, ipv4(csum=0xdead) + b"PAYLOAD-DATA"), arp],
    [eth("aabbccddeeff", "112233445566", 0x0800, ipv4(csum=csum_rfc1071(ipv4())) + b"PAYLOAD-DATA"), arp])

# tables_install: Table.install + Table.lookup end-to-end. Event tags are hardcoded from
# tables_install.dpt's declaration order: query_result=1, do_install=2, do_query=3.
test_case("tables_install",
    [lucid_frame(3, 42),            # do_query(42)
     lucid_frame(2, 42, 7),         # do_install(42, 7): hit_acn(7) for key 42, no output
     lucid_frame(3, 42),            # do_query(42)
     lucid_frame(3, 43),            # do_query(43)
     lucid_frame(3, 1)],            # do_query(1)
    [lucid_frame(1, 42, 1000, 0),   # miss before install 
     lucid_frame(1, 42, 7, 1),      # hit after install 
     lucid_frame(1, 43, 1000, 0),   # miss, would hit if an install masked with key
     lucid_frame(1, 1, 1000, 0)])   # miss, other keys shouldn't be affected

# wideints: a 62-bit counter and a 2-bit flag, then a 63-bit counter and a 1-bit
# flag. The handler increments each counter and sets its flag when the result is 0,
# so an increment from the largest value must wrap (be masked to 62 or 63 bits).
def wideints_pkt(a, a_wrapped, b, b_wrapped, payload=b"PAYLOAD-DATA"):
    return (struct.pack(">Q", (a << 2) | a_wrapped)
            + struct.pack(">Q", (b << 1) | b_wrapped) + payload)

test_case("wideints",
    [wideints_pkt(2**62 - 1, 0, 2**63 - 1, 0),    # both wrap
     wideints_pkt(5, 0, 7, 0)],                    # neither wraps
    [wideints_pkt(0, 1, 0, 1),
     wideints_pkt(6, 0, 8, 0)])

# recircport: pkt(7) arriving on port 1 reports port 1, then recirculates bg(7),
# which reports the recirculation port, 196. Tags from recircport.dpt's
# declaration order: pkt=1, bg=2, report=3.
test_case("recircport",
    [lucid_frame(1, 7)],
    [lucid_frame(3, 1, 7),
     lucid_frame(3, 196, 7)])

# delay: ping(0) sends out(100), out(101), out(102) at once from pings 1 ms
# apart, and out(0), out(1), out(2) 2.5 ms after each ping. Tags from
# delay.dpt's declaration order: ping=1, out=2. The pcap driver's virtual clock
# gives exact departure times: with no latency, an undelayed output leaves at its
# event's stamp and a delayed one at the stamp plus its delay.
delay_outs = [lucid_frame(2, x) for x in [100, 101, 102, 0, 1, 2]]
test_case("delay", [lucid_frame(1, 0)], delay_outs,
    [0, 1000000, 2000000, 2500000, 3500000, 4500000])
# delay_latency600: the same run with --latency-ns 600 (test_pcap.py VARIANTS): every
# output leaves at its stamp plus the larger of its delay and 600, so the undelayed
# ones 600 ns later; the delays here are longer than 600, so nothing else moves.
test_case("delay_latency600", [lucid_frame(1, 0)], delay_outs,
    [600, 1000600, 2000600, 2500000, 3500000, 4500000])

print("wrote pcap test cases to", PCAPS)

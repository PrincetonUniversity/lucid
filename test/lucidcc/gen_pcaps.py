#!/usr/bin/env python3
# Generate the pcap fixtures for test/lucidcc/test_pcap.sh into test/lucidcc/pcaps/ (untracked;
# test_pcap.sh runs this first, so it never needs to be run by hand).
#
# Each fixture is <program>.in.pcap + <program>.expected.pcap. The expected output is
# computed here in python from what the program should do, never captured from
# lucidcc's own output -- each fixture is an independent check, not a snapshot.
# A program with no fixture here is compile-only (test_compiler.sh).
import struct
import os

HERE = os.path.dirname(os.path.abspath(__file__))
PCAPS = os.path.join(HERE, "pcaps")

# pcap: little-endian global header, link type 1 (Ethernet), then per-packet
# (16-byte record header + bytes). Wire bytes are big-endian (network order).
# Zero timestamps, matching the pcap driver's output.
def write_pcap(name, pkts):
    with open(os.path.join(PCAPS, name), "wb") as f:
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))
        for p in pkts:
            f.write(struct.pack("<IIII", 0, 0, len(p), len(p)))
            f.write(p)

def fixture(name, ins, outs):
    write_pcap(name + ".in.pcap", ins)
    write_pcap(name + ".expected.pcap", outs)

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
fixture("ethswaprefl", ins, [swap_macs(f) for f in ins])

# ipv4refl: reflect only IPv4 (ethertype 0x0800); drop everything else. Tests
# that the ethertype is compared in network byte order.
ins = [
    eth("aabbccddeeff", "112233445566", 0x0800),   # IPv4  -> reflected
    eth("001122334455", "665544332211", 0x0806),   # ARP   -> dropped
]
fixture("ipv4refl", ins, [swap_macs(f) for f in ins if ethertype(f) == 0x0800])

# bitfields: a 4-bit `ver` + 12-bit `len` packed into the first 2 bytes (len straddles
# the byte boundary, R1' aligned-end), then dst/src/ety. ver=0x5, len=0x123 ->
# byte0 = (5<<4)|(0x123>>8) = 0x51, byte1 = 0x123 & 0xff = 0x23. The handler does
# len+1, so the output has byte1 = 0x24 and byte0 unchanged.
def bitfields_pkt(ver, length, dst, src, ety, payload=b"PAYLOAD-DATA"):
    b0 = ((ver & 0xF) << 4) | ((length >> 8) & 0xF)
    b1 = length & 0xFF
    return bytes([b0, b1]) + bytes.fromhex(dst) + bytes.fromhex(src) + struct.pack(">H", ety) + payload

fixture("bitfields",
    [bitfields_pkt(0x5, 0x123, "aabbccddeeff", "112233445566", 0x0800)],
    [bitfields_pkt(0x5, 0x124, "aabbccddeeff", "112233445566", 0x0800)])

# headeronly: no Payload.t -> the output keeps only the 14-byte eth header, dropping
# the input payload.
ins = [eth("aabbccddeeff", "112233445566", 0x0800)]
fixture("headeronly", ins, [f[:14] for f in ins])

# csum: recompute the IPv4 header checksum (hash<16>(checksum, ...)). The input's
# checksum field is wrong (0xdead); the program zeroes and recomputes it over the
# wire-format header bits. The expected checksum is RFC 1071, computed here.
def ipv4(ver=4, ihl=5, dscp=0, ecn=0, total_len=32, pkt_id=0x1c46, flags=2, frag=0,
         ttl=64, proto=17, csum=0, src="0a350101", dst="0a350105"):
    return struct.pack(">BBHHHBBH", (ver << 4) | ihl, (dscp << 2) | ecn, total_len,
                       pkt_id, (flags << 13) | frag, ttl, proto, csum) \
        + bytes.fromhex(src) + bytes.fromhex(dst)

def rfc1071(data):
    s = sum(struct.unpack(">%dH" % (len(data) // 2), data))
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    return ~s & 0xFFFF

arp = eth("aabbccddeeff", "112233445566", 0x0806, b"arp-payload.")  # non-IP -> reflected as-is
fixture("csum",
    [eth("aabbccddeeff", "112233445566", 0x0800, ipv4(csum=0xdead) + b"PAYLOAD-DATA"), arp],
    [eth("aabbccddeeff", "112233445566", 0x0800, ipv4(csum=rfc1071(ipv4())) + b"PAYLOAD-DATA"), arp])

# ---- Lucid background-event framing: dst=1 ++ src=2 ++ ety 666 ++ 16-bit tag ++ fields --
def lucid_frame(tag, *fields):
    return (bytes.fromhex("000000000001") + bytes.fromhex("000000000002")
            + struct.pack(">HH", 666, tag) + b"".join(struct.pack(">I", f) for f in fields))

# tables1_pcap: Table.install + Table.lookup end-to-end. Event tags are hardcoded from
# tables1_pcap.dpt's declaration order: query_result=1, do_install=2, do_query=3.
# Expected query_result(key, val, hit) frames on port 1:
#   (42, 1000, 0)  miss before install (default miss_acn echoes the lookup arg)
#   (42, 7, 1)     hit after install
#   (43, 1000, 0)  miss -- 43 & 42 == 42, catches an install that masks with the key
#                  instead of all-ones
#   (1, 1000, 0)   miss -- other keys unaffected
fixture("tables1_pcap",
    [lucid_frame(3, 42),      # do_query(42)
     lucid_frame(2, 42, 7),   # do_install(42, 7): hit_acn(7) for key 42, no output
     lucid_frame(3, 42),      # do_query(42)
     lucid_frame(3, 43),      # do_query(43)
     lucid_frame(3, 1)],      # do_query(1)
    [lucid_frame(1, 42, 1000, 0),
     lucid_frame(1, 42, 7, 1),
     lucid_frame(1, 43, 1000, 0),
     lucid_frame(1, 1, 1000, 0)])

print("wrote pcap fixtures to", PCAPS)

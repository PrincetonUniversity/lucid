#!/usr/bin/env python3
# Generate the input pcaps for the C-backend run tests (ctests/run_c_tests.sh).
# One <program>.in.pcap per program that has a runtime test. Re-run this whenever
# an input changes; then `run_c_tests.sh --update` to refresh the expected output.
#
# A program with no <program>.in.pcap here is compile-only in run_c_tests.sh.
import struct
import os

HERE = os.path.dirname(os.path.abspath(__file__))

# pcap: little-endian global header, link type 1 (Ethernet), then per-packet
# (16-byte record header + bytes). Wire bytes are big-endian (network order).
def write_pcap(name, pkts):
    with open(os.path.join(HERE, name), "wb") as f:
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))
        for p in pkts:
            f.write(struct.pack("<IIII", 0, 0, len(p), len(p)))
            f.write(p)

def eth(dst, src, ethertype, payload=b"PAYLOAD-DATA"):
    return bytes.fromhex(dst) + bytes.fromhex(src) + struct.pack(">H", ethertype) + payload

# ethswaprefl: swap src/dst MACs on every frame (any ethertype).
write_pcap("ethswaprefl.in.pcap", [
    eth("aabbccddeeff", "112233445566", 0x0800),   # IPv4
    eth("001122334455", "665544332211", 0x86dd),   # IPv6
])

# ipv4refl: reflect only IPv4 (ethertype 0x0800); drop everything else. Tests
# that the ethertype is compared in network byte order.
write_pcap("ipv4refl.in.pcap", [
    eth("aabbccddeeff", "112233445566", 0x0800),   # IPv4  -> reflected
    eth("001122334455", "665544332211", 0x0806),   # ARP   -> dropped
])

# bitfields: a 4-bit `ver` + 12-bit `len` packed into the first 2 bytes (len straddles
# the byte boundary, R1' aligned-end), then dst/src/ety. ver=0x5, len=0x123 ->
# byte0 = (5<<4)|(0x123>>8) = 0x51, byte1 = 0x123 & 0xff = 0x23. The handler does len+1,
# so the expected output has byte1 = 0x24 and byte0 unchanged.
def bitfields_pkt(ver, length, dst, src, ety, payload=b"PAYLOAD-DATA"):
    b0 = ((ver & 0xF) << 4) | ((length >> 8) & 0xF)
    b1 = length & 0xFF
    return bytes([b0, b1]) + bytes.fromhex(dst) + bytes.fromhex(src) + struct.pack(">H", ety) + payload

write_pcap("bitfields.in.pcap", [
    bitfields_pkt(0x5, 0x123, "aabbccddeeff", "112233445566", 0x0800),
])

# headeronly: no Payload.t -> the output keeps only the 14-byte eth header, dropping the
# input payload. Same input as ethswaprefl; the expected output is just the header.
write_pcap("headeronly.in.pcap", [
    eth("aabbccddeeff", "112233445566", 0x0800),
])

print("wrote input pcaps to", HERE)
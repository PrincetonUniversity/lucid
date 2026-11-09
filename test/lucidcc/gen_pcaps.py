#!/usr/bin/env python3
# Generate the pcap driver's test cases for test_pcap.py into _pcaps/: the pcap
# driver's virtual clock, which gives exact departure times. Each test case is an
# input pcap and an expected-output pcap with times: <name>.in.pcap +
# <name>.expected.pcap. Programs' behavior is tested by test/ltf; these cases
# check what only this driver does.
import glob
import os

from driverlib import PCAPS, write_pcap, lucid_frame

def test_case(name, ins, outs, out_times=None):
    """out_times: the outputs' departure times (ns), when the test checks them"""
    write_pcap(os.path.join(PCAPS, name + ".in.pcap"), ins)
    write_pcap(os.path.join(PCAPS, name + ".expected.pcap"), outs, out_times)

# start clean: test_pcap.py runs every case it finds in _pcaps/
os.makedirs(PCAPS, exist_ok=True)
for f in glob.glob(os.path.join(PCAPS, "*.pcap")):
    os.remove(f)

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

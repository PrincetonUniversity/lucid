This directory contains an example of using the Lucid interpreter as a switch operating on real network devices (the `lucidSwitch` binary).
`lucidSwitch` has been tested on macos 14.1 and ubuntu 24.04. 

Please see `test_reflector.py` for a simple usage example. This script: 

1. creates a veth pair (or a "feth" pair on macos);
2. constructs a test pcap
3. spawns the lucid softswitch running "reflector.dpt" in this directory
4. runs the test pcap through the softswitch
5. compares output packets to the original test pcap for validation
6. reports throughput

Here is an example run on macos:

```bash
(base) johnsonchack@Johns-MBP-2 lucidvswitch % ./test_reflector.py
[+] Removed old pcap: /Users/johnsonchack/Desktop/gits/lucid/examples/features/lucidvswitch/send.pcap
[+] Removed old pcap: /Users/johnsonchack/Desktop/gits/lucid/examples/features/lucidvswitch/recv.pcap
[+] Wrote 10000 packets to /Users/johnsonchack/Desktop/gits/lucid/examples/features/lucidvswitch/send.pcap
[+] feth0 and feth1 are up
[+] Started tcpdump on feth1, waiting for switch to initialize...
[+] Switch initialized
[+] Sent 10000 packets on feth1
[*] Sent: 10000 packets, Received: 10000 packets
[+] PASS: packet counts match
[*] Throughput: 251743 pps, 2062.49 Mbps (over 0.0397s)
```

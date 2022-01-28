### Running a lucid program on a physical testbed

This note walks through compiling and running the program `wire.dpt` on the Lucid testbed. 


#### Topology

Here are the servers in the lucid testbed.

**jc4**
- ubuntu 21.10
- 20 cores, 256GB ram
- 1 40Gb NIC, enp5s0f0 connected to dpid 136 on tofino

**jc5**
- ubuntu 18.04 (feel free to upgrade!)
- 8 cores, 32GB ram
- 2 10Gb NICs: 
    - enp3s0f0 (connected to dpid 128)
    - enp3s0f1 (connected to dpid 129)

**jc6**
- ubuntu 20.04 (feel free to upgrade!)
- 8 cores, 32GB ram
- 2 10Gb NICs: 
    - enp3s0f0 (connected to dpid 130)
    - enp3s0f1 (connected to dpid 131)

If you are not on the Lucid testbed, you can follow along with the example by connecting your tofino to machines on ports 128 and 130 with 10G cables.

### Running the program

Here is how to run `wire.dpt` and perform an iperf between the two hosts it connects.

1. on your local machine, compile `wire.dpt` from lucid to P4. In this directory, run
```make compile```

2. copy the build directory, `wire_build` to the tofino. We'll put it in `~jsonch/lucid_examples/`, but it can go anywhere. 

Here's what you should see on the tofino:
```
jsonch@localhost:~/lucid_examples/wire_build$ pwd
/home/jsonch/lucid_examples/wire_build
jsonch@localhost:~/lucid_examples/wire_build$ ls
libs  logs  lucid.cpp  lucid.p4  lucid.py  makefile  scripts  src
```

3. on the tofino, compile the application to P4 by going into the build directory and running `make build`.

```
jsonch@localhost:~/lucid_examples/wire_build$ make build
**** compiling lucid.p4 to lucid ****
... compiler output elided ...
**** done compiling lucid.cpp to lucid/bf_switchd ****
```

Note: you can also do this step on the lucid VM, before you copy the build directory over.

4. On the tofino, go into the build directory and run `make hw` to start up the p4 program and the control script. The control script automatically brings up all the ports for the testbed machines. 

```
jsonch@localhost:~/lucid_examples/wire_build$ make hw
running on: /home/jsonch/lucid_examples/wire_build/lucid/lucid.conf
...output elided...
[PY_MGR] bringing port 128 up
[PY_MGR] bringing port 129 up
[PY_MGR] bringing port 130 up
[PY_MGR] bringing port 131 up
[PY_MGR] bringing port 136 up
[PY_MGR] mgr.py disconnect complete.
**** switchd running -- press ctrl+c to terminate. ****
```
Note at the end where the control script brings the ports up for all the machines. 

5. In another window, log into the jc5 and jc6 machines. Give the interfaces connected to the Tofino IP addresses on the same subnet. **WARNING: MAKE SURE THE IP ADDRESSES ARE NOT IN THE SUBNET OF ANY INTERFACES NOT CONNECTED TO THE TOFINO!**

```
jsonch@jc5:~$ sudo ifconfig enp3s0f0 1.0.0.5 netmask 255.255.255.0
jsonch@jc5:~$ sudo ifconfig enp3s0f0
enp3s0f0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 1.0.0.5  netmask 255.255.255.0  broadcast 1.0.0.255
        inet6 fe80::d250:99ff:fee8:28cb  prefixlen 64  scopeid 0x20<link>
        ether d0:50:99:e8:28:cb  txqueuelen 1000  (Ethernet)
        RX packets 1  bytes 86 (86.0 B)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 30  bytes 2516 (2.5 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

```
```
jsonch@jc6:~$  sudo ifconfig enp3s0f0 1.0.0.6 netmask 255.255.255.0
jsonch@jc6:~$ ifconfig enp3s0f0
enp3s0f0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 1.0.0.6  netmask 255.255.255.0  broadcast 1.0.0.255
        inet6 fe80::d250:99ff:fee8:28c9  prefixlen 64  scopeid 0x20<link>
        ether d0:50:99:e8:28:c9  txqueuelen 1000  (Ethernet)
        RX packets 1  bytes 86 (86.0 B)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 25  bytes 2086 (2.0 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
```

6. At this point, jc5 and jc6 should be able to communicate. The last step is to add static arp entries on the hosts. (This is necessary because Lucid only handles IP packets correctly. ARP packets will currently get messed up.)

```
jsonch@jc6:~$ sudo arp -s 1.0.0.5 d0:50:99:e8:28:cb
jsonch@jc5:~$ sudo arp -s 1.0.0.6 d0:50:99:e8:28:c9
```

7. Finally, you can use ping, iperf, and any other tcp / udp applications to communicate between the hosts.

```
jsonch@jc5:~$ iperf -s
------------------------------------------------------------
Server listening on TCP port 5001
TCP window size:  128 KByte (default)
------------------------------------------------------------
[  4] local 1.0.0.5 port 5001 connected with 1.0.0.6 port 38748
[ ID] Interval       Transfer     Bandwidth
[  4]  0.0-10.0 sec  11.0 GBytes  9.41 Gbits/sec

jsonch@jc6:~$ iperf -c 1.0.0.5
------------------------------------------------------------
Client connecting to 1.0.0.5, TCP port 5001
TCP window size: 2.66 MByte (default)
------------------------------------------------------------
[  3] local 1.0.0.6 port 38748 connected with 1.0.0.5 port 5001
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0-10.0 sec  11.0 GBytes  9.41 Gbits/sec
```

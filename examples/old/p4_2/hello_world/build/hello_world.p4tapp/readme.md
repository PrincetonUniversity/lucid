### prog.p4tapp

Harness for runtime benchmarks of dpt apps on tofino. 

build: 
``make``

run data + control script in Tofino simulator: 
``jsonch@ubuntu:~/dpt/switchexp/prog.p4tapp$ `./find_p4tapp.sh` . sim -r``

start a test (in a separate window):

``jsonch@ubuntu:~/dpt/switchexp/prog.p4tapp$ `./find_p4tapp.sh` . sim -t``

- this will start a loopback wire script on veth3 (dpid 2)
- it will also send packets in on veth 1 (dpid 1)


to run on hardware: 

`./find_p4tapp.sh` . hw -r

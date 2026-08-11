## Branch description

This branch extends Lucid’s interpreter to support packet IO from standard network interfaces (e.g., in Linux, BSD). This will make it easy and safe to run Lucid programs on many platforms at 1-5Gb/s rates.

Milestone(s)

a. 	Integrate library for packet RX/TX from raw sockets.

b. 	Convert raw packets to internal representations.

c. 	Implement abstraction layer to map port identifiers to interfaces.

d. 	Testing and documentation.

### Overview of changes

The relevant changes for the above milestones are all on this branch -- [26.2.interp-io](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io). We will merge them into main after review. 

**Major changes**
1. Library for interpreter IO from sockets / interfaces. **(milestone a+b)**  
2. New interpreter-based virtual Lucid switch that processes packets from sockets in real time. **(milestone a+b)**  
3. New node-based interpreter topology configuration. **(milestone c)**  
4. 12 new multi-node interpreter examples with test cases and documentation, ported from the commonly-referenced P4 tutorials. **(milestone c+d)**  
5. Support for generic events, which simplify the above examples. **(milestone d)**  
6. Major interpreter and frontend refactoring, and interpreter performance improvements. **(all milestones)**

### Instructions for testing

1. clone the repo; cd in

```
git clone https://github.com/princetonuniversity/lucid
cd lucid
```
2. switch to interpreter improvements branch

```
git checkout 26.2.interp-io
```
3. build or pull the lucid dev docker container. Build may take ~20 minutes to build ocaml + z3 + etc


```
./docker/dev/dockercmd.sh build
```
or 

```
./docker/dev/dockercmd.sh pull
```
4. Spawn and enter the container, build lucid interpreter 
(note the path argument at the end that mounts the repo in the container) 

```
./docker/dev/dockercmd.sh enter ./
cd lucid
make
```

5. Test the new interpreter-based Lucid switch on veth interfaces

There is a simple reflector program, reflector.dpt, and a python script that starts it on the interpreter, sends packets in with tcpreplay, and measures output rate. Try them in the lucid dev container:

```
cd lucid
cd examples/features/lucidvswitch/
python3 test_reflector.py
```

The output should be something like: 

```
[+] Removed old pcap: /home/ubuntu/lucid/examples/features/lucidvswitch/send.pcap
[+] Removed old pcap: /home/ubuntu/lucid/examples/features/lucidvswitch/recv.pcap
[+] Wrote 10000 packets to /home/ubuntu/lucid/examples/features/lucidvswitch/send.pcap
[+] feth0 and feth1 are up
[+] Started tcpdump on feth1, waiting for switch to initialize...
[+] Switch initialized
[+] Sent 10000 packets on feth1
[*] Sent: 10000 packets, Received: 5102 packets
[-] FAIL: packet counts do not match
[*] Throughput: 125705 pps, 1029.98 Mbps (over 0.0406s)
```
Note: packet drops will probably happen because the test script just replays at a high throughput.

6. Test the new interpreter topology configuration with the examples ported from P4 BMv2. We chose these examples because many of them were focused on multi-node programs, which is also the point of topology configuration in the Lucid interpreter.
From the repo root inside the dev container, run:
```
cd examples/p4_bmv2_examples/
python3 test.py 
```

The output should look like: 
```
Running 11 example test(s):
  PASS     basic
  PASS     basic_tunnel
  PASS     calc
  PASS     ecn
  PASS     flowcache
  PASS     link_monitor
  PASS     load_balance
  PASS     mri
  PASS     multicast
  PASS     qos
  PASS     source_routing

11 passed, 0 failed, 11 total
```
Each example is inside its own directory in "p4_bmv2_examples", with a little readme and some helpers to construct the topology.


### More details on changes and new features

Everything described here is exercised in the testing instructions above, this is just extra info.

1. Added interpreter IO from sockets / interfaces. **(milestone a+b)**  
- Integrated the rawlink library for ocaml raw sockets ([https://opam.ocaml.org/packages/rawlink/](https://opam.ocaml.org/packages/rawlink/))   
- Added custom wrapper and I/O connectors to interpreter’s event loop  
- Code references:  
  - Vendored rawlink lib: [https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/vendor/rawlink](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/vendor/rawlink)   
  - Rawlink wrapper: [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpSocket.ml](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpSocket.ml)  
  - Integration with Rawlink wrapper at various points in interpreter: [https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/src/lib/midend/interpreter](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/src/lib/midend/interpreter)   
2. New interpreter-based Lucid switch that processes packets from sockets in real time. Benchmarks on an M3 macbook pro for a simple program are around 1Gbps.  **(milestone a+b+d)**  
- Code references:   
  - lucidSwitch binary: [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/bin/lucidSwitch.ml](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/bin/lucidSwitch.ml) (short, but relies on new code paths in interpreter backend)  
  - lucidSwitch test / benchmark example: [https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/features/lucidvswitch](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/features/lucidvswitch) 

3. New node-based interpreter topology configuration. **(milestone c)**  
- This allows the user to define a simulated multi-node (i.e., multi-switch) topology to run the interpreter on by declaring the configuration of each node, then the topology of links connecting the nodes. The implementation formalizes the config options as OCaml datatypes and will be extensible, e.g., to support simulations where different nodes run different Lucid programs.   
- Code references:  
  - Internal representation of interpreter network topologies: [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpTopo.ml](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpTopo.ml)  
  - A simple example: [https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/features/topology\_configs](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/features/topology_configs)   
4. Added 12 new multi-node interpreter examples, from BMv2 tutorial, with test cases and documentation. **(milestone c+d)**  
- [https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/p4\_bmv2\_examples](https://github.com/PrincetonUniversity/lucid/tree/26.2.interp-io/examples/p4_bmv2_examples)   
5. To better support the above examples, we added generic events **(milestone d)**  
- This involved completing two language features that were previously partially implemented: polymorphic event arguments and tuples.  
- Together, they let Lucid programs define generic events and handlers, e.g., an IP packet handler that is generic with respect to the type of the underlay network, or a source routing handler that is generic with respect to the length of the source routing header’s tail.  
- Generic events are used in several of the new multi-node interpreter examples, e.g., source routing (the “auto” parameter is polymorphic and allows the programmer to write 1 event and handler regardless of how many records are in the sr\_tail header): [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/examples/p4\_bmv2\_examples/source\_routing/source\_routing.dpt\#L86](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/examples/p4_bmv2_examples/source_routing/source_routing.dpt#L86)  
- Code references:   
  - New code is interleaved in frontend, start from tuple construction in the parser: [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/frontend/Parser.mly\#L345](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/frontend/Parser.mly#L345) , and trace through the frontend pipeline up to the point where tuples are eliminated [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/frontend/FrontendPipeline.ml\#L119](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/frontend/FrontendPipeline.ml#L119)   
6. Interpreter and frontend refactoring / technical debt cleanup **(milestones c \+ d)**  
- The interpreter was refactored from a monolithic architecture into “switch” and “network” modules. This makes the interpreter’s code structure match the computation and communication model of Lucid, and also improves the interpreter’s extensibility / maintainability.  
  - Code references:  
    - InterpSwitch and interpNetwork:  
      - [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpSwitch.ml](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpSwitch.ml)   
      - [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpNetwork.ml](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/src/lib/midend/interpreter/InterpNetwork.ml)   
    - Interpreter architecture overview: [https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/docs/interp-arch.md](https://github.com/PrincetonUniversity/lucid/blob/26.2.interp-io/docs/interp-arch.md)   
- The frontend was refactored to remove \~1K LoC related to match-action tables, which were previously hard-coded into Lucid’s AST but now, with tuples, can be represented as a “builtin library” similar to arrays.  
  - Most changes here are concentrated into this commit: [https://github.com/PrincetonUniversity/lucid/commit/54a179834ea6c4890b2f48c93dd280e0d4d8a163](https://github.com/PrincetonUniversity/lucid/commit/54a179834ea6c4890b2f48c93dd280e0d4d8a163) 

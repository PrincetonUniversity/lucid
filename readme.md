# The Lucid / DPT (data plane threads) language

Lucid is a processing / data plane programming language that focuses on simple, general, and modular abstractions. This makes it easier to express a range of data-plane algorithms and data structures, like specialized hash tables (e.g., cuckoo hashing), sketches, and custom telemetry caches. Programs are often 10X fewer lines of code in Lucid compared to P4, and read much more like Go, Python, or C, than equivalent implementations in P4. 

Lucid also has a type-and-effect system that guarantees the ordering of operations on global state (state that persists across packets). Programs that pass Lucid's ordering check can be laid out as a pipeline (important for targets like the Tofino) and also have a convenient memory model: each packet's updates to all the global state can be viewed as an atomic transaction, and a stream of packets can be viewed as a serial sequence of such atomic transactions that executes in the order of packet arrivals. In other words, you don't need to worry about concurrency or race conditions at the packet level for Lucid programs that pass the ordering check. 

There are 3 implementations of Lucid: 

1. The Lucid interpreter. This defines the semantics of the language in a target-independent way. It is relatively fast and works on simple json events, either from a file given at startup or piped from stdin.

2. The Lucid-Tofino compiler. This backend compiles a Lucid program to a p416 program for the Tofino 1. It does many target-specific optimizations drawn from our many years of programming the Tofino.
  
3. A DPDK-C compiler. This backend produces a C program that uses DPDK for packet IO. It is a work in progress and currently single threaded. 

## Installation

The easiest way to run Lucid is with the pre-built binaries in the `release` directory. 
This should work for recent macos and ubuntu/debian systems.
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./release/unpack.sh
./release/dpt.sh -h
```

Note: there is only a pre-built binary for the Interpreter at this time.

### Docker

There is also a docker image that can run the Lucid interpreter and Tofino compiler. 

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository and pull the lucid docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./docker_lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

Once finished, you can run `./docker_lucid.sh interpret` to run the interpreter or `./docker_lucid.sh compile` to run the Tofino compiler. The `docker_lucid` script takes care of forwarding all arguments, files, and directories to / from the docker image.

### Building from source

Finally, you can also build Lucid from source. Its main dependencies are ocaml and z3. 
On macos or linux, you should be able to do: 
```
./install dependencies
make
```
to build the Lucid interpreter (`dpt`) and tofino compiler (`dptc`).


## Run the interpreter

Using the docker container, you can run the interpreter with `./docker_lucid.sh interpret <lucid program name>`. The interpreter type checks your program, then runs it in a simulated network defined by a specification file. 

Try it out with one of the tutorial programs ([monitor.dpt](https://github.com/PrincetonUniversity/lucid/blob/main/tutorials/interp/01monitor/monitor.dpt)):

```
jsonch@jsonchs-MBP lucid % ./docker_lucid.sh interp tutorials/interp/01monitor/monitor.dpt
# ... startup output elided ...
t=0: Handling event eth_ip(11,22,2048,1,2,128) at switch 0, port 0
t=600: Handling event prepare_report(11,22,2048,1,2,128) at switch 0, port 196
sending report about packet {src=1; dst=2; len=128} to monitor on port 2
dpt: Final State:

Switch 0 : {

 Pipeline : [ ]

 Events :   [ ]

 Exits :    [
    eth_ip(11,22,2048,1,2,128) at port 1, t=600
    report(1,2,128) at port 2, t=1200
  ]

 Drops :    [ ]

 packet events handled: 0
 total events handled: 2

}
```

### Run the compiler

Finally, to compile Lucid programs to P4 for the Intel tofino, run the compiler with `./docker_lucid.sh compile <lucid program name> -o <build directory name>`.

The compiler translates a Lucid program into P4, optimizes it, and produces a build directory with a P4 program, Python control plane, and a directory that maps control-plane modifiable Lucid object names to their associated P4 object names. 
Try it out with the tutorial application: 

```
jsonch@jsonchs-MBP lucid % ./docker_lucid.sh compile tutorials/interp/01monitor/monitor.dpt -o monitor_build
build dir: monitor_build
build dir: /Users/jsonch/Desktop/gits/lucid/monitor_build
# ... output elided ...
[coreLayout] placing 41 atomic statement groups into pipeline
.........................................
[coreLayout] final pipeline
--- 41 IR statements in 3 physical tables across 2 stages ---
stage 0 -- 2 tables: [(branches: 3, IR statements: 25, statements: 25),(branches: 4, IR statements: 13, statements: 13)]
stage 1 -- 1 tables: [(branches: 4, IR statements: 3, statements: 3)]
Tofino backend: -------Layout for egress: wrapping table branches in functions-------
Tofino backend: -------Layout for egress: deduplicating table branch functions-------
Tofino backend: -------Translating to final P4-tofino-lite IR-------
Tofino backend: -------generating python event parsing library-------
Tofino backend: -------generating Lucid name => P4 name directory-------
Tofino backend: -------printing P4 program to string-------
Tofino backend: -------printing Python control plane to string-------
compiler: Compilation to P4 finished. Writing to build directory:/app/build
local p4 build is in: /Users/jsonch/Desktop/gits/lucid/monitor_build
jsonch@jsonchs-MBP lucid % ls monitor_build
eventlib.py     libs            lucid.p4        manifest.txt
globals.json    logs            lucid.py        scripts
layout_info.txt lucid.cpp       makefile        src
jsonch@jsonchs-MBP lucid % cat monitor_build/manifest.txt 
Lucid-generated tofino project folderContents: 
lucid.p4 -- P4 data plane program
lucid.py -- Python script to install multicast rules after starting lucid.p4
eventlib.py -- Python event parsing library
globals.json -- Globals name directory (maps lucid global variable names to names in compiled P4)
makefile -- simple makefile to build and run P4 program
lucid.cpp -- c control plane (currently unused)
```


### What to look at next

There are many [example programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples) in our test suite, including some for the [Tofino](https://github.com/PrincetonUniversity/lucid/tree/main/examples/tofino_apps). 

Lucid has been used to implement a number of interesting [applications](https://github.com/PrincetonUniversity/lucid/tree/main/examples/apps). 

Lucid's [wiki](https://github.com/PrincetonUniversity/lucid/wiki) documents all of Lucid's well-supported features. 

There are also a number of publications about Lucid, listed below.

## Publications

### About Lucid or its components:

- Lucid, a Language for Control in the Data Plane ([SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html) -- The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch).

- Safe, modular packet pipeline programming ([POPL 2022](https://dl.acm.org/doi/pdf/10.1145/3498699))

- Lucid, a high-level easy-to-use dataplane programming language ([Devon K. Loehr's PhD Thesis](https://dkloehr.github.io/files/Thesis.pdf))

- Automated Optimiation of Parameterized Data-Plane Programs with Parasol ([arxiv preprint](https://arxiv.org/pdf/2402.11155))

### Using Lucid: 

- SwitchLog: A Logic Programming Language for Network Switches ([PADL 2023](https://par.nsf.gov/servlets/purl/10430321))


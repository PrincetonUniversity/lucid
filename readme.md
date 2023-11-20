# The Lucid / DPT (data plane threads) language

Lucid is a language for programming the Intel Tofino, and eventually other data plane platforms. Compared to P4, Lucid provides higher-level abstractions, a specialized type system, a relatively fast (and semantically accurate) software interpreter, and an optimizing compiler. All of these features are designed to make it easier to prototype and develop complex data plane applications that compile to real hardware. 

Lucid is especially useful for programs that feature *complex, user-defined data structures* (e.g., data-plane writeable cuckoo hash tables, approximate data structures), *packet recirculation*, or *integrated network control*.

## Getting Started

The easiest way to use Lucid is with the Lucid docker image.

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository and pull the lucid docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

That's it! Once the pull is done, you are ready to run the Lucid interpreter and P4 compiler inside the lucid docker container. The `lucid.sh` script makes this easy.

### Run the interpreter

Run the interpreter with `./lucid.sh interpret <lucid program name>`. The interpreter type checks your program, then runs it in a simulated network defined by a specification file. 

Try it out with one of the tutorial programs ([monitor.dpt](https://github.com/PrincetonUniversity/lucid/blob/main/tutorials/interp/01monitor/monitor.dpt)):

```
jsonch@jsonchs-MBP lucid % ./lucid.sh interp tutorials/interp/01monitor/monitor.dpt
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

Finally, to compile Lucid programs to P4 for the Intel tofino, run the compiler with `./lucid.sh compile <lucid program name> -o <build directory name>`.

The compiler translates a Lucid program into P4, optimizes it, and produces a build directory with a P4 program, Python control plane, and a directory that maps control-plane modifiable Lucid object names to their associated P4 object names. 
Try it out with the tutorial application: 

```
jsonch@jsonchs-MBP lucid % ./lucid.sh compile tutorials/interp/01monitor/monitor.dpt -o monitor_build
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


### What to do next

Lucid is a simple language. You might want to just play around with some [examples](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests) in our interpreter test suite, or some [tofino examples](https://github.com/PrincetonUniversity/lucid/tree/main/examples/tofino_apps) that we use to test the tofino compiler. 
Alternately, you can take a look at the [tutorials](tutorials/readme.md).


## More details
For more information about Lucid, check out our paper and talk at [SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html)

The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch.

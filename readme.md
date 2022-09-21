# The Lucid / DPT (data plane threads) language

Lucid is a language for programming the Intel Tofino, and eventually other data plane platforms. Compared to P4, Lucid provides higher-level abstractions, a specialized type system, a relatively fast (and semantically accurate) software interpreter, and an optimizing compiler. All of these features are designed to make it easier to prototype and develop complex data plane applications that compile to real hardware. 

Lucid is especially useful for programs that feature *complex, user-defined data structures* (e.g., data-plane writeable cuckoo hash tables, approximate data structures), *packet recirculation*, or *integrated network control*.

## Getting Started

The easiest way to use Lucid is with the Lucid docker image.

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository and pull the lucid_production docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

That's it! Once the pull is done, you are ready to run the Lucid interpreter and P4 compiler inside the `lucid_production` docker container. The `lucid.sh` script makes this easy.

### Run the interpreter

Run the interpreter with `./lucid.sh interpret <lucid program name>`. The interpreter type checks your program, then runs it in a simulated network defined by a specification file. 

Try it out with the tutorial program, `histogram.dpt`:
```
% ./lucid.sh interpret examples/tutorial/histogram.dpt 
running command:docker run --rm -it -v /Users/jsonch/Desktop/gits/lucid/examples/tutorial/histogram.dpt:/app/inputs/histogram.dpt -v /Users/jsonch/Desktop/gits/lucid/examples/tutorial/histogram.json:/app/inputs/histogram.json jsonch/lucid:lucid /bin/sh -c "./dpt /app/inputs/histogram.dpt --spec /app/inputs/histogram.json"
dpt: -------Checking well-formedness---------
# ... type checker output elided ...
dpt: Simulating...
# ... interpreter output elided ...
t=32400: Handling event report(3) at switch 0, port 196
t=40000: Handling entry event ip_in(128,5,2,768,0) at switch 0, port 0
dpt: Final State:
# ... interpreter output elided ...
 entry events handled: 5
 total events handled: 9

}
``` 

### Run the compiler

Finally, to compile Lucid programs to P4, run the compiler with `./lucid.sh compile <lucid program name>`.

The compiler translates a Lucid program into P4, optimizes it, and produces a build directory with a P4 program, Python control plane, and helper scripts to make deployment easier. 

Try it out with a simple application that bounces packets back to their ingress port:

```
% ./lucid.sh compile examples/tofino_apps/src/reflector.dpt                            
compiler: Compilation to P4 started...
...
% ls reflector_build                                       
libs           logs           lucid.cpp      lucid.p4       lucid.py       makefile       num_stages.txt scripts        src
```

### What to do next

Ultimately, Lucid is simple language -- by design. The best way to get a feel for Lucid is to play around with the examples (in `/examples`) or check out the tutorials that introduce the Lucid [language](docs/tutorial_language.md), [interpreter](docs/tutorial_interpreter.md), and [p4-tofino compiler](docs/tutorial_compiler.md).

**Warning: the tutorials are currently (9/22) being updated. The P4-tofino compiler tutorial is particularly out of date.**


## More details
For more information about Lucid, check out our paper and talk at [SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html)

The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch.

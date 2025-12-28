This is the codebase of the Lucid language.

Lucid is a high-level network programming language. It has an event-based programming model inspired by the actor model, has a module system inspired by ocaml, and has domain-specific constructs that are similar (but higher level) than P4.


### Compiler architecture

Lucid has a frontend, midend, interpreter, compiler to the Tofino, and compiler to C. They are all implemented in ocaml.

In the frontend, there is a type-and-effect system that tracks the order in which a program accesses objects. Any Lucid program that passes type and effect checking is guaranteed to always access all objects in the same order across all control flows. This makes it pipelineable at a fine granularity.

The frontend also does Hindley-Miller inference and focuses on lowering to a core / midend IR that has fewer abstractions. 

The interpreter operates directly on the core IR and is a full implementation of the lanugage. 
It runs in a simulator of a network with a custom-defined topology.
The interpreter takes a json spec file as input, which lists the events to execute. 
Events are annotated with a location (which switch and port they should arrive at) and optional timestamp.
The interpreter can alternatively run in an "interactive" mode, accepting events from stdin and printing output events back to stdout. 

The compiler to tofino implements most features, and is pretty well tested. 
It translates Lucid programs into optimized P4 for the Intel Tofino. 

The compiler to C is a work in progress. It currently compiles Lucid to single-threaded C that uses DPDK or lpcap for packet IO.

### Development and testing

Compile Lucid with "make" -- this builds the interpreter, tofino compiler, c compiler, and some other utilities that are not important for now.

OCaml's type checker is very useful, so run make after every change to make sure there are no errors. 

src/bin contains the interpreter and compiler frontends. 

src/lib contains all the actual functionality. Directory names in src/lib tell you what components they contain. 

examples/interp_tests are the primary test applications. 

examples/tofino_apps are applications we compile and run on the Tofino.

There is a test script for the interpreter and c compiler in test/runtests.py

You can also call it by running "make test" (for interpreter tests) or "make testcc" (for c compiler tests).

The test script takes a while to run, so leave running that up to the user in most cases.

### Lucid references

docs/manual contains comprehensive documentation of Lucid's language, interpreter, and tofino compiler features. 

tutorials contains well-documented examples designed for newcomers to the language. They are useful to see how everything fits together. 

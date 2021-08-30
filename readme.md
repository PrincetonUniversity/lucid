# The Lucid / DPT language

Lucid is a high-level language for programming reconfigurable datapath switch ASICs, currently the Intel Tofino. Compared to P4, Lucid provides higher-level abstractions and a specialized type system that make it much easier to develop complex applications that compile to real hardware.

Lucid's **event-driven abstractions** make it easier to write programs that interleave stateful packet processing with network control *and* compile to real switch ASICs. So, for example, with about 200 lines of Lucid code, you can implement a cuckoo hash table that can install new entries from the data plane. This is possible in P4, but would take thousands of lines of code. 

Lucid's **domain-specific type system** catches bugs in programs related to persistent state in data plane programs by applying syntactic checks early in compilation. This lets Lucid's compiler give developers useful  feedback, such as error messages that point to specific lines of source code, when developers accidentally write code that violates fundamental architectural constraints of the underlying hardware. 

Lucid also has other quality-of-life features for data plane developers. For example, an interpreter for fast prototyping and an optimizing compiler that encodes many of the tricks that we have learned over the years to write efficient Tofino code.


## Using Lucid

To get started with Lucid, first follow the [setup guide](docs/setup.md) and then go through the tutorials that introduce the [language](docs/tutorial_language.md), [interpreter](docs/tutorial_interpreter.md), and [p4-tofino compiler](docs/tutorial_compiler.md).


## More details
For more information about Lucid, check out our paper and talk at [SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html)

The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch.


## Limitations
The current Lucid-to-Tofino compiler is a rewrite of an initial prototype and doesn't yet support all the language features (e.g., events as variable in a program and event-scheduling combinators). There are also likely to be bugs in the compiler. The Lucid interpreter, however, is feature complete and much more reliable.

# The Lucid / DPT language

Lucid is a high-level language for programming reconfigurable datapath switch ASICs, currently the Intel Tofino. Compared to P4, Lucid provides higher-level abstractions and a specialized type system that make it much easier to develop complex applications that compile to real hardware.

Lucid's **higher level abstractions** make it easier to write programs that interleave  packet-processing with network control. So, for example, with about 200 lines of Lucid code, you can implement a cuckoo hash table that uses packet recirculation to install new entries. This is possible in P4, but would be thousands of lines of code. 

Lucid's **domain-specific type system** catches bugs in programs related to the use of persistent state by checking the syntax of a program against simple high-level rules *before* attempting to map it to the datapath hardware. This lets the Lucid compiler provide better feedback, making the development cycle less painful in Lucid than in P4. 

Lucid also has other quality-of-life features for data plane developers: an interpreter for fast prototyping without compiling to the hardware and an optimizing compiler that encodes many (but not yet all) of the tricks that we have learned over the years to write efficient Tofino code.


## Using Lucid

To setup and use Lucid, start with: the [setup guide](docs/setup.md) and tutorials that introduce the [language](docs/tutorial_language.md), [interpreter](docs/tutorial_interpreter.md), and [compiler](docs/tutorial_compiler.md).


## More details
For more information about Lucid, check out our paper and talk at [SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html)

The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch.


## Limitations
The current Lucid-to-Tofino compiler is a rewrite of an initial prototype and doesn't yet support all the language features (e.g., events as variable in a program and event-scheduling combinators). There are also likely to be bugs in the compiler. The Lucid interpreter, however, is feature complete and much more reliable.

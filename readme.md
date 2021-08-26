# The Lucid / DPT language

Lucid is a high-level language for programming reconfigurable datapath switch ASICs, currently the Intel Tofino. Compared to P4, Lucid provides higher-level abstractions and a specialized type system that make it much easier to develop complex applications that compile to real hardware.

Lucid's **higher level abstractions** make it easier to write programs that interleave  packet-processing with network control. So, for example, with about 200 lines of Lucid code, you can implement a cuckoo hash table that uses packet recirculation to install new entries. This is possible in P4, but would be thousands of lines of code. 

Lucid's **domain-specific type system** catches bugs in programs related to the use of persistent state by checking the syntax of a program against simple high-level rules *before* attempting to map it to the datapath hardware. This lets the Lucid compiler provide better feedback, making the development cycle less painful in Lucid than in P4. 

Lucid also has other quality-of-life features for data plane developers: an interpreter for fast prototyping without compiling to the hardware and an optimizing compiler that encodes many (but not yet all) of the tricks that we have learned over the years to write efficient Tofino code.


## Using Lucid
To setup and use Lucid, see the documentation: [setup](docs/setup.md) and [tutorial](docs/tutorial_language.md)


## More details
For more information about Lucid, check out our paper and talk at [SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html)

The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch.


## Limitations
Lucid is an ongoing project. While the Lucid interpreter supports all the language features, the current Lucid compiler does not. . Specifically, the compiler does not support events as variables in programs and does not generate code for scheduling events (i.e., setting when and where events happen via the ``locate`` and ``delay`` combinators). There are also likely to be other bugs in the  compiler.
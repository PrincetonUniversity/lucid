### Lucid interpreter examples

The examples in this directory demonstrate how to: 

1. use the Lucid interpreter and Lucid's event-based processing model  (`01monitor`)
2. use match statements and runtime-modifiable tables for sophisticated decision logic (`02filter`)
3. use lucid Arrays to track state across packets/events (`03counter`)
4. use modules to write libraries and re-usable data structures (`04modules`)
5. control event serialization and deserialization with parsers (`05parsing`)

Each example includes a `readme.md`, an annotated `.dpt` source file and a `.json` input trace for the interpreter. To run an example in the interpreter, you should be able to just run `make` in its project directory (Assuming you are in a local copy of the Lucid repo and have compiled it).
### Lucid interpreter examples

The examples in this directory demonstrate how to: 

1. use the Lucid interpreter  (`01monitor`)
2. structure a packet processing program using _events_ and _handlers_ (`01monitor`)
3. use match statements and runtime-modifiable tables for sophisticated decision logic (`02filter`)
4. use lucid Arrays to track state across packets/events (`03counter`)

Each example includes a `readme.md`, an annotated `.dpt` source file and a `.json` input trace for the interpreter. To run an example in the interpreter, you should be able to just run `make` in its project directory. (Assuming that you have docker installed.)
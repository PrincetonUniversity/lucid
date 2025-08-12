### Lucid interpreter examples

The examples in this directory introduce most features of Lucid. Each example includes a `readme.md`, an annotated `.dpt` source file and a `.json` input trace for the interpreter. To run an example in the interpreter, you should be able to just run `make` in its project directory (Assuming you are in a local copy of the Lucid repo and have compiled it).

Examples 1 - 4 go through the basics that you are likely to use in any complete program:
1. use the Lucid interpreter and Lucid's event-based processing model  (`01monitor`)
2. match statements and runtime-modifiable match-action tables (`02filter`)
3. atomically-updateable Arrays to track state across packets/events (`03counter`)
4. event serialization and deserialization with parsers (`04parsing`)

Examples 5 - 7 focus on features that support modular programs and re-usable libraries.
5. modules for libraries and data structures (`05modules`)
6. polymorphism for type-safe generic functions and containers (`06polymorphism`) *under construction*
7. vectors and statically-bounded for loop (`07vectors`) *under construction*


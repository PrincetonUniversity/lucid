# Tutorials


The examples in this directory introduce most features of Lucid. Each example includes a readme.md, an annotated .dpt source file and a .json input trace for the interpreter. To run an example in the interpreter, you should be able to just run make in its project directory (Assuming you are in a local copy of the Lucid repo and have compiled it).

### Basic language and interpreter features

[Tutorial 1](interp/01monitor) Using the interpreter and structuring a packet processing program using _events_ and _handlers_.

[Tutorial 2](interp/02filter) Using match statements and runtime-modifiable tables for sophisticated decision logic, installing match-action rules in the interpreter.

[Tutorial 3](interp/03counter) Using Lucid Arrays to track state across packets/events. Reading program state in the interpreter.

### Language features for modular and re-usable code

[Tutorial 4](interp/04parsing) Event serialization and deserialization with parsers.

[Tutorial 5](interp/05modules) Modules for libraries and data structures.

[Tutorial 6](interp/06polymorphism) Polymorphism for type-safe generic functions and containers.

[Tutorial 7](interp/07vectors) Vectors and statically-bounded for loops.

### Compiling to the tofino

[Tofino SDE Setup](tofino/00setup) Setting up the Tofino SDE and environment variables.

[Tutorial 1](tofino/01reflector) Compiling and running a simple packet reflector program on the Tofino using Lucid-provided scripts.

[Tutorial 2](tofino/02tables) Writing and running a simple script on the Tofino's management CPU that adds a rule to a table in the program from the control plane.

### Next steps

After going through these tutorials, you should be ready to dig into the example and test applications:

- The suite of [interpreter test programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests)

- Some re-usable [data structure libraries](https://github.com/PrincetonUniversity/lucid/tree/main/examples/library)

- A few [larger example applications](https://github.com/PrincetonUniversity/lucid/tree/main/examples).

To see how to do specific things, look at the comprehensive details about all of Lucid's features in the [wiki](https://github.com/PrincetonUniversity/lucid/wiki).

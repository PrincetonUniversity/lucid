# Tutorials

There are two [tutorials in the repository](https://github.com/PrincetonUniversity/lucid/tree/main/tutorials). Each tutorial has a short series of well-documented examples that demonstrates and explains the core parts of Lucid. If you have not yet installed Lucid, refer to the install instructions on the [homepage](/lucid/):

### Interpreter tutorials

[Tutorial 1](/lucid/tutorials/interp/01monitor) Using the interpreter and structuring a packet processing program using _events_ and _handlers_.

[Tutorial 2](/lucid/tutorials/interp/02filter) Using match statements and runtime-modifiable tables for sophisticated decision logic, installing match-action rules in the interpreter.

[Tutorial 3](/lucid/tutorials/interp/03counter) Using Lucid Arrays to track state across packets/events. Reading program state in the interpreter.

### Tofino tutorials

[Tofino SDE Setup](/lucid/tutorials/tofino/00setup) Setting up the Tofino SDE and environment variables.

[Tutorial 1](/lucid/tutorials/tofino/01reflector) Compiling and running a simple packet reflector program on the Tofino using Lucid-provided scripts.

[Tutorial 2](/lucid/tutorials/tofino/02tables) Writing and running a simple script on the Tofino's management CPU that adds a rule to a table in the program from the control plane.

### Next steps

After going through these tutorials, you should be ready to dig into the example and test applications:

- The suite of [interpreter test programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests)

- Some re-usable [data structure libraries](https://github.com/PrincetonUniversity/lucid/tree/main/examples/library)

- A few [larger example applications](https://github.com/PrincetonUniversity/lucid/tree/main/examples).

To see how to do specific things, look at the comprehensive details about all of Lucid's features in the [wiki](https://github.com/PrincetonUniversity/lucid/wiki).
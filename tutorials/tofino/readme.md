## Lucid-Tofino backend tutorial

The tutorials in this section focus on compiling and running Lucid applications on the Tofino. Compiling and running any P4 program on the tofino is a somewhat complicated process, and so Lucid provides scripts and makefiles to simplify it. These scripts, however, are not as well-tested as the interpreter or the Lucid compiler itself, so you may run into issues. If you do, please make sure to try running them with the recommended versions of the bf-sde. Then, if you still have issues, please reach out to us via github or email. 

### Initial setup

This tutorial assumes that you are using the Lucid compiler built from source, that you have access to the tofino's bf-sde, and that you have basic familiarity with the bf-sde. 

Before continuing, make sure that you have: 
1. bf-sde version 9.13.0 installed with `bfrt` enabled. The exact yaml file used to test the lucid compiler is included in this directory, `bfsde-lucid.yaml`. For the current version of lucid, it is likely that the only option you really need is the `drivers: bfrt: true`. However we have not tested that hypothesis yet. It is fine to include other options in the yaml file as needed by your other projects.

2. $SDE set to the root of the bf-sde directory, and $SDE_INSTALL set to the install directory. This is usually done by running `. ./set_sde.bash` in the bf-sde directory, with the `set_sde.bash` script being provided by Intel.

3. hugepages configured appropriately to run the Tofino model.

### Tutorials

- 01Reflector: A simple program that reflects packets back to the sender. The readme.md file in this directory contains a step-by-step guide to compiling and running the program on the tofino using Lucid-provided scripts.

- 02Tables: A program that demonstrates how to write and run a simple script that adds a rule to a table in the program from the control plane.
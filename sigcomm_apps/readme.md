### Lucid Sigcomm artifact

**Note: these scripts are all intended to be run on the included VM, or an ubuntu 18.04 environment that has run ../vagrant/install_unbuntu_prereqs.sh**

This directory contains the programs used to benchmark Lucid in the Sigcomm 2021 paper. Use the included scripts and makefiles to: 

1) compile the lucid programs to P4. 
2) compile the P4 programs to Tofino binaries (if you have access to the Tofino SDE)
3) measure and report statistics about the Lucid program, P4 program, and Tofino binary. 

#### Reproducing the principle table

The main empirical result in the Lucid paper is Figure 8, which lists the lines of code and pipeline stages for each of the applications in the ``apps`` directory. Here is how to reproduce those results. 

*Note that, to measure the Tofino pipeline stages, you must have the Tofino SDE installed in your system and the $SDE and $SDE_INSTALL env variables set. This was tested with SDE version 9.5.0. Original results were obtained with SDE version 9.2.0.*

1. compile lucid --> P4: ``make p4``
    *estimated time: 1 hour. Around 40 minutes to compile starflow, around 1 minute for each of the other 9 applications.*

2. compile P4 --> tofino: ``make tofino``
    *estimated time: 30 minutes. This will fail if the Tofino SDE is not installed with $SDE and $SDE_INSTALL set.*

3. finally, to print the statistics from the builds, run ``make stats``. This can be run with or without compiling to the Tofino. 

#### Compiling individual applications

The individual applications are in ``/apps/*``. The Lucid source file of each program is ``/apps/<appname>/<appname>.dpt``. You can build apps individually by using the same make commands described above in the application directories. 

#### Directory contents and structure

- ``apps/<appname>`` -- Build directory for application <appname>
- ``apps/<appname>/<appname>.dpt`` -- Lucid implementation of <appname>. 
- ``apps/<appname>/harness.p4`` -- A simple P4 program that converts packets into Lucid events.
- ``apps/<appname>/p4`` -- The output of the Lucid compiler.
- ``apps/<appname>/p4/lucid.p4`` -- Lucid-generated P4-tofino code. 
- ``apps/<appname>/p4/lucid.<cpp|py>`` -- Lucid-generated P4-tofino controller code, e.g, for setting up multicast groups.  
- ``apps/<appname>/p4/logs`` -- Lucid compiler logs.
- ``apps/<appname>/p4/libs`` -- Some runtime libraries provided by the Lucid compiler.
- ``apps/<appname>/p4/makefile`` -- Build the lucid-generated P4 (lucid.p4) to a Tofino binary. 
- ``apps/<appname>/p4/lucid`` -- Output of the Tofino compiler when using the above makefile. 
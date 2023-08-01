This directory contains utilities that make compiling and running P4 programs easier. The Lucid compiler copies these scripts to the project's build directory. There are two 

p4tapp.sh -- script with commands to build p4 programs and launch them in tofino asic or on hardware. Also has commands to run tests, etc.

testutils.py -- script to generate test packet traces from jsons in the Lucid tofino backend test cases. Used by p4tapp.sh

validationutils.py -- script to check output of tofino model against expected results from Lucid tofino backend test jsons. Used by p4tapp.sh. Also has some helpers for manual analysis of compiler output.

controldriver.py -- library that lucid-generated control code uses to interface with tofino's driver. Functions for things like adding rules to tables, multicast groups, port up / down, etc. Expects to be run in the bfrt python interpreter that launches inside the bfshell started with bf_switchd. Requires bfrt to be enabled in your tofino sde installation. This skips over many layers of the tofino's control plane stack -- finds and loads a low-level interface shared library for the tofino that should be present in any sde installation with bfrt enabled. The benefit of connecting to the driver at this low level is API stability -- controldriver.py works with at least SDE 9.5.0 - 9.13.0. 
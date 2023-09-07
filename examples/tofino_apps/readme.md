## Tofino examples

This directory contains example programs that are regularly compiled and tested on the tofino asic model -- these programs should work correctly on a real switch. 


#### Applications 

Applications are in the `./src/` subdirectory. Some applications include:

**simple_cuckoo_firewall** -- a stateful firewall that uses a cuckoo hash table to remember connections from trusted hosts. Insert operations, for the hash table, may occur over multiple events that move items around in memory to mitigate collisions.

**reflector** -- sends an ip packet out of the same port it came in on. 

**control_reflector** -- generates a control event from an ip packet. When processing the control event, generates an exit event that sends the packet out of the same port it arrived on. 

**multi_events** -- generate two background events from a handler. 

**checksum** -- compute checksums in parser and deparser of packet events.

#### Adding new cases

To add a new test case: 

1. add the program and test to `./src` and `./tests`. 

2. update `<lucid_git_root>/test/testspecs/tested_examples.json` to include commands to compile / assemble / run the program.


#### Contents

- ``src/`` -- the lucid applications

- ``tests/`` -- test cases, use with scripts in ``<git root>/test``

- ``ip_harness.p4`` -- a simple IP harness. 

- ``ip_harness_triggers.json`` -- entry event triggers for the IP harness.

- ``makefile`` -- compile, assemble, and test all the applications in this directory. 
    - ``make assemble_{appname}`` compile ``src/appname.dpt`` from Lucid to P4, in ``builds/{appname}``
    - ``make assemble_{appname}`` compile the P4 in ``builds/{appname}`` into a tofino binary
    - ``make test_{appname}`` test the compiled P4 in ``builds/{appname}`` on the Tofino asic model, using the test case ``tests/{appname}.json``.


#### Harness

All programs use a common P4 harness, ``ip_harness.p4`` and its configuration file, ``ip_harness_triggers.json``. This harness parses ethernet/ip packets and can be configured, using the json, to generate events based on any parsed header fields, and handle exit events by changing parsed field values. 

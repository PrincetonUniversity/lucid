### SDE requirements

This was tested on SDE version 9.13.0 with the following profile:

```
global-options: {}
features:
  drivers:
    bfrt: true
    bfrt-generic-flags: true
    grpc: true
    p4rt: true
    pi: true
    thrift-driver: true
  switch:
    sai: true
    thrift-switch: true
architectures:
  - tofino
  - tofino2
```

SDE versions back to 9.5 are likely to work. 

You MUST have bfrt enabled in your SDE install. YMMV if you don't have the other options enabled.

### Usage

1. compile and run `artifact/lucid.p4`
2. start the python control plane, `artifact/lucid.py`, with this command: 

```$(SDE)/run_bfshell.sh -b lucid.py```

**NOTE: I think it is important to run this command from the directory where lucid.py lives**

### Files

**source files**
- src/flowpoll.dpt -- the lucid program. if you have lucid installed, build with `make build` to create the `lucid_tofino` build directory.
- src/custom_tofino_control.py -- a simple control plane function that polls the registers in flowpoll.dpt once per second.

**p4 artifact files**
- artifact/* -- the artifact directory is the compiled p4 + control script
- artifact/lucid.p4 -- the compiled P4 program.
- artifact/lucid.py -- the python control plane. It polls the flow table once per second and prints the non-zero entries.
- artifact/test_trace.pcap -- a small test trace
- artifact/makefile -- commands to compile / run / test the program on the asic model. Not tested on systems besides my own. You will need to have a vethpair veth256/veth257 mapped to asic model dpid 128.
- artifact/globals.json -- a json directory mapping from lucid objects to p4 objects

### Other notes
the control script only polls register arrays on pipeline number 1. If you want to use a different pipeline, please change the argument to `poll_flowtable` in `lucid.py`

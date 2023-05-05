### Untested tofino apps

This directory contains some misc tofino apps that have been tested to compile successfully to the tofino (as of 5/5/2023 on SDE 9.7.1), but have not been run.

- `NAT.dpt`: an update of `NAT.dpt` from the sigcomm2021 evaluation app set.
- `stateful_fw.dpt`: an update of the `stateful_fw.dpt` from the sigcomm2021 evaluation app set. **Note: there is a variant of a cuckoo-based stateful firewall in /examples/tofino_apps/src/simple_cuckoo_firewall.dpt that is regularly tested and works on Eth/IP packets. I recommend looking at that first.**

Usage: 
1) compile dptc.
2) use the makefile to build the program of your choice with the SRC arg, e.g.:
```
make SRC=NAT.dpt
```
3) the generated P4 and python control plane will be in `build_<program_name>`. 

If you want to compile and run the application using your own build tools, compile `build_<prog_name>/lucid.p4`. To run the program, after you start `bf_switchd`, be sure to run the lucid-generated control plane to load the appropriate multicast rules. To run the 
lucid control plane, use the `run_bfshell.sh`: `run_bfshell.sh -b build_<prog_name>/lucid.py`.

Alternately, if you want to compile and run the application using the lucid-provided build scripts, try:

- build the p4: `cd build_<program_name>; make build`
- run in asic simulator, after p4 is built: `cd build_<program_name>; make sim`
- run on hardware switch, after p4 is built: `cd build_<program_name>; make hw`

Note that the Lucid build scripts assume the program is running on a bf-wedge 32-port 100Gbe switch with 2 pipelines. Also, the lucid build scripts and makefile assume $SDE is set appropriately on your system.

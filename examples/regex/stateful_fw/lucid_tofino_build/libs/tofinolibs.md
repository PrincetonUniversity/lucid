This directory contains utilities that make compiling and running P4 programs easier. The Lucid compiler copies these scripts to the project's build directory. 

##requirements

These scripts assume that you are using the tofino sde built with the configuration options in ``install_bf_sde.sh``. The build must include: bf-runtime, p4-runtime, pi, thrift, grpc, and ptf-modules. 
These scripts also assume that your $SDE and $SDE_INSTALL variables are set appropriately, e.g., by running ``. ./set_sde.bash`` in the sde directory. 
Testing was done on ubuntu 18.04 with sde version ``9.5.0``.

##p4tapp.sh

``p4tapp.sh`` is a script for P4 on Tofino that automates: building, running with the ASIC simulator and hardware, and testing with template-generated pcaps. 

###Building a P4 program

To build, use: ``p4tapp.sh build prog.p4``. 
**Input**: p4tapp expects three files in the same target directory: 
    - prog.p4 -- the P4 program.
    - prog.cpp  -- a low-level controller for the P4 program. This is a bf_switchd extension that builds on the included ``mgr.h``.
    - prog.py -- a high-level controller for the P4 program. This uses the included ``mgr.py``, which is a python abstraction layer over to bf_switchd's grpc and thrift interfaces.

**Output**: 
    p4tapp builds the Tofino binary, configuration file, and custom bf_switchd of ``prog.p4`` to ``./prog/``


###Running on Tofino ASIC simulator

To run a built project on the Tofino ASIC simulator, use:  

``p4tapp.sh sim prog.p4``

This will start up: 1) the ASIC simulator; 2) the program's custom bf_switch; 3) the p4 program itself; 4) the program's custom python control plane. It will print output from all processes to stdout and log to ``./run_logs``

###Testing

``p4tapp.sh makepcap trace.json trace.pcap`` -- generate trace.pcap based on trace.json

``p4tapp.sh sendpcap trace.pcap 1 2`` -- send trace.pcap into port 1 of the asic simulator, collect pcap output of port 2 and save it to trace.pcap.rx.pcap. You have to start the simulator, etc. before you run this command. 

``p4tapp.sh simtest prog.p4 trace.json 1 2`` -- start prog.p4 on the simulator, convert trace.json into a pcap, send the pcap into port 1 of the simulator, and collect output from port 2.

### reflector.dpt

This application is a simple packet reflector that sends each packet out of its input port. The purpose is simply to illustrate how to compile a Lucid program to P4, and run that program on the Tofino ASIC model. To do so, we will use some of the scripts included with the Lucid compiler. 

### Preliminaries

Before continuing, make sure that you have: 
1. bf-sde installed with `bfrt` enabled. 

2. $SDE set to the root of the bf-sde directory, and $SDE_INSTALL set to the install directory. This is usually done by running `. ./set_sde.bash` in the bf-sde directory, with the `set_sde.bash` script being provided by Intel.

3. hugepages configured appropriately to run the Tofino model.

### Compiling the program

The first step is compiling the lucid program to P4. Run `make build` from this example directory to build the program in `./build`. The `build` directory will contain the P4 program as `lucid.p4` and other helper files. `make build` just calls the lucid compiler with the appropriate arguments: `dptc reflector.dpt -o build`.

The second step is compiling the P4 program in the build directory. Run `make assemble` to do this. This will compile the P4 program to a tofino binary / configuration in the `<build>/lucid_tofino` directory. It uses a helper script, `./build/libs/p4tapp.sh`, to do construct the call to the tofino compiler and put the output in the right place.


### Running the program

The next steps are: 

    1. run the Tofino model;

    2. load the compiled tofino binary into the model; 

    3. run the lucid control plane python script.

Run `make sim` to do all of this. The makefile again uses the `<build>/libs/p4tapp.sh` helper script: `cd build; ./libs/p4tapp.sh sim lucid.p4`. When you run `make sim` or the underlying `p4tapp.sh` command, the output should look something like this: (a description follows)

```
sonch@johnshack3:~/Desktop/gits/lucid/tutorials/tofino/01Reflector/build$ ls
eventlib.py   layout_info.txt  logs       lucid.p4  lucid_tofino  manifest.txt  src
globals.json  libs             lucid.cpp  lucid.py  makefile      scripts
jsonch@johnshack3:~/Desktop/gits/lucid/tutorials/tofino/01Reflector/build$ make sim
running simulator on: /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
setting up veths for ports in simulator
veth pair veth256 <--> veth257 exists
veth pair veth258 <--> veth259 exists
veth pair veth260 <--> veth261 exists
veth pair veth262 <--> veth263 exists
veth pair veth264 <--> veth265 exists
veth pair veth272 <--> veth273 exists
veth pair veth280 <--> veth281 exists
veth pair veth288 <--> veth289 exists
veth pair veth296 <--> veth297 exists
veth pair veth304 <--> veth305 exists
veth pair veth312 <--> veth313 exists
veth pair veth320 <--> veth321 exists
veth pair veth328 <--> veth329 exists
veth pair veth336 <--> veth337 exists
veth pair veth344 <--> veth345 exists
veth pair veth352 <--> veth353 exists
veth pair veth360 <--> veth361 exists
veth pair veth368 <--> veth369 exists
veth pair veth376 <--> veth377 exists
veth pair veth384 <--> veth385 exists
veth pair veth392 <--> veth393 exists
SIM_CMD: sudo /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/tofino-model --time-disable --p4-target-config /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf -d 1 -f /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json --chip-type 2 --install-dir /home/jsonch/Desktop/bf-sde-9.13.0/install --log-dir . --json-logs-enable --pkt-log-len 100000 --int-port-loop 196
[SIM] PRIVS: Eff=0x00000000 Perm=0x00002006 Inh=0x00000000
[SIM] Performing preliminary checks on config file
[SIM] Opening p4 target config file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf' ...
[SIM] Loaded p4 target config file
[SIM] Package size 1
[SIM] No of Chips is 1
[SIM] Of-Port info file is /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json
[SIM] Chip part revision number is 1
[SIM] No of Packages is 1
[SIM] Chip 0 SKU: defaulting to BFN77110
[SIM] Chip 0 SKU setting: sku 0 pipe mode 0
[SIM] Chip 0 Physical pipes enabled bitmap 0xf
[SIM] Setting Packet log length to 100000
[SIM] Internal port loopback for pipes: 0xc4
[SIM] Opening /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json' ...
[SIM] Loaded /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json
[SIM] Adding interface veth256 as port 128
[SIM] Mapping phyofport 128 (logofport 128) to veth256 and veth257
[SIM] Adding interface veth258 as port 129
[SIM] Mapping phyofport 129 (logofport 129) to veth258 and veth259
[SIM] Adding interface veth260 as port 130
[SIM] Mapping phyofport 130 (logofport 130) to veth260 and veth261
[SIM] Adding interface veth262 as port 131
[SIM] Mapping phyofport 131 (logofport 131) to veth262 and veth263
[SIM] Adding interface veth264 as port 132
[SIM] Mapping phyofport 132 (logofport 132) to veth264 and veth265
[SIM] Adding interface veth272 as port 136
[SIM] Mapping phyofport 136 (logofport 136) to veth272 and veth273
[SIM] Adding interface veth280 as port 140
[SIM] Mapping phyofport 140 (logofport 140) to veth280 and veth281
[SIM] Adding interface veth288 as port 144
[SIM] Mapping phyofport 144 (logofport 144) to veth288 and veth289
[SIM] Adding interface veth296 as port 148
[SIM] Mapping phyofport 148 (logofport 148) to veth296 and veth297
[SIM] Adding interface veth304 as port 152
[SIM] Mapping phyofport 152 (logofport 152) to veth304 and veth305
[SIM] Adding interface veth312 as port 156
[SIM] Mapping phyofport 156 (logofport 156) to veth312 and veth313
[SIM] Adding interface veth320 as port 160
[SIM] Mapping phyofport 160 (logofport 160) to veth320 and veth321
[SIM] Adding interface veth328 as port 164
[SIM] Mapping phyofport 164 (logofport 164) to veth328 and veth329
[SIM] Adding interface veth336 as port 168
[SIM] Mapping phyofport 168 (logofport 168) to veth336 and veth337
[SIM] Adding interface veth344 as port 172
[SIM] Mapping phyofport 172 (logofport 172) to veth344 and veth345
[SIM] Adding interface veth352 as port 176
[SIM] Mapping phyofport 176 (logofport 176) to veth352 and veth353
[SIM] Adding interface veth360 as port 180
[SIM] Mapping phyofport 180 (logofport 180) to veth360 and veth361
[SIM] Adding interface veth368 as port 184
[SIM] Mapping phyofport 184 (logofport 184) to veth368 and veth369
[SIM] Adding interface veth376 as port 188
[SIM] Mapping phyofport 188 (logofport 188) to veth376 and veth377
[SIM] Adding interface veth384 as port 192
[SIM] Mapping phyofport 192 (logofport 192) to veth384 and veth385
[SIM] Adding interface veth392 as port 196
[SIM] Mapping phyofport 196 (logofport 196) to veth392 and veth393
[SIM] Simulation target: Asic Model
[SIM] Using TCP port range: 8001-8004
[SIM] Listen socket created
[SIM] bind done on port 8001. Listening..
[SIM] Waiting for incoming connections...
[SIM] CLI listening on port 8000
SWITCHD COMMAND: sudo env SDE=/home/jsonch/Desktop/bf-sde-9.13.0 SDE_INSTALL=/home/jsonch/Desktop/bf-sde-9.13.0/install PATH=/home/jsonch/Desktop/bf-sde-9.13.0/install/bin:/home/jsonch/.local/bin:/home/jsonch/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin LD_LIBRARY_PATH=/usr/local/lib:/home/jsonch/Desktop/bf-sde-9.13.0/install/lib: /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/bf_switchd --background --status-port 7777 --install-dir /home/jsonch/Desktop/bf-sde-9.13.0/install --conf-file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
[SWITCHD_MGR] 2024-07-29 12:23:24.087858 BF_SWITCHD DEBUG - bf_switchd: system services initialized
[SWITCHD_MGR] 2024-07-29 12:23:24.087911 BF_SWITCHD DEBUG - bf_switchd: loading conf_file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf...
[SWITCHD_MGR] 2024-07-29 12:23:24.087951 BF_SWITCHD DEBUG - bf_switchd: processing device configuration...
[SWITCHD_MGR] 2024-07-29 12:23:24.088009 BF_SWITCHD DEBUG - Configuration for dev_id 0
[SWITCHD_MGR] 2024-07-29 12:23:24.088030 BF_SWITCHD DEBUG -   Family        : tofino
[SWITCHD_MGR] 2024-07-29 12:23:24.088049 BF_SWITCHD DEBUG -   pci_sysfs_str : /sys/devices/pci0000:00/0000:00:03.0/0000:05:00.0
[SWITCHD_MGR] 2024-07-29 12:23:24.088069 BF_SWITCHD DEBUG -   pci_int_mode  : 0
[SWITCHD_MGR] 2024-07-29 12:23:24.088089 BF_SWITCHD DEBUG -   sds_fw_path   : share/tofino_sds_fw/avago/firmware
[SWITCHD_MGR] 2024-07-29 12:23:24.088109 BF_SWITCHD DEBUG - bf_switchd: processing P4 configuration...
[SWITCHD_MGR] 2024-07-29 12:23:24.088162 BF_SWITCHD DEBUG - P4 profile for dev_id 0
[SWITCHD_MGR] 2024-07-29 12:23:24.088183 BF_SWITCHD DEBUG - num P4 programs 1
[SWITCHD_MGR] 2024-07-29 12:23:24.088202 BF_SWITCHD DEBUG -   p4_name: lucid
[SWITCHD_MGR] 2024-07-29 12:23:24.088223 BF_SWITCHD DEBUG -   p4_pipeline_name: pipe
[SWITCHD_MGR] 2024-07-29 12:23:24.088242 BF_SWITCHD DEBUG -     libpd:
[SWITCHD_MGR] 2024-07-29 12:23:24.088262 BF_SWITCHD DEBUG -     libpdthrift:
[SWITCHD_MGR] 2024-07-29 12:23:24.088282 BF_SWITCHD DEBUG -     context: /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json
[SWITCHD_MGR] 2024-07-29 12:23:24.088303 BF_SWITCHD DEBUG -     config: /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/tofino.bin
[SWITCHD_MGR] 2024-07-29 12:23:24.088322 BF_SWITCHD DEBUG -   Pipes in scope [
[SWITCHD_MGR] 2024-07-29 12:23:24.088342 BF_SWITCHD DEBUG - 0
[SWITCHD_MGR] 2024-07-29 12:23:24.088362 BF_SWITCHD DEBUG - 1
[SWITCHD_MGR] 2024-07-29 12:23:24.088382 BF_SWITCHD DEBUG - 2
[SWITCHD_MGR] 2024-07-29 12:23:24.088401 BF_SWITCHD DEBUG - 3
[SWITCHD_MGR] 2024-07-29 12:23:24.088421 BF_SWITCHD DEBUG - ]
[SWITCHD_MGR] 2024-07-29 12:23:24.088446 BF_SWITCHD DEBUG -   diag:
[SWITCHD_MGR] 2024-07-29 12:23:24.088466 BF_SWITCHD DEBUG -   accton diag:
[SWITCHD_MGR] 2024-07-29 12:23:24.088486 BF_SWITCHD DEBUG -   Agent[0]: /home/jsonch/Desktop/bf-sde-9.13.0/install/lib/libpltfm_mgr.so
[SWITCHD_MGR] 2024-07-29 12:23:24.090255 BF_SWITCHD DEBUG - bf_switch: Operational mode set to default: MODEL
[SWITCHD_MGR] Starting PD-API RPC server on port 9090
[SWITCHD_MGR] 2024-07-29 12:23:24.101217 BF_SWITCHD DEBUG - bf_switchd: drivers initialized
[SWITCHD_MGR] 2024-07-29 12:23:24.101249 BF_SWITCHD DEBUG - bf_switchd: initializing dru_sim service
[SWITCHD_MGR] 2024-07-29 12:23:24.101519 BF_SWITCHD DEBUG - bf_switchd: library libdru_sim.so loaded
[SIM] Connection accepted on port 8001
[SWITCHD_MGR] INFO: DRU sim MTI initialized successfully
[SIM] Client socket created
[SWITCHD_MGR] dru_sim: client socket created
[SIM] Connected on port 8002
[SWITCHD_MGR] dru_sim: connected on port 8001
[SWITCHD_MGR] dru_sim: listen socket created
[SIM] INFO: DRU sim MTI initialized successfully
[SWITCHD_MGR] dru_sim: bind done on port 8002, listening...
[SWITCHD_MGR] dru_sim: waiting for incoming connections...
[SWITCHD_MGR] dru_sim: connection accepted on port 8002
[SWITCHD_MGR] dru_sim: DRU simulator running
[SIM] EFUSE:Idx 0, sku 0, pipe_mode is 0, pipes_en_bmp 0xf, model-val 0x0
[SWITCHD_MGR] Setting core_pll_ctrl0=cd44cbfe
[SIM] Tofino Verification Model - Version 4a11102-dirty
[SIM] Created 1 packet processing threads
[SIM] LOGS captured in: ./model_20240729_122322.log
[SIM] :07-29 12:23:26.570392:    Setting logging fn
[SIM] :07-29 12:23:26.570469:    Registering handler for tx
[SIM] Opening p4 target config file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf' ...
[SIM] Loaded p4 target config file
[SIM] Device 0: Pipe 0: loading P4 name lookup file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json found in /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
[SIM] :07-29 12:23:26.570562:    :-:-:<0,-,0>:Waiting for packets to process
[SIM] Opening context file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json' ...
[SIM] Loaded context file
[SIM] Device 0: Pipe 1: loading P4 name lookup file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json found in /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
[SIM] Opening context file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json' ...
[SIM] Loaded context file
[SIM] Device 0: Pipe 2: loading P4 name lookup file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json found in /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
[SIM] Opening context file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json' ...
[SIM] Loaded context file
[SIM] Device 0: Pipe 3: loading P4 name lookup file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json found in /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
[SIM] Opening context file '/home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/pipe/context.json' ...
[SIM] Loaded context file
[SIM] :07-29 12:23:26.584157:     Updating model log flags: clearing 0xffffffff ffffffff, setting 0x00000000 0000000f
[SIM] :07-29 12:23:26.591606:    :-:-:<0,0,0>:Updating p4 log flags: clearing 0xffffffff ffffffff, setting 0x00000000 0000007f
[SIM] :07-29 12:23:26.598913:    :-:-:<0,0,0>:Updating tofino log flags: clearing 0xffffffff ffffffff, setting 0x00000000 0000007f
[SIM] :07-29 12:23:26.605316:    :-:-:<0,0,0>:Updating packet log flags: clearing 0xffffffff ffffffff, setting 0x00000000 0000007f
[SIM] Dropping excess privileges...
[SIM] :07-29 12:23:26.846668:    :-:-:<0,0,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.846767:    :-:-:<0,0,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.848526:    :-:-:<0,1,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.848592:    :-:-:<0,1,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.850345:    :-:-:<0,2,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.850410:    :-:-:<0,2,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.852180:    :-:-:<0,3,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
[SIM] :07-29 12:23:26.852247:    :-:-:<0,3,->:LearningFilter::clear : Excessive clearing of filter 1 !!! Nothing to clear
/2024-07-29 12:23:33.974748 BF_SWITCHD DEBUG - bf_switchd: dev_id 0 initialized
[SWITCHD_MGR] 2024-07-29 12:23:33.974780 BF_SWITCHD DEBUG - bf_switchd: initialized 1 devices
[SWITCHD_MGR] 2024-07-29 12:23:33.974812 BF_SWITCHD DEBUG - bf_switchd: spawning cli server thread
[SWITCHD_MGR] 2024-07-29 12:23:33.975081 BF_SWITCHD DEBUG - bf_switchd: running in background; driver shell is disabled
[SWITCHD_MGR] 2024-07-29 12:23:33.975121 BF_SWITCHD DEBUG - bf_switchd: server started - listening on port 9999
[SWITCHD_MGR] bfruntime gRPC server started on 0.0.0.0:50052
PYTHON CONTROL: /home/jsonch/Desktop/bf-sde-9.13.0/run_bfshell.sh -b /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid.py
[PY_MGR] Using SDE /home/jsonch/Desktop/bf-sde-9.13.0
[PY_MGR] Using SDE_INSTALL /home/jsonch/Desktop/bf-sde-9.13.0/install
[PY_MGR] Connecting to localhost port 7777 to check status on these devices: [0]
[PY_MGR] Waiting for device 0 to be ready
[PY_MGR] /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/bfshell /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid.py
[PY_MGR] bfrt_python /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid.py
[PY_MGR] exit
[PY_MGR] 
[PY_MGR] ********************************************
[PY_MGR] *      WARNING: Authorised Access Only     *
[PY_MGR] ********************************************
[PY_MGR] 
[PY_MGR] 
bfshell> bfrt_python /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid.py
[PY_MGR] cwd : /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs
[PY_MGR] 
[PY_MGR] We've found 1 p4 programs for device 0:
[PY_MGR] lucid
[PY_MGR] Creating tree for dev 0 and program lucid
[PY_MGR] 
[PY_MGR] Devices found :  [0]
[PY_MGR] Python 3.10.9 (main, May 18 2023, 15:23:07) [GCC 11.3.0]
[PY_MGR] Type 'copyright', 'credits' or 'license' for more information
[PY_MGR] IPython 7.31.1 -- An enhanced Interactive Python. Type '?' for help.
[PY_MGR] globals_dict: {}
[PY_MGR] [add_multicast_group] adding mc group: 1000--> [[(128, 0), (129, 0), (130, 0), (131, 0)]]
[PY_MGR] [add_multicast_group] done
[SWITCHD_MGR] bf_rt cli exited normally.
[PY_MGR] [add_multicast_group] adding mc group: 1--> [[(196, 1)]]
[PY_MGR] [add_multicast_group] done
[PY_MGR] closing session
[PY_MGR] control startup complete.
[PY_MGR] bfshell> exit
**** ports available in simulation ****
tofino dpid,userspace veth
128,veth257
129,veth259
130,veth261
131,veth263
132,veth265
136,veth273
140,veth281
144,veth289
148,veth297
152,veth305
156,veth313
160,veth321
164,veth329
168,veth337
172,veth345
176,veth353
180,veth361
184,veth369
188,veth377
192,veth385
**** simulation running -- press ctrl+c to terminate. ****
```
### What the P4tapp script does

Running `./libs/p4tapp.sh sim lucid.p4` does 4 things. 

1. **Sets up veth pairs for the tofino model**, and binds them to DPID in the P4 tofino program. 
    This can be seen in the first part of the output, e.g., 
    ```
    [SIM] Adding interface veth256 as port 128
    [SIM] Mapping phyofport 128 (logofport 128) to veth256 and veth257
    ...
    ```
    If the veth pairs are already setup, the script will not recreate them.
    The mapping from DPID to veth pair is also reported at the end of startup, e.g., 
    ```
    **** ports available in simulation ****
    tofino dpid,userspace veth
    128,veth257
    ...
    ```

2. **Starts the tofino model as a background process.** This can be seen in the output as the `SIM_CMD` line, e.g., 
    ```
    SIM_CMD: sudo /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/tofino-model --time-disable --p4-target-config /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf -d 1 -f /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/run_logs/sim_veth_map.json --chip-type 2 --install-dir /home/jsonch/Desktop/bf-sde-9.13.0/install --log-dir . --json-logs-enable --pkt-log-len 100000 --int-port-loop 196
    ```
    After the model is started, the script will redirect all stdout from the model to the current shell, prefixing each line with the `[SIM]` tag. The script will wait for the model to be ready before proceeding. It detects this by monitoring for the line "CLI listening on port 8000" in the model output.

3. **Starts the switchd process**, passing it the P4 binary corresponding to the `lucid.p4` program. This can be seen in the output as the `SWITCHD COMMAND` line, e.g., 
    ```
    SWITCHD COMMAND: sudo env SDE=/home/jsonch/Desktop/bf-sde-9.13.0 SDE_INSTALL=/home/jsonch/Desktop/bf-sde-9.13.0/install PATH=/home/jsonch/Desktop/bf-sde-9.13.0/install/bin:/home/jsonch/.local/bin:/home/jsonch/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin LD_LIBRARY_PATH=/usr/local/lib:/home/jsonch/Desktop/bf-sde-9.13.0/install/lib: /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/bf_switchd --background --status-port 7777 --install-dir /home/jsonch/Desktop/bf-sde-9.13.0/install --conf-file /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid_tofino/lucid.conf
    ```
    The script assumes that the tofino build directory for `./<prog>.p4` is in `./<prog>_tofino`. The script will redirect all stdout from the switchd process to the current shell, prefixing each line with the `[SWITCHD_MGR]` tag. As you see in the log, output from both the model and switchd are interleaved.

4. **Starts the control plane python script.** The last step is to start the control plane script. This can be seen in the output as the `PYTHON CONTROL` line, e.g., 
    ```
    PYTHON CONTROL: /home/jsonch/Desktop/bf-sde-9.13.0/run_bfshell.sh -b /home/jsonch/Desktop/gits/lucid/tutorials/tofino/01Reflector/build/lucid.py
    ```
    The script will redirect all stdout from the control plane script to the current shell, prefixing each line with the `[PY_MGR]` tag. The most basic control plane script sets up multicast rules necessary to execute the "generate" statements in the Lucid program. 


#### Common errors

- If the script seems to hang when starting bf_switchd, it may be because hugepages are not configured correctly. ctrl-c to stop the script, make sure hugepages are configured correctly, and try again.

### Sending packets

The script will continue running the model and bf_switchd until you ctrl-c out of it. While its running, you should be able to send packets to the program via one of the veth interfaces listed at the end of the output. For example, in another window, cd to the `01Reflector` directory and run `sudo ./send_pkt.py veth257`. This will send a packet port 128 of the model. In the window where `p4tapp.sh` is running, you should see output from the simulator as it receives the packet from port 128, processes it, and sends it out of the same port. e.g.: 
```
[SIM] :07-29 13:06:35.503359:    :-:-:<0,-,0>:Begin packet processing
[SIM] :07-29 13:06:35.503546:    :0x1:-:<0,1,0>:========== Ingress Pkt from port 128 (64 bytes)  ==========
[SIM] :07-29 13:06:35.503600:    :0x1:-:<0,1,0>:Packet :
[SIM] :07-29 13:06:35.503656:        :0x1:-:<0,1,0>:04 8d 38 7f 66 b9 54 bf 64 93 17 a0 08 00 45 00
[SIM] :07-29 13:06:35.503710:        :0x1:-:<0,1,0>:00 14 00 01 00 00 40 00 6c a4 0a 00 00 40 01 02
[SIM] :07-29 13:06:35.503761:        :0x1:-:<0,1,0>:03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.503812:        :0x1:-:<0,1,0>:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.504114:    :0x1:-:<0,1,->:========== Packet to input parser: from port 128 (80 bytes)  ==========
[SIM] :07-29 13:06:35.504157:    :0x1:-:<0,1,->:Packet :
[SIM] :07-29 13:06:35.504209:        :0x1:-:<0,1,->:00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.504262:        :0x1:-:<0,1,->:04 8d 38 7f 66 b9 54 bf 64 93 17 a0 08 00 45 00
[SIM] :07-29 13:06:35.504313:        :0x1:-:<0,1,->:00 14 00 01 00 00 40 00 6c a4 0a 00 00 40 01 02
[SIM] :07-29 13:06:35.504366:        :0x1:-:<0,1,->:03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.504416:        :0x1:-:<0,1,->:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
...
...
...
[SIM] :07-29 13:06:35.530212:        :-:0x1:<0,1,->:Egress Deparser Headers:
[SIM] :07-29 13:06:35.531032:    :-:0x1:<0,1,0>:========== Tx Pkt to port 128 (64 bytes) ==========
[SIM] :07-29 13:06:35.531056:    :-:0x1:<0,1,0>:Packet :
[SIM] :07-29 13:06:35.531081:        :-:0x1:<0,1,0>:04 8d 38 7f 66 b9 54 bf 64 93 17 a0 08 00 45 00
[SIM] :07-29 13:06:35.531104:        :-:0x1:<0,1,0>:00 14 00 01 00 00 40 00 6c a4 0a 00 00 40 01 02
[SIM] :07-29 13:06:35.531126:        :-:0x1:<0,1,0>:03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.531147:        :-:0x1:<0,1,0>:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 13:06:35.531406:    :-:-:<0,-,0>:Waiting for packets to process
```
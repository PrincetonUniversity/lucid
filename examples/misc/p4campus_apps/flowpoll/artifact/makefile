# compile tofino binary.
build:
	$(SDE_INSTALL)/bin/bf-p4c --verbose 3 --std p4-16 --target tofino --arch tna -o `realpath ./lucid_tofino` --bf-rt-schema `realpath ./lucid_tofino/bf-rt.json` ./lucid.p4

# run in terminal window 1
start_model:
	sudo $(SDE_INSTALL)/bin/tofino-model --time-disable --p4-target-config `realpath ./lucid_tofino/lucid.conf` -d 1 -f sim_veth_map.json --chip-type 2 --install-dir $SDE_INSTALL --pkt-log-len 100000 --int-port-loop 196

# run in terminal window 2
start_bfswitchd:
# sudo env SDE=/home/jsonch/Desktop/bf-sde-9.13.0 SDE_INSTALL=/home/jsonch/Desktop/bf-sde-9.13.0/install PATH=/home/jsonch/Desktop/bf-sde-9.13.0/install/bin:/home/jsonch/.local/bin:/home/jsonch/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin LD_LIBRARY_PATH=/usr/local/lib:/home/jsonch/Desktop/bf-sde-9.13.0/install/lib: /home/jsonch/Desktop/bf-sde-9.13.0/install/bin/bf_switchd --background --status-port 7777 --install-dir /home/jsonch/Desktop/bf-sde-9.13.0/install --conf-file /home/jsonch/Desktop/gits/lucid/examples/misc/p4campus_apps/flowpoll/build_flowpoll/lucid_tofino/lucid.conf
	sudo env SDE=$(SDE) SDE_INSTALL=$(SDE_INSTALL) PATH=$(SDE_INSTALL)/bin:$(PATH) LD_LIBRARY_PATH=$(SDE_INSTALL)/lib:$(LD_LIBRARY_PATH) $(SDE_INSTALL)/bin/bf_switchd --background --status-port 7777 --install-dir $(SDE_INSTALL) --conf-file `realpath ./lucid_tofino/lucid.conf`

# run in terminal window 3
start_control:
	$(SDE)/run_bfshell.sh -b lucid.py

# run in terminal window 4
send_pcap:
	sudo tcpreplay --pps .2 --preload-pcap --quiet -i veth257 test_trace.pcap
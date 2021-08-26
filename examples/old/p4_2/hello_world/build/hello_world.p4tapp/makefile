P4TAPP := $(shell ./find_p4tapp.sh)
ifndef P4TAPP
$(error p4tapp.py could not be found.)
endif

# ./submodules/p4tapp/src/p4tapp.py -c -b ${DST} --dumpPasses -p4c " --table-placement-in-order --relax-phv-init 2";
# P4TAPP="../../submodules/p4tapp/src/p4tapp.py"

all: 
	$(P4TAPP) -cb . -p4c " --table-placement-in-order" #--relax-phv-init 2

verbose: 
	$(P4TAPP) -cb . -v --dumpPasses -p4c " --table-placement-in-order --relax-phv-init 2"

# skip phv allocation to see if the rest of the code compiles. 
trivpa:
	$(P4TAPP) -cb . -v -p4c " --table-placement-in-order --trivpa" #--relax-phv-init 2

# some other phv options? 
phvalt:
	$(P4TAPP) -cb . -v -p4c " --table-placement-in-order --nophvintf --noclusterintf --nophvslice --nophvover" #--relax-phv-init 2

# run ASIC sim, switchd, and control script.
runsim:
	`./find_p4tapp.sh` . sim -r

# generate a pcap to replay into running simulator.
pcapgensim: 
	`./find_p4tapp.sh` . sim -g

# send pcap into running simulator and collect output.
pcaptestsim:
	`./find_p4tapp.sh` . sim -t

clean: 
	$(P4TAPP) -c .

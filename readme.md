## Sigcomm 2021 Lucid Artifact

This branch contains a snapshot of the Lucid compiler from 5/2021 that reproduces the principle result from the Lucid paper at Sigcomm 2021, Figure 8, which reports the lines of code of 10 benchmark Lucid programs when compiled to Tofino-compatible P4.

The scripts and VM in this branch automate the compilation and analysis of these programs. For other applications and general use of Lucid, we recommend returning to the master branch, as Lucid is under active development and frequently updated. 

### Short instructions

Make sure you have virtualbox and vagrant installed and room for a vm that uses 2 cores, 4GB of ram, and 10GB of disk space. Run ``./vm_setup.sh; ./vm_reproduce.sh`` to setup a VM and compile the 10 benchmark programs from Figure 8 of the Lucid paper. This will take about 1.5 hours overall. When complete, the script will print out statistics about lines of code for the benchmark applications. 

### Longer instructions

0. Install [virtualbox](https://www.virtualbox.org/wiki/Downloads) (tested with virtualbox 6.1.8) and [vagrant](https://www.vagrantup.com/downloads) (tested with vagrant 2.2.9).

1. Set up the VM: ``./vm_setup.sh`` This will download a pre-built vagrant box, [lucid_vm.box](princeton.edu) and set it up as the default vagrant box in this directory. If the VM is not accessible, see ``./vagrant/readme.md`` to rebuild ``lucid_vm.box``.

2. Run the compiler and analysis: ``./vm_reproduce.sh`` This sshes into the VM set up in step 1, then runs ``sigcomm_apps/reproduce.sh`` to compile 10 benchmark applications from Lucid to P4. It will take about 1.5 hours to run and prints a bunch of output from the Lucid compiler. For more information about what ``reproduce.sh`` is doing, see ``sigcomm_apps/readme.md``. At the end of compilation, it should print out the following statistics about application lines of code. 
```
----- stats for chain_replication.dpt -----
lucid program:              ./chain_replication.dpt
lucid program loc:          97
p4-tofino program loc:      987
number of tofino stages:    ---
----- stats for countmin_historical.dpt -----
lucid program:              ./countmin_historical.dpt
lucid program loc:          93
p4-tofino program loc:      920
number of tofino stages:    ---
----- stats for stateful_fw.dpt -----
lucid program:              ./stateful_fw.dpt
lucid program loc:          189
p4-tofino program loc:      1889
number of tofino stages:    ---
----- stats for chain_prob_stateful_firewall.dpt -----
lucid program:              ./chain_prob_stateful_firewall.dpt
lucid program loc:          74
p4-tofino program loc:      999
number of tofino stages:    ---
----- stats for rip_single.dpt -----
lucid program:              ./rip_single.dpt
lucid program loc:          81
p4-tofino program loc:      855
number of tofino stages:    ---
----- stats for dnsguard.dpt -----
lucid program:              ./dnsguard.dpt
lucid program loc:          219
p4-tofino program loc:      1867
number of tofino stages:    ---
----- stats for starflow.dpt -----
lucid program:              ./starflow.dpt
lucid program loc:          156
p4-tofino program loc:      2017
number of tofino stages:    ---
----- stats for chain_prob_stateful_firewall_timeout.dpt -----
lucid program:              ./chain_prob_stateful_firewall_timeout.dpt
lucid program loc:          119
p4-tofino program loc:      1620
number of tofino stages:    ---
----- stats for rerouter.dpt -----
lucid program:              ./rerouter.dpt
lucid program loc:          115
p4-tofino program loc:      1076
number of tofino stages:    ---
----- stats for NAT.dpt -----
lucid program:              ./NAT.dpt
lucid program loc:          41
p4-tofino program loc:      738
number of tofino stages:    ---
```

3. (optional) If you have access to the Tofino SDE and want to use it to compile Lucid-generate P4 to the Tofino binaries, download a copy of ``bf-sde-9.5.0.tar`` and put it in the root directory of this git. Run ``./vm_install_sde.sh`` to install a copy of the SDE to the VM. SDE installation will take a while, as usual. Then, run ``./vm_reproduce.sh`` again. With the SDE installed, the script will also compile every benchmark Lucid app from P4 to the Tofino. This will take an additional 30 minutes - 1 hour. With the SDE installed, the expected final output is: 
```
----- stats for chain_replication.dpt -----
lucid program:              ./chain_replication.dpt
lucid program loc:          97
p4-tofino program loc:      987
number of tofino stages:    12
----- stats for countmin_historical.dpt -----
lucid program:              ./countmin_historical.dpt
lucid program loc:          93
p4-tofino program loc:      920
number of tofino stages:    6
----- stats for stateful_fw.dpt -----
lucid program:              ./stateful_fw.dpt
lucid program loc:          189
p4-tofino program loc:      1889
number of tofino stages:    10
----- stats for chain_prob_stateful_firewall.dpt -----
lucid program:              ./chain_prob_stateful_firewall.dpt
lucid program loc:          74
p4-tofino program loc:      999
number of tofino stages:    11
----- stats for rip_single.dpt -----
lucid program:              ./rip_single.dpt
lucid program loc:          81
p4-tofino program loc:      855
number of tofino stages:    9
----- stats for dnsguard.dpt -----
lucid program:              ./dnsguard.dpt
lucid program loc:          219
p4-tofino program loc:      1867
number of tofino stages:    11
----- stats for starflow.dpt -----
lucid program:              ./starflow.dpt
lucid program loc:          156
p4-tofino program loc:      2017
number of tofino stages:    12
----- stats for chain_prob_stateful_firewall_timeout.dpt -----
lucid program:              ./chain_prob_stateful_firewall_timeout.dpt
lucid program loc:          119
p4-tofino program loc:      1620
number of tofino stages:    10
----- stats for rerouter.dpt -----
lucid program:              ./rerouter.dpt
lucid program loc:          115
p4-tofino program loc:      1076
number of tofino stages:    9
----- stats for NAT.dpt -----
lucid program:              ./NAT.dpt
lucid program loc:          41
p4-tofino program loc:      738
number of tofino stages:    10
```
## Sigcomm 2021 Lucid Artifact

This branch contains a snapshot of the Lucid compiler from 5/2021 that reproduces the principle result from the Lucid paper at Sigcomm 2021, Figure 8, which reports the lines of code of 10 benchmark Lucid programs when compiled to Tofino-compatible P4.

The scripts and VM in this branch automate the compilation and analysis of these programs. For other applications and general use of Lucid, we recommend returning to the master branch, as Lucid is under active development and frequently updated. 

### Short instructions

Make sure you have [virtualbox](https://www.virtualbox.org/wiki/Downloads) and vagrant](https://www.vagrantup.com/downloads) installed and room for a vm that uses 2 cores, 4GB of ram, and 10GB of disk space. Download this [3GB vm image](https://drive.google.com/file/d/14h-8nJdGtYtNBrZoJim_c_wM-CFEQGbD/view?usp=sharing) and put it in the same directory as this readme. Run ``./vm_setup.sh; ./vm_reproduce.sh`` to setup a VM and compile the 10 benchmark programs from Figure 8 of the Lucid paper. This will take 1-2 hours. When complete, the script will measure and report line of code statistics for the benchmark applications. 

### Longer instructions

0. Install [virtualbox](https://www.virtualbox.org/wiki/Downloads) (tested with virtualbox 6.1.8) and [vagrant](https://www.vagrantup.com/downloads) (tested with vagrant 2.2.9).

1. Set up the VM. Download the pre-built vagrant box, [https://drive.google.com/file/d/14h-8nJdGtYtNBrZoJim_c_wM-CFEQGbD/view?usp=sharing](https://drive.google.com/file/d/14h-8nJdGtYtNBrZoJim_c_wM-CFEQGbD/view?usp=sharing) and save it in the root directory of this repository as ``lucidvm.box``. Then run ``./vm_setup.sh``. If the VM is not accessible, see ``./vagrant/readme.md`` to build ``lucid_vm.box``.

2. Run the compiler and analysis. Run ``./vm_reproduce.sh`` This sshes into the VM set up in step 1, then runs ``sigcomm_apps/reproduce.sh`` to compile 10 benchmark applications from Lucid to P4. It will take about 1.5 hours to run and prints a bunch of output from the Lucid compiler. For more information about what ``reproduce.sh`` is doing, see ``sigcomm_apps/readme.md``. After compilation, an analysis script will report the following statistics about the applications.
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

3. (optional) If you have access to the Tofino SDE and want to use it to compile Lucid-generate P4 to Tofino binaries, download a copy of ``bf-sde-9.5.0.tar`` and put it in the root directory of this git. Run ``./vm_install_sde.sh`` to install a copy of the SDE to the VM. After the SDE installs, run ``./vm_reproduce.sh`` again. The script will now compile the P4 version of each Lucid application into a Tofino binary. This will take an additional 30 minutes - 1 hour. With the SDE installed, the final output will also report the number of tofino stages that each application used based on the Tofino compiler's logs.
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

### The source code

Lucid's compiler source code is in ``/src/``. The compiler's main function is in ``/src/bin/Compiler.ml``. It calls modules in ``src/lib/``. Here are the libraries related to the parts of the compiler described in the paper: 

- syntax (Section 3): ``/src/lib/frontend/syntax.ml``

- type system (Section 4) and syntactic memop restrictions (Section 5.1): ``src/lib/frontend/typing/``

- translation to an intermediate language for the Tofino (Section 5.2): ``src/lib/backend/translate/`` (start with ``TofinoFromDpt.ml``)

- graph-based optimizations (Section 5.3): ``src/lib/backend/optimization/``


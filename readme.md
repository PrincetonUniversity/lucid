## Sigcomm 2021 Lucid Artifact

This branch contains a snapshot of the Lucid compiler from 5/22/2021 that reproduces the principle result from the Lucid paper at Sigcomm 2021. The main function of this artifact is to reproduce Figure 8 from the paper, by compiling 10 benchmark Lucid programs (found in ``./apps``).

For other applications and general use of Lucid, we recommend returning to the master branch, as Lucid is under active development and frequently updated. 

### Requirements: 

  - [virtualbox](https://www.virtualbox.org)
  
  - [vagrant](https://www.vagrantup.com)

### Instructions: 

1. Download the vm: [vm link]( ). 

2. Initialize the vm: from the vm's directory, run ``vagrant ...``. If you would rather rebuild the VM from scratch, see ``vagrant/readme.md``

3. Run the command to rebuild the 10 benchmark Lucid programs and report statistics about them: ``vagrant ssh -c ~/lucid/sigcomm_apps/reproduce.sh`` (from the vm's directory) **this takes around 1 - 2 hours to complete!**

For more details on what this script is doing, see ``sigcomm_apps/readme.md``.

The final output will be statistics about the benchmark applications. 

```

```

4. (optional) To get information about the number of pipeline stages required by each application, you will need access to the Tofino 9.5.0 SDE. Put the 9.5.0 SDE file ``bf-sde-9.5.0.tar`` in the same directory as the VM file. Inside of the VM, run ``~/lucid/sigcomm_apps/utils/install_bf_sde.sh`` to start the install. The sde installs to ``~/bf-sde-9.5.0``. If you install the sde, make sure to also set the $SDE and $SDE_INSTALL environment variables. After installing the sde, ``~/lucid/sigcomm_apps/reproduce.sh`` will also compile the applications from P4 to Tofino binaries and report the number of tofino pipeline stages required.

Here is the expected output, with the sde installed: 

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
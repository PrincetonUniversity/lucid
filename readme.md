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

Here is the expected final output 
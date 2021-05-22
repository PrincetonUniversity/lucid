## Sigcomm 2021 Lucid Artifact

This branch of the repository contains an environment to reproduce the principle result from the Lucid paper at Sigcomm 2021. The main function of this artifact is to compile the 10 benchmark programs from Lucid to Tofino-compatible P4 and measure the Lucid and P4 program lines of code. A secondary function, if you have access to the Tofino SDE, is to compile the P4 to a Tofino binary.

### Requirements: 

  - [virtualbox](https://www.virtualbox.org)
  
  - [vagrant](https://www.vagrantup.com)

### Instructions: 

1. Download the vm: [vm link]( ). To rebuild the VM from scratch, see ``vagrant/readme.md``

2. Setup the vm: ``vagrant ...`` (from the vm's directory) 

3. Run the command to rebuild the 10 benchmark Lucid programs and report statistics about them: ``vagrant ssh -c ~/lucid/sigcomm_apps/reproduce.sh`` (from the vm's directory) **this takes around 1 - 2 hours to complete!**

For more details on what this script is doing, see ``sigcomm_apps/readme.md``.

Expected final output are statistics about the lines of code of each application in Lucid and P4. 

```

```

4. (optional) If you want to compile Lucid-generated P4 to a Tofino binary, to also get the number of pipeline stages required by each application, you will need access to the Tofino SDE. Put the 9.5.0 SDE file ``bf-sde-9.5.0.tar`` in the same directory as the VM file. Inside of the VM, run ``~/lucid/sigcomm_apps/utils/install_bf_sde.sh`` to start the install. The sde installs to ``~/bf-sde-9.5.0``. If you install the sde, make sure to also set the $SDE and $SDE_INSTALL environment variables. After installing the sde, ``~/lucid/sigcomm_apps/reproduce.sh`` will also compile the applications from P4 to Tofino binaries and report the number of tofino pipeline stages required.
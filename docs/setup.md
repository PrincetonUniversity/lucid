# Setting up Lucid

This page briefly describes how to set up a Lucid environment in a virtualbox VM or from scratch. We have tested Lucid on OSX and Ubuntu 18.04. 


## In a VM
The easiest way to use Lucid is to build a vagrant virtualbox VM. You can either build a VM that runs Lucid's interpreter and P4 compiler, or you can build a VM that runs Lucid's interpreter, P4 compiler, and also the Tofino sde required to compile that P4 into a Tofino binary and run it on a model of the Tofino ASIC. This is recommended if you want to follow along with the tutorials. 

Before proceeding, make sure you have virtualbox and vagrant installed. 

The VM build process takes an hour or two on a laptop, especially if you are also compiling p4 studio.

### To build a lucid only VM

In ``./vm``, run ``buildbox.sh interpreter`` to build ``lucid.box`` -- a vagrant box / appliance. Then, run ``setupvm.sh`` to create a local vm from the box. 

### To build a lucid + tofino sde VM

1. Download a copy of bf-sde-9.5.0.tgz from Intel. *(bf-sde-9.5.1.tgz should also work, but it is untested)*

2. Put it in ``./vm``

3. In ``./vm``, run ``buildbox.sh compiler bf-sde-9.5.1.tgz`` to build ``lucid.box`` -- a vagrant box / appliance. Then, run ``setupvm.sh`` to create a local vm from the box. 

### Using the VM

Once the VM is built, use ``vagrant ssh`` from the ``vm`` directory to ssh into the vm. This git is shared in ``/lucid``, so run ``cd /lucid; make`` to compile Lucid.

## From scratch

**Ubuntu 18.04** To install Lucid and p4 studio on an ubuntu 18.04 machine (or your own VM), see the script ``vm/lucidbox/setup_ubuntu.sh`` for installing dependencies. Then run ``make`` in the root directory of this git. 

**Other platforms** To install Lucid on other platforms (e.g., OSX), 

1. Install opam and ocaml. 

2. Install the ocaml packages that Lucid requires: 
```
opam install -y \
    integers \
    batteries \
    ounit \
    ansiterminal \
    menhir \
    ppx_deriving \
    ppx_deriving_argparse \
    ppx_string_interpolation \
    zarith \
    visitors \
    fileutils \
    ppx_import \
    core \
    dune \
    ocamlgraph \
    z3 \
    yojson \
    angstrom
```

3. Finally run ``make`` in the root directory of this git.

**notes**: 
1. Tested with ocaml 4.11.1. 
2. You may need to also install other libraries for these packages to install. 
3. These "from scratch" steps only install dependencies for Lucid, not the tofino sde. 
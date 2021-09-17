# Setting up Lucid

This page briefly describes how to set up a Lucid environment in a virtualbox VM or from scratch. We have tested Lucid on OSX and Ubuntu 18.04. 


## In a VM
The easiest way to use Lucid is to build a vagrant virtualbox VM. You can either build a VM that runs Lucid's interpreter and P4 compiler, or you can build a VM that runs Lucid's interpreter, P4 compiler, and also the Tofino sde required to compile and run that P4 on the Tofino. The latter VM is recommended if you want to follow along with the tutorials. 

Before proceeding, make sure you have virtualbox (https://www.virtualbox.org/wiki/Downloads) and vagrant (https://www.vagrantup.com/downloads) installed. 

The VM build process takes an hour or two on a laptop, especially if you are also compiling p4 studio.

*Note: if you have a Princeton google account, you can download a pre-built VM image with Lucid and the Tofino SDE here: https://drive.google.com/file/d/1wu8PjGdebsHAlj6JwlX0iWp8IFmkx38j/view?usp=sharing. Download that, put it in ``/vm``, and skip to "using the VM".*

### To build a lucid only VM

In ``./vm``, run ``buildbox.sh interpreter`` to build ``lucid.box`` -- a vagrant box / appliance. Then, run ``setupvm.sh`` to create a local vm from the box. 

### To build a lucid + tofino sde VM

1. Download a copy of bf-sde-9.5.0.tgz from Intel. *(bf-sde-9.5.1.tgz should also work, but it is untested)*

2. Put it in ``./vm``

3. In ``./vm``, run ``buildbox.sh compiler bf-sde-9.5.1.tgz`` to build ``lucid.box`` -- a vagrant box / appliance. 

### Using the VM

Once the base vm box is build, in ``./vm`` run ``setupvm.sh`` to create a local vm from the box. Access the vm by running ``vagrant ssh`` from the ``vm`` directory. Inside the vm, the git is shared in ``/lucid``. 

### Testing the build environment

After sshing into the vm, you should test the build environment.

1. compile lucid: 
```
cd /lucid; make
```
3. test the installation by running all four make commands for the tutorial: 
```
cd /lucid/examples/tutorial
make interp
make compile
make assemble
make test
```

If all of the above make commands succeed, the vm has been set up correctly.

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

3. Finally run ``make`` in the root directory of this git to build lucid.

**notes**: 
1. Tested with ocaml 4.11.1. 
2. You may need to also install other libraries for these packages to install. 
3. These "from scratch" steps only install dependencies for Lucid, not the tofino sde. 

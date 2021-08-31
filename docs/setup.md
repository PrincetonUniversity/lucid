# Setting up Lucid

This page briefly describes how to set up a Lucid environment in a virtualbox VM or from scratch. We have tested Lucid on OSX and Ubuntu 18.04. 


## In a VM
The easiest way to use Lucid is to build a vagrant virtualbox VM. In ``./vm``, run ``buildbox.sh`` to build ``lucid.box`` -- a vagrant box / appliance. Then, run ``setupvm.sh`` to create a local vm from the box.

If you want to compile Lucid-generated P4 to the Tofino in your VM, get a copy of the p4 studio 9.5.0 and put ``bf-sde-9.5.0.tgz`` in ``./vm`` before running ``buildbox.sh`` or ``setupvm.sh``. This is recommended if you want to follow along with the tutorials. 

The VM build process takes an hour or two on a laptop, especially if you are also compiling p4 studio.

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
**notes**: Tested with ocaml 4.11.1. You may need to also install other libraries for these packages to install. 

3. Finally run ``make`` in the root directory of this git.

Also note that, while the steps install the dependencies for Lucid, they don't install p4 studio for compiling Lucid-generated P4 to the tofino. 

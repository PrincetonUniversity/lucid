# Setting up Lucid

This page briefly describes how to set up a Lucid environment in a virtualbox VM or from scratch. We have tested Lucid on OSX and Ubuntu 18.04. 


## In a VM
The easiest way to use Lucid is to set up a virtualbox vm based on a vagrant box that contains all the Lucid requirements. There are two boxes: an *interpreter box* that only has the requirements to run the Lucid toolchain itself; and a *tofino compiler box* that has the requirement to run Lucid's toolchain, and also the underlying P4-Tofino compiler. Use the compiler box if you have access to the Tofino's toolchain and want to actually compile the P4 that Lucid produces. Otherwise, use the interpreter box. 

Before proceeding, make sure you have virtualbox (https://www.virtualbox.org/wiki/Downloads) and vagrant (https://www.vagrantup.com/downloads) installed. 

### With a pre-built base box

First, you need to either download or build the lucid box. It is easier to use the pre-build boxes if possible. We update these boxes whenever lucid's library requirements change. 

Pre-built boxes are available at: 

- The interpreter box: [lucidinterpreter.box](https://drive.google.com/file/d/1bIQXSOM4vZfL3hcz2JJhXKWxNHTNAtpk/view?usp=sharing)

- The compiler box: [lucidcompiler.box](https://drive.google.com/file/d/1wu8PjGdebsHAlj6JwlX0iWp8IFmkx38j/view?usp=sharing) **Note: to download the compiler box from this link, you must have a Princeton google drive account.**

To use either of these boxes, place the downloaded file (lucidinterpreter.box or lucidcompiler.box) in the ``vm`` directory of this git and run ``installbox.sh <interpreter | compiler>``. This will use vagrant to install the selected box. Then, skip to the section **using the vm**.

**note: boxes are currently global -- you cannot have both the interpreter and compiler box installed at once. However, installbox.sh can switch between them.**


#### Building the base box

You can also build lucidinterpreter.box and lucidcompiler.box from scratch. The process may take an hour or two, and assumes you have at least 6 free CPU cores. 

To build the interpreter box (``lucidinterpreter.box``), just run ``buildbox.sh interpreter`` in the ``vm`` directory. ``installbox.sh interpreter`` will then install the box. 

To build the compiler box (``lucidcompiler.box``): 

1. Download a copy of bf-sde-9.5.0.tgz from Intel. *(bf-sde-9.5.1.tgz should also work, but it is untested)*

2. Put it in ``./vm``

3. In ``./vm``, run ``buildbox.sh compiler bf-sde-9.5.0.tgz`` to build ``lucidcompiler.box``.

4. Run ``installbox.sh compiler`` to install the box.


### Using the VM

After running ``installbox.sh``, you can use the vm with standard vagrant commands from the ``vm`` directory:

- ``vagrant up`` launches the vm, and initializes it if it has never been initialized before. 
- ``vagrant ssh`` logs into the vm.

Inside the vm, the git from the host machine is shared in ``/lucid``. 

#### Testing the build environment

After sshing into the vm, you should test the build environment.

1. compile lucid: 
```
cd /lucid; make
```
2. test the installation by running all four make commands for the tutorial: 
```
cd /lucid/examples/tutorial
make interp
make compile
make assemble
make test
```

**Note: if you are using the interpreter vm, only make interp and make compile will work.**


## Setting up Lucid on a custom machine

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

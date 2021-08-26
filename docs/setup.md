# Setting up Lucid

This page briefly describes how to set up a Lucid environment on a virtualbox VM or bare metal. 

## Prerequisites

Lucid depends on a number of ocaml packages, mostly ocaml libraries. You can set up these dependencies in a VM, or on a bare metal machine.  

### Preparing a VM
The easiest way to use Lucid is to build a vagrant virtualbox VM. Before setting up the VM, if you want to compile Lucid to P4,  get a copy of the p4 studio 9.5.0 and put``bf-sde-9.5.0.tgz`` in \vagrant. 
Set up the vm with: 
```
cd vagrant; 
vagrant up
```
This will take an hour or two, especially if it is also compiling p4-studio.

### From scratch
If you want to install the Lucid prerequisites from scratch, first install opam and ocaml. Then run these commands: 

```
opam init -a -y --compiler=4.11.1 
# opam switch create 4.11.1
eval $(opam env)
echo "eval 'opam config env'" >> ~/.bashrc
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
eval $(opam env)
```
You may need to also install other libraries for these packages to install. For ubuntu 18.04, ``vagrant/setup_ubuntu.sh`` installs all the required libraries. Also, to compile Lucid to the Tofino, you will also need a local install of p4studio 9.5.0. 

## Building
After either installing the prerequirements or sshing into the vm, build lucid by running ``make`` in the root directory of this repository.

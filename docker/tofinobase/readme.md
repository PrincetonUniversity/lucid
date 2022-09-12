This is a docker image designed to compile 
and run Lucid-generated P4 on the official 
Tofino verification asic model. The image 
extends jsonch/lucid:ocamlbase. It includes:
    - ubuntu 18.04
    - ocaml + lucid build dependencies
    - tofino sde, built from a local file

Rebuild with: 

``docker build -t tofinobase .``


Requirements to build: around 50GB of free space in your docker VM


Usage: 
1. Put a copy of bf-sde-9.7.2.tgz in this directory. 
2. run ``/tofino_dev.sh`` to ssh into the vm 
   (and build it if it does not exist)
3. access the lucid repo from your host machine in 
   ``/lucid``

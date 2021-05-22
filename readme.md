# The Lucid / DPT language

## Installing Lucid prerequisites
*Last updated: 2/18/21*

Lucid requires ocaml >= 4.11.1, a bunch of ocaml libraries, and a few other libraries.

### Virtualbox / vagrant

``dev_vms/vagrant`` has a Vagrantfile and script to set up a VM with ubuntu 18.04 and the necessary ocaml environment. You should be able to build the Lucid compiler with:

```
cd dev_vms/vagrant
vagrant up
vagrant ssh
cd /vagrant/
git clone https://github.com/PrincetonUniversity/dpt
cd dpt
make
```

### Local installation

0. install dependencies

- `libgmp` and (likely already installed) `python3`

If you use OSX, you can install this with homebrew (https://brew.sh) or macports. If you don't have either, the easiest way is probably with homebrew:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install gmp

```


1. install opam and ocaml

```
# follow the "Binary Distribution" instructions:
# https://opam.ocaml.org/doc/Install.html
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
opam init -a -y
eval $(opam env)
opam switch create 4.11.1
eval $(opam env)
```

2. install ocaml packages (this may take a while)

```
opam install -y \
 	integers \
 	batteries \
 	ounit \
 	ansiterminal \
 	menhir \
 	ppx_deriving \
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

In general, you can see which ocaml packages you're missing to run dpt using dune:
```dune external-lib-deps --missing @all```


## Using the Lucid interpreter

1. build the compiler and interpreter

```make```

2. run the interpreter, `./dpt`, which takes two arguments: a .dpt program and a configuration file preceded by the `-spec` flag.

```./dpt examples/interp_tests/NAT.dpt -spec examples/interp_tests/NAT.json```

You should see something like:
```
dpt: Parsing ...
---------typing-------------
-----------renaming-----------
-----------inlining-----------
---------------typing again-------------
------------Checking entry handlers---------------

In examples/interp_tests/NAT.dpt:

53|    generate add_to_nat(src_ip, src_port); // Will generate the continue for us
                      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

warning: examples/interp_tests/NAT.dpt: Conditional generation of potential non-exit event in entry handler.

---------------------------

dpt: Simulating...

dpt: Using random seed: 0

t=0: Handling outside_packet~104(10u32)@[0u32] at switch 0
Mapped port 10 to (ip: 0, port: 0)
dropped
t=100000: Handling outside_packet~104(12u32)@100000ms@[0u32] at switch 0
Mapped port 12 to (ip: 0, port: 0)
dropped
t=200000: Handling outside_packet~104(14u32)@200000ms@[0u32] at switch 0
Mapped port 14 to (ip: 0, port: 0)
dropped
t=300000: Handling inside_packet~102(10u32,100u32)@300000ms@[0u32] at switch 0
Adding to NAT
t=300600: Handling add_to_nat~106(10u32,100u32)@self at switch 0
Mapped (ip: 10, port: 100) to port 12
t=400000: Handling inside_packet~102(10u32,100u32)@400000ms@[0u32] at switch 0
IP already in NAT, maps to port 12
t=500000: Handling outside_packet~104(10u32)@500000ms@[0u32] at switch 0
Mapped port 10 to (ip: 0, port: 0)
dropped
t=600000: Handling outside_packet~104(12u32)@600000ms@[0u32] at switch 0
Mapped port 12 to (ip: 10, port: 100)
t=700000: Handling outside_packet~104(14u32)@700000ms@[0u32] at switch 0
Mapped port 14 to (ip: 0, port: 0)
dropped
t=800000: Handling inside_packet~102(11u32,101u32)@800000ms@[0u32] at switch 0
Adding to NAT
t=800600: Handling add_to_nat~106(11u32,101u32)@self at switch 0
Mapped (ip: 11, port: 101) to port 14
t=900000: Handling outside_packet~104(10u32)@900000ms@[0u32] at switch 0
Mapped port 10 to (ip: 0, port: 0)
dropped
t=1000000: Handling outside_packet~104(12u32)@1000000ms@[0u32] at switch 0
Mapped port 12 to (ip: 10, port: 100)
t=1100000: Handling outside_packet~104(14u32)@1100000ms@[0u32] at switch 0
Mapped port 14 to (ip: 11, port: 101)

dpt: Final State:

Switch 0 : {

 Pipeline : [
    0 : [0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 10u32; 0u32; 11u32; 0u32]
    1 : [0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 0u32; 100u32; 0u32; 101u32; 0u32]
  ]

 Events :   [ ]

 Exits :    [
    inside_continue~103(12u32)@[0u32]
    inside_continue~103(12u32)@[0u32]
    outside_continue~105(10u32,100u32)@[0u32]
    inside_continue~103(14u32)@[0u32]
    outside_continue~105(10u32,100u32)@[0u32]
    outside_continue~105(11u32,101u32)@[0u32]
  ]

}

dpt: Done
```

*Note: you can also just pass the interpreter a program, and it will look for a configuration file with the same name in the same directory. For example, instead of the above command we can also use:*

```./dpt examples/interp_tests/NAT.dpt```

3. check the wiki for examples and more information (work in progress).

### Lucid programs

Lucid source renders decently as C in editors. Examples in ```examples/interp_tests``` and ```examples/tutorial``` run in the compiler and demonstrate features. Examples in ```sigcomm_apps``` compile to the Tofino. (There are a few features of Lucid not yet supported by the Tofino backend).

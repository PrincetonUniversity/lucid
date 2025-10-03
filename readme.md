# The Lucid / DPT (data plane threads) language

Lucid is a high-level language for programming high-performance data planes. Its designed to feel more like a regular programming language than P4, with simple, general, and modular abstractions. At the same time, Lucid is carefully designed so that its compiler can translate programs into efficient code optimized for real platforms. 

An important idea in Lucid is pipeline parallelism. Unlike other languages, Lucid guarantees that a program can be pipelined in its type-and-effect. In other words, any type-checking program is guaranteed to always access persistent objects in the same order. This is necessary on targets like the Tofino that are physical pipelines. On other targets it allows the compiler to use pipelining to exploit parallelism safely (without changing program semantics).

Lucid also has an interpreter with support for the entire language and fast, multi-node simulations. This makes it a convenient modeling language for data-plane applications. 

The Lucid codebase is also organized for extensibility, with a modular micropass design and frontend / midend / backend components that use a few common core IRs throughout.


## Getting Started

The best way to get started with Lucid is to compile it from source (instructions below), download the ([Visual Studio syntax highlighting extension](https://github.com/benherber/Lucid-DPT-VSCode-Extension)), and then follow the [tutorials](https://github.com/PrincetonUniversity/lucid/tree/main/tutorials/readme.md). 

## Installation 

Compiling Lucid from source requires installing ocaml and a few dependencies from its package manager. 

1. install opam

Opam is ocaml's package manager. Install it from source with:
```
bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
```
Or follow the instructions here: [opam install](https://opam.ocaml.org/doc/Install.html)


2. add a ocaml 4.12 switch

Lucid is pinned to ocaml 4.12 due to a few minor API changes in later versions. 

```
opam switch create 4.12.0
opam switch 4.12.0
```

3. install the Lucid dependencies

From the root of this repo, run:
```
opam install --deps-only .
``` 

4. build lucid with make

```
make
```

### Install script

On a linux machine (with sudo access), you should be able to run: 
```
./scripts/utils/install_dependencies.sh
make
```
to install all the dependencies from scratch (i.e., including opam, ocaml, etc.) and build the Lucid interpreter (`dpt`) and tofino compiler (`dptc`).

## Docker image
There is also a small docker image for Lucid. Its designed to be as simple as possible to install, but not always up to date.

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository and pull the lucid docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./docker_lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

Once finished, you can run `./docker_lucid.sh interpret` to run the interpreter or `./docker_lucid.sh compile` to run the Tofino compiler. The `docker_lucid` script takes care of forwarding all arguments, files, and directories to / from the docker image.


### Syntax highlighting
There is a VSCode syntax highlighter for Lucid here: ([https://github.com/benherber/Lucid-DPT-VSCode-Extension](https://github.com/benherber/Lucid-DPT-VSCode-Extension))

Lucid also renders okay as C (besides polymorphic size arguments with the single quote symbol).


### What to look at next

The [tutorials](https://github.com/PrincetonUniversity/lucid/tree/main/tutorials/readme.md) covers basic language features and using the interpreter and compiler. 

The [interpreter test programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests) has examples that cover every language feature, with some documentation. 

There are re-usable [data structure libraries](https://github.com/PrincetonUniversity/lucid/tree/main/examples/library) that may be useful for your application, and also illustrate how to package Lucid libraries as modules with clean interfaces.

There are a few [larger example applications](https://github.com/PrincetonUniversity/lucid/tree/main/examples/apps) that give you a sense of some of the cool things you can do with Lucid. 

The [examples directory](https://github.com/PrincetonUniversity/lucid/tree/main/examples) also has some other subdirectories with misc examples. Some may be out of date, though.

Finally, Lucid's [wiki](https://github.com/PrincetonUniversity/lucid/wiki) documents all of Lucid's well-supported features. 


## Publications

There are also a number of publications about Lucid or building higher-level languages on top of it.

### About Lucid or its components:

- Lucid, a Language for Control in the Data Plane ([SIGCOMM 2021](https://conferences.sigcomm.org/sigcomm/2021/program.html) -- The SIGCOMM 2021 artifact is in the ``sigcomm21_artifact`` branch).

- Safe, modular packet pipeline programming ([POPL 2022](https://dl.acm.org/doi/pdf/10.1145/3498699))

- Lucid, a high-level easy-to-use dataplane programming language ([Devon K. Loehr's PhD Thesis](https://dkloehr.github.io/files/Thesis.pdf))

- Automated Optimiation of Parameterized Data-Plane Programs with Parasol ([arxiv preprint](https://arxiv.org/pdf/2402.11155))

### Building on Lucid: 

- SwitchLog: A Logic Programming Language for Network Switches ([PADL 2023](https://par.nsf.gov/servlets/purl/10430321))

- Sequence Abstractions for Flexible, Line-Rate Network Monitoring." ([NSDI 2024](https://www.usenix.org/system/files/nsdi24-johnson.pdf))

## Citation

For citations, please use the SIGCOMM 2021 paper: 

```
Sonchack, John, Devon Loehr, Jennifer Rexford, and David Walker. "Lucid: A language for control in the data plane." In Proceedings of the 2021 ACM SIGCOMM 2021 Conference, pp. 731-747. 2021.
```

```
@inproceedings{lucid,
  title={Lucid: A language for control in the data plane},
  author={Sonchack, John and Loehr, Devon and Rexford, Jennifer and Walker, David},
  booktitle={Proceedings of the 2021 ACM SIGCOMM 2021 Conference},
  pages={731--747},
  year={2021}
}
```
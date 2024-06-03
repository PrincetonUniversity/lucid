This is a work in progress tutorial for Lucid. For now, it contains a series of examples that demonstrate the core parts of Lucid. There will be two series of tutorials: 

- `./interp` : developing Lucid applications with the interpreter
- `./tofino`: compiling and running Lucid applications to the Tofino. (This is not written yet, for now please reference the [wiki](https://github.com/PrincetonUniversity/lucid/wiki/03-Lucid-tofino-compiler) for documentation on compiling to the tofino.)

If you are new to Lucid, take a look at the instructions below.

### Getting started

The easiest way to use lucid is with its docker image and the lucid.sh script. 

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository and pull the lucid docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
./lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

**3. Test the lucid.sh script with the first example.** 

To make sure everything is working correctly, run this command from the root of the repository:

`./lucid.sh interp ./tutorials/interp/01monitor/monitor.dpt`

You should see a bunch of output from the interpreter, ending with a summary of events handled in an execution of it.

**4. (optional) set up your IDE**

If you use an IDE or text editor with syntax highlighting, basic lucid source files render decently with `c` syntax highlighting. Lucid source files typically end in `.dpt`. 

If you are using visual studio, there is a nice custom lucid syntax highlighter here: https://github.com/benherber/Lucid-DPT-VSCode-Extension

**5. Look at the example applications**

Finally, take a look at the examples applications, starting with [interp/01monitor](https://github.com/PrincetonUniversity/lucid/tree/main/tutorials/interp/01monitor). 

For more information about any topic covered in the tutorial, look at the [lucid wiki](https://github.com/PrincetonUniversity/lucid/wiki).

For more examples to play around with, look at the suite of [interpreter test programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests), [data structure library](https://github.com/PrincetonUniversity/lucid/tree/main/examples/library), and [other examples](https://github.com/PrincetonUniversity/lucid/tree/main/examples).


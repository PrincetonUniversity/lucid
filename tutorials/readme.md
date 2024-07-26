This is a work in progress tutorial for Lucid. For now, it contains a series of examples that demonstrate the core parts of Lucid. There will be two series of tutorials: 

- `./interp` : developing Lucid applications with the interpreter
- `./tofino`: compiling and running Lucid applications to the Tofino. (This is not written yet, for now please reference the [wiki](https://github.com/PrincetonUniversity/lucid/wiki/03-Lucid-tofino-compiler) for documentation on compiling to the tofino.)

If you are new to Lucid, take a look at the instructions below.

### Getting started

#### Building from source
The best way to use Lucid is to build it from source. From the root of the repo, run: 
```
./install_dependencies.sh
make
```
This will build: 

1. `./dpt`, the interpreter

2. `./dptc`, the tofino compiler

#### Docker image
Alternately, there is also a docker image that you can download by running: 

```
./docker_lucid.sh pull
```

The image is about 400MB of data and should take < 5 minutes. 

Run the interpreter and compiler inside the docker image by calling: 

1. `./docker_lucid.sh interp`

2. `./docker_lucid.sh compile`

### Testing the interpreter

To make sure everything is working correctly, run this command from the root of the repository (assuming you built lucid from source):

`./dpt ./tutorials/interp/01monitor/monitor.dpt`

You should see a bunch of output from the interpreter, ending with a summary of events handled in an execution of it.

### (optional) set up your IDE

If you use an IDE or text editor with syntax highlighting, basic lucid source files render decently with `c` syntax highlighting. Lucid source files typically end in `.dpt`. 

If you are using visual studio, there is a nice custom lucid syntax highlighter here: https://github.com/benherber/Lucid-DPT-VSCode-Extension

### Look at the example applications

Finally, take a look at the examples applications, starting with [interp/01monitor](https://github.com/PrincetonUniversity/lucid/tree/main/tutorials/interp/01monitor). 

For comprehensive details about all of the language's features, look at the [lucid wiki](https://github.com/PrincetonUniversity/lucid/wiki).

For more examples, look at the suite of [interpreter test programs](https://github.com/PrincetonUniversity/lucid/tree/main/examples/interp_tests), [data structure library](https://github.com/PrincetonUniversity/lucid/tree/main/examples/library), and [other examples](https://github.com/PrincetonUniversity/lucid/tree/main/examples).


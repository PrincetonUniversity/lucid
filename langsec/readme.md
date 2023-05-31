### langsec project directory

This is a working directory for the summer REU project, **Language-based security and privacy for network programming**

**Initial contents**

- `examples` -- example programs to introduce components of lucid


**Getting started**

The easiest way to use lucid is with its docker image and the lucid.sh script. 

**1. Install docker**
  - if you are on a laptop/desktop, just install the docker desktop app: [docker desktop](https://www.docker.com/products/docker-desktop/)
  - if you are on a server... you can probably figure out how to install docker

**2. Clone this repository, switch to the langsec23 branch, and pull the lucid docker container**

Run this in your terminal:
```
git clone https://github.com/PrincetonUniversity/lucid/
cd lucid
git checkout langsec23
./lucid.sh pull
```

This will download about 400MB of data and should take < 5 minutes. 

**3. Test the lucid.sh script with the first example.** 

To make sure everything is working correctly, run this command from the root of the repository:

`./lucid.sh interp ./langsec/examples/01monitor/monitor.dpt`

You should see some messages from the lucid compilter and a short trace of output from the interpreter, ending with something like: 

```
dpt: Simulating...
dpt: Using random seed: 1685567342

t=0: Handling packet event eth_ip(11,22,2048,1,2,128) at switch 0, port 0
t=600: Handling event prepare_report(11,22,2048,1,2,128) at switch 0, port 196
sending report about packet {src=1; dst=2; len=128} to monitor on port 2
dpt: Final State:

Switch 0 : {

 Pipeline : [ ]

 Events :   [ ]

 Exits :    [
    eth_ip(11,22,2048,1,2,128) at port 1, t=0
    report(1,2,128) at port 2, t=600
  ]

 Drops :    [ ]

 packet events handled: 1
 total events handled: 2

}
```

**4. (optional) set up your IDE**

If you use an IDE or text editor with syntax highlighting, basic lucid source files render decently with `c` syntax highlighting. Lucid source files typically end in `.dpt`. 

If you are using visual studio, there is a nice custom lucid syntax highlighter here: https://github.com/benherber/Lucid-DPT-VSCode-Extension


**5. Look at the example applications**

Finally, take a look at the examples applications. 
- Start with `langsec/examples/01monitor/readme.md`.
- For more advanced applications, look at the programs in `examples/interp_tests` and `examples/tofino_apps/src`.
- For documentation on most language features, look at the wiki: [lucid wiki](https://github.com/PrincetonUniversity/lucid/wiki)


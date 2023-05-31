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

You should see a trace of output from the interpreter. 

**4. Look at the example applications.**

Finally, take a look at the examples applications, starting with `./langsec/examples/01monitor/readme.md`
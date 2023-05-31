## monitor.dpt

This is a simple monitoring application that runs on a switch. When packets arrive at the switch, they are automatically converted into `eth_ip` events, which are handled by the `eth_ip` handler. The `eth_ip` handler performs two functions: 
	1) **forwarding**: it sends every packet out of port 1 by using the `generate_port` command to generate an `eth_ip` event on port 1.
	2) **basic monitoring**: it generates a report summarizing each packet, and sends it to a monitoring server on port 2.


### Lucid basics

Before running the example, it is useful to understand some basics about the lucid language and its interpreter.

**Core Lucid abstractions**
First, please read through the `monitor.dpt` file. The comments describe the general processing model of lucid and the two most basic lucid abstractions: *events* and *handlers*. 

*A note on syntax highlighting: lucid source files render decently with `c` syntax highlighters. If you are using visual studio, there is a custom lucid syntax highlighter here: https://github.com/benherber/Lucid-DPT-VSCode-Extension*

**The Lucid interpreter**

The Lucid interpreter type checks a lucid program and optionally runs the program in a simulated network. You configure the simulation with the *interpreter specification file*, which is typically defined in `<progname>.dpt`. So, for this example `monitor.dpt`, the interpreter runs on `monitor.json`. 

`monitor.json` is very simple: 
```
{
  "switches": 1,
  "max time": 9999999,
  "events": [
    {"name":"eth_ip", "args": [11, 22, 2048, 1, 2, 128]}
  ]
}
```
- "switches" tells the interpreter how many switches are in the network. If there are more than 1, you will also need to specify the topology of the network by defining links between switches. A later example will cover that. 

- "max time" defines when the simulation ends, in _nanoseconds_.
- "events" is a list of input events to the simulated network. Each event record has a name and a list of arguments. For events that have record types as arguments (e.g., `eth_ip` has an argument of type `eth_t`, which itself contains multiple fields), the arguments in the json are flattened. For example, the event in `monitor.json`: `{"name":"eth_ip", "args": [11, 22, 2048, 1, 2, 128]}` is an `eth_ip` event with arguments `eth#dmac = 11; eth#smac = 22; eth#etype = 2048; ip#src = 1; ip#dst = 2; ip#len = 128;`


#### Usage

Finally, to run the program, make sure that you have docker and the lucid-docker image installed, then run `make interp`.

**Expected output**

Here's the expected output from `make interp`. We will break it down below.
```
jsonch@johnshack3:~/Desktop/langsec23/lucid/langsec/progs/monitor$ make interp
program: monitor.dpt
sources: monitor.dpt
no interp spec file provided
checking if spec exists: monitor.json
spec exists, adding args
running command: docker run --rm -it  --mount type=bind,source=/home/jsonch/Desktop/langsec23/lucid/langsec/progs/monitor/monitor.dpt,target=/app/inputs/monitor.dpt --mount type=bind,source=/home/jsonch/Desktop/langsec23/lucid/langsec/progs/monitor/monitor.json,target=/app/inputs/monitor.json jsonch/lucid:lucid /bin/sh -c "./dpt /app/inputs/monitor.dpt --spec /app/inputs/monitor.json"
dpt: -------Checking well-formedness---------
dpt: ---------typing1---------
dpt: ---------Concretizing symbolics-------------
dpt: ---------Aliasing Modules-------------
dpt: ---------Making returns explicit-------------
dpt: -----------renaming-----------
dpt: -------Performing parser slot analysis---------
dpt: -------Eliminating modules---------
dpt: -------Inlining size declarations---------
dpt: ---------typing2---------
dpt: -------Eliminating type aliases 2---------
dpt: -----------inlining functions-----------
dpt: -----------inlining tables-----------
dpt: ---------Eliminating events with global arguments----------
dpt: ---------------typing3-------------
dpt: -------Eliminating vectors-------
dpt: -----------re-renaming-----------
dpt: -------Eliminating EStmts-------
dpt: -------Eliminating records-------
dpt: -------Eliminating tuples-------
dpt: -------Inlining Constants-------
dpt: -----------re-re-renaming-----------
dpt: ---------------typing again-------------
dpt: -------Translating to core syntax---------
dpt: -------Partial interpreting---------
dpt: Simulating...
dpt: Using random seed: 1685563174

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

**reading the intepreter output**
The first few lines from `make interp` are output from the lucid.sh script, where it is generating the docker command to run. e.g., up to: 
``
running command: docker run --rm -it  --mount type=bind,source=/home/jsonch/Desktop/langsec23/lucid/langsec/progs/monitor/monitor.dpt,target=/app/inputs/monitor.dpt --mount type=bind,source=/home/jsonch/Desktop/langsec23/lucid/langsec/progs/monitor/monitor.json,target=/app/inputs/monitor.json jsonch/lucid:lucid /bin/sh -c "./dpt /app/inputs/monitor.dpt --spec /app/inputs/monitor.json"
``

Next, the lines prefixed with `dpt:` are messages from the lucid compiler frontend as it type checks and transforms the program into a simpler form that the interpreter can run. If your program has an error, typically the interpreter will print an error message and halt at some point in this process. 

Finally, after the `dpt: Simulating...` line, we see the output of the lucid simulator. The simulator's output has two components: 
1. a trace summarizing the events that were handled at each switch in the simulation, interleaved with any "printf" statements that executed in the program. In this example, the trace is: 
```
t=0: Handling packet event eth_ip(11,22,2048,1,2,128) at switch 0, port 0
t=600: Handling event prepare_report(11,22,2048,1,2,128) at switch 0, port 196
sending report about packet {src=1; dst=2; len=128} to monitor on port 2
```
the lines with `t=...` are reports about events arriving at switches in the simulation, printed by the Lucid interpreter. The line `sending report...` is printed from the `printf` statement in the `prepare_report` handler. 

2. a summary of the state of each simulated switch at the end of the simulation. The fields of the summary are: 
	- `Pipeline`: the contents of any global variables in the switch at the end of simulation. A later tutorial will go into more details about global variables. 
	- `Events`: a list of events that were queued at the switch, but not yet handled by the time the simulation ended.
	- `Exits`: event packets generated by the switch that were sent to external components.
	- `packet events handled` and `total events handled` the number of packet events and events handled. 

# Tutorial 2: the Lucid interpreter

Lucid's interpreter type checks a program and then executes it on a pre-defined trace of events specified in a json file. The Lucid interpreter is a quick way to prototype and test the correctness of a program before compiling it to P4 for the Tofino. There are a few reasons to use the interpreter instead of always compiling programs to P4 and testing them on the Tofino. 

1. **Agile development.** Compiling to P4, then to Tofino, then running the Tofino ASIC model takes a long time. The interpreter, on the other hand, runs almost instantly, which makes iterating and prototyping *much* faster.

2. **Semantics and compiler bugs.** The interpreter defines the semantics of the Lucid language in a relatively straightforward way. It also has far fewer bugs than the Lucid --> Tofino toolchain, because its so much simpler. So, you can use the interpreter to quickly find and eliminate logical bugs in your program.

3. **Language features.** The interpreter supports the full Lucid language. In contrast, there are some features that the compiler doesn't support yet, like delaying events and scheduling events to execute at different nodes in the network. 

4. **Type-and-effects checking.** The final reason to use the interpreter is because Lucid's type checking is done before interpretation. So, if your Lucid program  interprets, you know that the program uses persistent state in a way that can be mapped to legal instructions and laid out in a valid way for the underlying architecture. That being said, it is still possible to write programs that interpret, but require more resources than the underlying hardware can provide. 


## Using the interpreter
Let's run the interpreter on the histogram example program. Assuming you compiled Lucid successfully, cd to the ``<git root>/examples/tutorial`` directory and run ``../../dpt histogram.dpt --spec histogram.json``. You should see output like this: 

```
vagrant@lucidvm:/lucid/examples/tutorial$ ../../dpt histogram.dpt --spec histogram.json
dpt: Parsing ...
Processing histogram.dpt
#
# many elided lines of output here
#
dpt: Done
```

There are two things to understand about the interpreter: its input and output. 

## Interpreter input specification
For the histogram example, the interpreter specification file is ``examples/tutorial/histogram.json``: 

```
{
  "switches": 1,
  "max time": 9999999,
  "default input gap": 10000,
  "random seed": 0,
  "events": [
    {"name":"ip_in", "args": [128, 1, 2, 256, 0]},
    {"name":"ip_in", "args": [128, 2, 2, 256, 0]},
    {"name":"ip_in", "args": [128, 3, 2, 256, 0]},
    {"name":"ip_in", "args": [128, 4, 2, 768, 0]},
    {"name":"ip_in", "args": [128, 5, 2, 768, 0]}
  ]
}
```

In this specification file: 
- ``switches`` is the number of nodes in the model network. All nodes are directly connected. 
- ``max time`` is the number of time units to run the simulation for. Simulation time is in nanoseconds.
- ``default input gap`` is the number of nanoseconds between events provided to the simulator.
- ``random seed`` is used internally by the interpreter for hash functions and anything else that requires randomness. 
- ``events`` are the events that the interpreter executes the program on. Each event object has a ``name``, literally the name of the event, and ``args``, which are an order list of arguments to the event. So, for example, the line ``{"name":"ip_in", "args": [128, 1, 2, 256, 0]}`` will generate the same event as the Lucid statement ``generate ip_in(128, 1, 2, 256, 0);``. 
- ``timestamp and locations`` (not shown in the example) each event can also have a timestamp (in nanoseconds from simulation start time) and a location, which defines exactly when and where the event executes. So for example, if we had a network with two switches, we could schedule two concurrent ``ip_in`` events at them with the lines: 
```
{"name":"ip_in", "args": [128, 1, 2, 256, 0], "timestamp" : 10000, "locations" : [0]},
{"name":"ip_in", "args": [128, 2, 2, 256, 0], "timestamp" : 10000, "locations" : [1]}
```

## Interpreter output

When you run the interpreter, you get messages from Lucid's type checker, then a trace of the execution of the program on the input trace. Here's the simulation output that we get from the histogram example: 

```
vagrant@lucidvm:/lucid/examples/tutorial$ ../../dpt histogram.dpt --spec histogram.json
dpt: Parsing ...
Processing histogram.dpt
#
# type checking messages elided
# 
dpt: Simulating...
dpt: Using random seed: 0

t=0: Handling entry event ip_in(128,1,2,256,0) at switch 0
t=10000: Handling entry event ip_in(128,2,2,256,0) at switch 0
t=20000: Handling entry event ip_in(128,3,2,256,0) at switch 0
t=30000: Handling entry event ip_in(128,4,2,768,0) at switch 0
t=30600: Handling event report(0) at switch 0
t=31200: Handling event report(1) at switch 0
t=31800: Handling event report(2) at switch 0
t=32400: Handling event report(3) at switch 0
t=40000: Handling entry event ip_in(128,5,2,768,0) at switch 0
dpt: Final State:

Switch 0 : {

 Pipeline : [
    0 : [0u32; 0u32; 1u32; 0u32]
    1 : [1u32]
  ]

 Events :   [ ]

 Exits :    [
    ip_out(128,1,2) at t=0
    ip_out(128,2,2) at t=10000
    ip_out(128,3,2) at t=20000
    ip_out(128,4,2) at t=30000
    ip_out(132,0,0) at t=30600
    ip_out(132,1,3) at t=31200
    ip_out(132,2,1) at t=31800
    ip_out(132,3,0) at t=32400
    ip_out(128,5,2) at t=40000
  ]

 entry events handled: 5
 total events handled: 9

}
dpt: Done
```

The first part of the simulator output is a time-ordered trace of the events that the Lucid program handled, e.g., ``t=0: Handling entry event ip_in(128,1,2,256,0) at switch 0``. The interpreter also supports printf functions that can print messages in this trace -- useful for debugging. 

The second part of the simulator output is the final state of each simulated switch. The final state of a switch has three parts: 
1. ``Pipeline`` -- this is a list of the state of all persistent arrays in the program. For now, the simulator tells you the ID of the array, rather than its name. The first array declared in the program gets ID 0, the second array declared gets ID 1, and so on. So, in the above trace, the line ``0 : [0u32; 0u32; 1u32; 0u32]`` tells us that the array declared first in the program, i.e., ``global Array.t<<32>> hist_arr = Array.create(4);``, ended execution with zeros in all cells except at index 2, which had value 1.

2. ``Events`` -- this is a list of events that were still pending at the switch when the simulation ended. This will often be empty. 

3. ``Exits`` -- this is a list of exit events that the switch generated throughout its execution. Note that generation of non-exit events are not shown in this list. For example, the ``report`` events that the ``histogram.dpt`` program generates are not shown -- only the ``ip_out`` events. 

Finally, at the end of the switch state block is a summary of the number of events and entry events handled by the switch. 

## Next steps

The next tutorial walks through the process of compiling a Lucid program to P4 and executing it on the Tofino model. [tutorial_compiler](tutorial_compiler.md).

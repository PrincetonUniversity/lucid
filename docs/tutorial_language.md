# Tutorial 1: Introduction to Lucid
There are three core abstractions in Lucid: events, handlers, and arrays. To see how we use these primitives, lets look at ``examples/histogram.dpt`` -- a simple measurement application that reports a histogram of recent packet sizes to a collection server at regular intervals. 

Here's a visual representation of ``histogram.dpt``:

![histogram_monitor](images/histogram_flowchart.jpg)

### Events and Handlers

**Events.** Events are the communication primitive in Lucid. They are an abstraction of both packets that your system is processing, and also control messages between components in your system. Each event has a name and carries user-specified data as parameters. For example, in ``histogram.dpt``, one event is ``ip_in``. This event abstracts a packet. It carries an input port, source and destination addresses, packet length, and tos byte. It is declared in ``histogram.dpt`` as: 
```
event ip_in (int<<9>> igr_port, int src, int dst, int<<16>> len, int<<8>> tos);
```

This event is generated outside of Lucid, by the underlying switch, when an IP packet arrives.

**Handlers.** Handlers are the main computation primitive in Lucid. A handler is an imperative function that executes when a certain event occurs. Handlers can operate on local variables that persist for only the duration of the event or global variables that persist across packets. Handlers can also generate additional events, to be processed later. 

Here is the ``ip_in`` handler from ``histogram.dpt``: 

```
handle ip_in (int<<9>> igr_port, int src, int dst, int<<16>> len, int<<8>> tos) {
  int idx = 0;
  if (len <= 128) { 
    idx = 0;
  } else {
    if (len <= 512) {
      idx = 1;
    } else {
      if (len <= 1024){ 
        idx = 2;
      } else {
        idx = 3;
      }
    }
  }
  Array.setm(hist_arr, idx, incr, 1);
  int total_ct = Array.update(total_arr, 0, incr, 1, incr, 1);
  if (total_ct == pktct_interval) {
    generate report(0);
  }
  generate ip_out(igr_port, src, dst);
}
```

As you can see, this handler updates the persistent histogram state (using the ``Array.*`` operations), generates an ``ip_out`` event that sends the packet back down to the switch with information about which port the packet should be sent out of, and periodically generates a ``report`` event that will export the histogram state to the collection server. 

**Continuation and recursion.** When a handler generates an event, the event is encoded into a packet, recirculated, and processed in a subsequent pass through the switch's pipeline. You can think of a handler ``foo`` that generates an event ``bar`` as a function ``foo`` that calls a continuation function ``bar`` *to run at some future time*. Handlers that generate events let us express computation that is too complicated to perform in single pass through the switch's pipeline. 

Handlers can also be recursive, they can generate their own event type. For example, in the histogram example, the ``report`` event is recursive: 
```
handle report(int idx){
    int cur_idx = idx; 
    int column_ct = Array.update(hist_arr, cur_idx, getf, 0, setf, 0);
    generate ip_out(collector_port, cur_idx, column_ct);                
    if (cur_idx == 0) {
      Array.set(total_arr, 0, 0);      
    }
    if (cur_idx < 3){       
      generate report(cur_idx + 1);
    }
}
```
The ``report`` handler processes ``hist_arr[idx]``, then generates another ``report`` event to process ``hist_arr[idx+1]``. Each event gets processed in a separate pass through the switch's pipeline and the recursion continues until all 4 elements of ``hist_arr`` are processed. Each recursive call has a cost -- a packet is recirculated. But there is no way around that, since the underlying hardware can only access one element of each persistent memory array per packet. 

**Entry and exit events.**

Entry and exits events are how a switch transfers control flow to / from a Lucid program. For example, in ``histogram.dpt``, we have the following events: 
```
entry event ip_in (int<<9>> igr_port, int src, int dst, int<<16>> len, int<<8>> tos);
event report(int idx);
exit event ip_out (int<<9>> egr_port, int src, int dst);
```

A regular event, without either keyword, is an event that is generated by a Lucid handler and processed by a Lucid handler. An *entry* event is an event that is generated by *the switch's underlying P4 program* and processed by a Lucid handler. Finally, an *exit* event is the opposite of an entry event -- it is generated by a Lucid handler and *processed by the underlying P4 program*.


### Arrays 

Lucid programs interact with persistent state through the *Array* module. In ``histogram.dpt``, we see some calls to Array methods, such as in ``ip_in``:  
```
Array.setm(hist_arr, idx, incr, 1);
int total_ct = Array.update(total_arr, 0, incr, 1, incr, 1);
```

In general, Array methods read a value from a specific cell in a persistent array, perform a small amount of computation, and then return the result to the memory cell and / or a local variable. 

**Array ordering.** Lucid's type checker enforces two rules related to operations on arrays that are designed to catch programs with memory access patterns that cannot be supported by the underlying hardware. Specifically, Lucid's type system demands that you obey two program-wide rules on stateful operations: 

1. always access arrays in the order that they were declared

2. only access each array once per control flow of the program

If you break either of these rules, there's a very good chance that your program is inherently incompatible with the underlying hardware, so Lucid's type system will complain and tell you what you did wrong.

**Memops.** When you call a method of the Array module, you typically pass it a memop function -- a simple function that describes the computation to perform on the state read from persistent memory before returning the result to either the memory cell it was read from, or a local variable in the handler. For example, we would write this line ``Array.setm(hist_arr, idx, incr, 1);`` in C as: ``hist_arr[idx] = incr(hist_arr[idx], 1);`` 

And here is the memop ``incr``: 
```
memop incr(int memval, int incrval) {
    return memval + incrval;
}
```

As we can see, memops look like functions. But, they have a few syntactic restrictions that let Lucid guarantee that every array operation which uses memops is simple enough to compile to legal instructions for the underlying hardware. Basically, when you write a memop, you must obey three simple rules: 

1. a memop can only have two arguments.
2. a memop can only consist of a return statement, or a single if / else statement with a return statement in each branch.
3. a memop can only use each of its arguments once in its body. 

If a memop does not adhere to these rules, the Lucid type checker will complain. 

*A note on memop restrictions: If you are familiar with the Tofino, you might observe that these rules are more limiting than the underlying hardware. However, they present a simpler and more regular interface to state that simplifies a developer's mental model. In the future, our goal is for Lucid's memop syntax to be extendible, so that developers can choose their own balance between regularity and completeness.*

**Array.update.** The most powerful Array method is ``Array.update``, which performs a read and write operation in parallel. There is an example in ``histogram.dpt``: 
```
int total_ct = Array.update(total_arr, 0, incr, 1, incr, 1);
```
In C, we would write this as: 
```
int tmp = incr(total_arr[0], 1); 
total_arr[0] = incr(total_arr[0], 1); total_ct = tmp;
``` 
Again, because of the syntactic restrictions on memops, Lucid can guarantee that any ``Array.update`` call can be compile to a legal instruction for the underlying hardware. 



### Next steps

This tutorial introduced the basics of Lucid. There are many more language elements that will be described in other documents. For now, you can continue to the next tutorial, on how to run a Lucid program in the interpreter [tutorial_interpreter](tutorial_interpreter.md).

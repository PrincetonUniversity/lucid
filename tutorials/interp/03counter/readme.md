## counter.dpt

In this example, we extend `monitor.dpt` so that it counts packet by flow. Instead of streaming reports to a monitoring server, `counter.dpt` maintains a pollable array of per-flow counters that a monitoring server can query as needed. 

To implement the counters, we need to store some state that persists across packets. In Lucid, we do this with  the builtin `Array` data structure. 

### Arrays

Arrays in lucid are a global builtin data structure, like Tables. An important difference between arrays and tables is that tables are _read only_ from inside the switch -- a packet (or event) can read from a table, but cannot update its contents. Arrays, on the other hand, can be both read and written while processing a packet / event. 

`counter.dpt` stores the number of packets in each monitored flow in an _array_ named `flow_counts`.
```c
global Array.t<32> flow_counts = Array.create(8);
```
Arrays in Lucid are global (that is, they are shared by handlers and persist across packets). When we declare an array, we specify the size of the integers that it stores (32-bit integers, in `flow_counts`) and its size (8, for `flow_counts`).

Lucid programs read and write to arrays using `Array` methods. 
For example, to get the value at a position in `flow_counts`, `counter.dpt` uses `Array.get`: `int ct = Array.get(flow_counts, idx);`

There is also an `Array.set(<array>, <index>, <value>)` method, however `counter.dpt` uses a different method to increment the value in `flow_counts`: `Array.setm(<array>, <index>, <memop>, <memop arg>)`. The "m" in "setm" stands for "memop". A memop is a special type of function that can be passed to certain array methods to specify how the value stored at an index of an array should be modified. Here's how `counter.dpt` uses a memop: 

```c
memop incr(int stored_val, int incr_by) {
	return stored_val + incr_by;
}
// later, in a function:
{
	Array.setm(flow_counts, idx, incr, 1);
}
```

Array.setm uses the incr function to update the value stored at `flow_counts[idx]`. In psuedocode, here's what `Array.setm` does: 

```python
def Array.setm(array, idx, memop, memop_arg):
	array[idx] = memop(array[idx], memop_arg)
```

There is also an `Array.getm` function that uses a memop, but returns the memops output to the program instead of writing it 
back to the array:

```python
def Array.getm(array, idx, memop, memop_arg):
	return memop(array[idx], memop_arg)
```

Finally, there's an array function called `Array.update`, which is essentially a `setm` and a `getm` in parallel:

```python
def Array.update(array, idx, get_memop, get_memop_arg, set_memop, set_memop_arg):
	return_val = get_memop(array[idx], get_memop_arg)
	array[idx] = set_memop(array[idx], set_memop_arg)
	return return_val
```

The last thing to know about Arrays, and `global` data structures in general: event handlers are only allowed to access each `global` object once per execution, and all the control flows in a program must access the globals in the order that they are declared. 

**Background: Why memops?**

The documentation above should give you all that you need to start playing around with arrays for yourself. However, at this point, you may be wondering why Lucid uses this oddly restrictive memop abstraction. 

Why can't we just write code that reads and writes to arrays like we would in general-purpose languages? For example, instead of writing:
```c
memop incr(int stored_val, int incr_by) {
	return stored_val + incr_by;
}
Array.setm(flow_counts, idx, incr, 1);
```

Couldn't we just write: 
```c
int tmp = flow_cts[idx] + 1;
flow_cts[idx] = tmp;
```

The reason for Lucid's restrictive array interface is to make it impossible to write programs that have array access patterns fundamentally incompatible with the high-performance packet processing engines that Lucid targets. 
*(This kind of design is sometimes called "correct by construction" -- if the language primitives allow you to build something, then it is correct, because it is not possible to express incorrect things given the available primitives.)*

Packet processors are carefully designed to support high throughput (process billions of packets per second) and low latency (process each packet in tens of nanoseconds). To meet these performance demands, a common packet processing architecture is a feed-forward pipeline of stages. Packets move through the pipeline in one direction, spending few cycles in each stage. Each stage has a few compute units that execute mostly in parallel, and around 1MB of memory for arrays. Importantly, the memory is local to the stage -- it can only be accessed by compute units in its own stage. See Section 2.2 of the Lucid paper for a more detailed description: [Lucid paper](https://www.cs.princeton.edu/~jrex/papers/lucid21.pdf)

The important thing to realize about this architecture is that **not every packet processing function can be laid out across a pipeline with this architecture**. For example, there's no way to lay out a program if it reads a value from an array, does a bunch of computation, and then tries to write it back in the same event handler: 

```c
int tmp = flow_cts[idx];
tmp = do_stuff(tmp);
flow_cts[idx] = tmp;
```

The code above can't compile to a pipeline because if `do_stuff` is sufficiently complicated, it will advance the pipeline to the next stage, where it is no longer possible to write back to `flow_cts`.

Requiring programmers to operate on arrays using memops and the `Array.getm/setm/update` functions makes it impossible to write code like this -- because memops themselves are restricted with respect to the amount of computation that they can perform. 

Another class of programs that cannot compile to feed-forward pipelines are programs that use arrays in different orders depending on their control flow. For example: 

```c
if(x == true) {
	a = array1[1];
	array2[1] = a;
} else {
	a = array2[1];
	array1[1] = a;
}
```
This can't be compiled because the `true` branch requires that `array1` comes in a stage before `array2`, but the `false` branch requires the opposite. 

Lucid's restriction that handlers always use global variables in the order that they are declared and at most once per control flow makes it impossible to write these kinds of invalid programs.

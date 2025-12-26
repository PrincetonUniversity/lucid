### Vectors

*Note: this example is under construction.*

A common pattern in high-performance programs is to use multiple instances of the same data structure. For example, instead of implementing a cache as a single hash-indexed array, implement it as multiple hash-indexed arrays that each use a different hash function. This reduces collision rate compared to allocating the memory to one large hash table.

Lucid includes vectors and simple for loops, primarily to support data structures like this. 

This example shows how to use vectors and for loops to write a mult-stage integer set. Type arguments in the set let users configure the number of stages and the size of the integers. For example: 

```
global MultiStageSet.t<3, 32> cache1  = MultiStageSet.create(256); // construct a 3-stage cache for 32-bit ints.
global MultiStageSet.t<10, 16> cache2 = MultiStageSet.create(256); // a 10-stage cache for 16-bit ints.
```


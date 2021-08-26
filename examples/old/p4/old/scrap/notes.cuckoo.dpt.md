notes:

can we pass an object to generate to a handler? So somehow pass a "generate command" to a handler?

- How would we encode that generate command in the packet data?

- How do we implement a hash function in P4? 
- Can we do modulo


Front end todo:

- Transformation passes
	- functions / inlining
	- expression simplification


- Algorithm
	- How do we represent that a key slot is "empty"?


- Optimization
	- could we reduce critical path length by not re-using variables?
	- should support constants as inputs to Array.* (for index parameter)

	- We can get the debugged code working by manually placing the stash into stages 9 and 10!
	- inlined version should work with above fixed and some minor tweaks.
	- Fix in the backend: the cloned header is limited to 32B. Have to bridge the data and remove it for not-DPT packets. 

- Call generate in the cuckoo_inline example

- Optimization is getting more and more important.
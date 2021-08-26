applications from sigcomm 21 submission

In general, there were 4 bug workarounds these programs use:

1. inlined events combinators
2. removed boolean negations i.e., 
	if (!continue) 
--> 
    if (continue==false) {

3. removed Array.update from inside of if condition (not supported by subexpression eliminator yet)
4. added placeholder for location of current switch. 
	- "int self = 1;"
	- did not add as const because that would produce a comparison of const vs const, which is bugged.

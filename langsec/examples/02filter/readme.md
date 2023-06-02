## filter.dpt

`filter.dpt` builds on `monitor.dpt` by filtering report events so that the monitoring server only receives reports about packets belonging to certain _packet flows_. 

### Filtering packet flows
Packet flows, or flows, are a common networking term that means a a sequence of packets that all share the same values for some subset of fields. So, for example, we could define a flow as all the packets with ip source address = 1 and ip destination address = 2. We'll call flows based on IP source and destination address _ip flows_, and talk about them with the syntax `(src=x, dst=y)`, which means the flow of packets from ip address x to ip address y. 

`filter.dpt` only generates reports about packets from flows `(src=1, dst=2)` and `(src=3, dst=4)`.

If you run it, with `make interp`, you should see it generate two report events, even though the input trace has 3 packets:

```
dpt: Final State:

Switch 0 : {

 Pipeline : [
    filter_table(0) : Table [...]
  ]

 Events :   [ ]

 Exits :    [
    eth_ip(11,22,2048,1,2,128) at port 1, t=2
    eth_ip(11,22,2048,3,4,128) at port 1, t=3
    eth_ip(11,22,2048,5,6,128) at port 1, t=4
    report(1,2,128) at port 2, t=602
    report(3,4,128) at port 2, t=603
  ]

 Drops :    [ ]

 packet events handled: 3
 total events handled: 5

}
```


### Implementing a filtering function

`filter.dpt` implements filtering as a function that returns a boolean, `true` if a report should be generated and `false` if it should not: 

```c

handle eth_ip(eth_t eth, ip_t ip) {
	generate_port(OUT_PORT, eth_ip(eth, ip));

	// if (filter_with_if(ip)) {
	// if (filter_with_match(ip)) {
	if (filter_with_table(ip)) {
		generate(prepare_report(eth, ip));
	}
}
```

As you can see, this example has three filtering functions that show the three different ways to implement a filtering function in Lucid. Each approach uses an increasingly powerful (and slightly more complicated) Lucid primitive.

#### If/else statements

The simplest way to implement a filtering function is with nested `if / else` statements:

```c

fun bool filter_with_if(ip_t ip) {
	if (ip#src == 1 && ip#dst == 2) {
		return true;
	} 
	else {
		if (ip#src == 3 && ip#dst == 4) {
			return true;
		}
		else {
			return false;
		}
	}
}
```

#### Match statement

If/else statements are fine for simple programs, like if you only want to filter one or two flows. But they get very gnarly for more complex filtering, for example if we wanted to filter 10 different IP flows. A much cleaner way to implement more complex filtering functions is to use a match statement:

```c

fun bool filter_with_match(ip_t ip) {
	match (ip#src, ip#dst) with 
	| 1, 2 -> {return true;}
	| 3, 4 -> {return true;}
	|	_, _ -> {return false;}
}

```

A match statement is like a more powerful version of a switch statement in C. You specify a _key_, which is a list of expressions (typically a list of variables, for example `(ip#src, ip#dst)`), and then a list of cases.

Each case has a _condition_ (values that the variable must take for the case to match) and an _action_ (a block of code to execute if the case matches). 
The match statement evaluates the key, then checks each case in order and executes the action of the first case whose condition matches the key. 

In the `filter_with_match` function, one case is `| 1, 2 -> {bool rv = true; return rv;}`. The condition of this case is `1, 2`, which means that it will match when `ip#src == 1` and `ip#dst == 2`. The action of this case is `{return true;}`, so the function will exit if this case matches.

The last case in `filter_with_match` uses _wildcard values_ (`_`) in its condition: `| _, _ -> {return false;}`. The wildcard (`_`) just means that the this condition matches for any value of the corresponding key field. Since the last case of the match statement has wildcards for all keys, it effectively a "default" case that is guaranteed to match if none of the previous cases do. 

Match statements are a clean way to implement decision logic with many branches. For something like a flow filter, we could imagine scaling a match statement up to handle dozens, or even hundreds, of flows while keeping the code clean and easy to read. Match statements are also very efficient at runtime, because they compile into "match-action hardware tables" in the data plane switch. A "match-action" table can check _all_ the cases of a match statement simultaneously in a single operation! For more information about "match-action" tables, take a look at this paper: [forwarding metamorphasis](http://people.eecs.berkeley.edu/~sylvia/cs268-2016/papers/rmt.pdf), or google / ask chatGPT about "TCAM tables".

#### Match tables

The main limitation of a match table is that it is static: the cases are hard-coded into the program. For example if you decide that you want to filter some additional flow, you would have to modify the program, recompile it, and restart it on the switch. This isn't practical in a live network. 

The solution for programs with dynamic decision logic is to use a builtin Lucid data structure called a _match table_. A match table is similar to a match statement -- it has a key and a list of cases (or _rules_) that each have a condition and an action. But unlike a match statement, the rules in a match table are _dynamic_: you can change the rules in a match table at runtime. 

Match tables are powerful, but a little more verbose than match statements. Here's how you _declare_, _use_, and _update_ a match table.


**declaring and using tables**

`filter.dpt`, has a table named `filter_match`. First, to create the filter table, we call Lucid's builtin `table_create` function. This function takes three arguments: a list of actions that rules in the table can use; a size, and a default action to execute. It creates the table named "filter_table", which is annotated as a `global`, which just means that this table is accessible from any handler and its contents persist across events.


```c
// create the filter_table
global filter_table_t filter_table = 
	table_create<filter_table_t>(
		(result), // result is an action bound to this table, there may be more than 1.
		1024, // the maximum number of rules in the table.
		result(false) // the default action of this table is to call result(false)
	);
```

The action `result` is defined a bit earlier:
```c
action bool result(bool b)() {
	return b;
}
```

Actions are like functions, except their body can only include a return statement. Also, actions have _two_ sets of parameters. The first set of parameters gets passed to the action when a rule is installed, and the second set of parameters is passed to the action when a rule matches a key.

Then, to use the filter table, we call Lucid's builtin `table_match` function, which takes three arguments: the name of the table to apply, a key to match against the rules of the table, and a list of arguments to pass to whatever action the table ends up calling. `table_match` will find the first rule that matches the key, call its action, and return whatever the action returned. 

```c
// use the filter_table
fun bool filter_with_table(ip_t ip) {
	bool r =              // return value from the matching rule
    table_match(
      filter_table,     // the table, which holds rules and actions
      (ip#src, ip#dst), // the key to match with the table
      ()                // arguments to the table's actions
  );
	return r;
}
```

**installing table rules**

Again, the main point of tables versus match statements is that the rules in a table can be modified at runtime. In the Lucid intepreter, the easiest way to install a rule into a table is with a special "command event" that we put into the interpreter's input trace. `filter.json` has command events to install two rules into `filter_table`. Here is one of them:

```json
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"filter_table", 
        "key":[1, 2], 
        "action":"filter_table.result", 
        "args":["true"]}}

```
this command says, "run Table.install to add a rule to `filter_table`. The rule matches on the key `[1, 2]` and when it matches it calls `result(true)`". 
It is roughly equivalent to having a case in a match statement 
`1, 2 -> {return result(true);}`

**table types**

A final detail about tables. Looking at the declaration of `filter_table` closely, you will notice that the filter table has its own type, `filter_table_t`: 

```c
global filter_table_t filter_table = ...;
```
`filter_table_t` is defined as: 

```c
table_type filter_table_t = {
	key_size: (32, 32)
	arg_types: ()
	ret_type: bool
}
```

You can think of a "table type" like `filter_table_t` as a schema that defines the columns of a table: the number of fields in the table's key and how many bits each field is; the types of the parameters of all of the table's actions, and the type of the return values from all of the table's actions. 

Lucid makes us specify table types for two reasons. First, because we need to know all of these details about the table at compile time so that we can allocate enough memory for it (in data plane hardware, there is no dynamic memory allocation!). Second, table types let lucid ensure that a program always uses tables in safe and well-defined ways. 

Specifically, when you call `table_match` on something of type `filter_table_t`, lucid will check that: 
1. the key argument of the `table_match` call is a list of two expressions that each evaluate to 32-bit values, because of `key_size: (32, 32)`; 

2. the "action argument" argument of the `table_match` call is an empty list, because of `arg_types: ()`; 

3. the return value of `table_match` will always be a bool, because of `ret_type : bool`. 





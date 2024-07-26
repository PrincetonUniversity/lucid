## filter.dpt

`filter.dpt` builds on `monitor.dpt` by filtering report events so that the monitoring server only receives reports about packets belonging to certain _packet flows_. 

### Filtering packet flows
Packet flows, or flows, are a common networking term that means a sequence of packets that all share the same values for some subset of fields. So, for example, we could define a flow as all the packets with ip source address = 1 and ip destination address = 2. We'll call flows based on IP source and destination address _ip flows_, and talk about them with the syntax `(src=x, dst=y)`, which means the flow of packets from ip address x to ip address y. 

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

As you can see from the commented out `// if ...` lines, this example has three filtering functions that show different implementation strategies. Each function uses an increasingly powerful (and slightly more complicated) Lucid primitive.

#### If/else statements

The simplest way to implement a filtering function is with nested `if / else` statements, much like you could do in C / python / Java / etc:

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

If/else statements are fine for simple cases, like if you only want to filter one or two flows. But they get gnarly for more complex filtering, for example if we wanted to filter 10 different IP flows. A  cleaner way to implement decision logic with many branches is to use a match statement:

```c

fun bool filter_with_match(ip_t ip) {
	match (ip#src, ip#dst) with 
	| 1, 2 -> {return true;}
	| 3, 4 -> {return true;}
	|	_, _ -> {return false;}
}

```

A match statement is similar to a switch statement in C, but more flexible. You specify a _key_, which is a list of expressions (typically a list of variables, for example `(ip#src, ip#dst)`), and then a list of _cases_.

Each case has a _condition_ (values that the variable must take for the case to match) and an _action_ (a block of code to execute if the case matches). 
The match statement evaluates the key, then scans through the list of cases and executes the action of the first case whose condition matches the key. 

In the `filter_with_match` function, one case is `| 1, 2 -> {bool rv = true; return rv;}`. The condition of this case is `1, 2`, which means that it will match when `ip#src == 1` and `ip#dst == 2`. The action of this case is `{return true;}`, so the function will exit returning true if this case matches.

The last case in `filter_with_match` uses a _wildcard value_ (`_`) in its condition: `| _, _ -> {return false;}`. The wildcard (`_`) just means that the this condition matches for any value of the corresponding key field. Since the last case of the match statement has wildcards for all keys, it effectively a "default" case that is guaranteed to match if none of the previous cases do. 

Match statements are a clean way to implement decision logic with many branches. For something like a flow filter, we could imagine scaling a match statement up to handle dozens, or even hundreds, of flows while keeping the code clean and easy to read. Match statements are also very efficient at runtime, because they compile into "match-action hardware tables" in the switch's packet processing chip. A "match-action" table can check _all_ the cases of a match statement simultaneously in a single operation! For more information about "match-action" tables, take a look at this paper: [forwarding metamorphasis](http://people.eecs.berkeley.edu/~sylvia/cs268-2016/papers/rmt.pdf), or google / ask chatGPT about "TCAM tables in switches".

#### Match tables

The main limitation of a match statement is that it is static: the cases are hard-coded into the program. For example if you decide that you want to filter some additional flow, you would have to modify the program, recompile it, and restart it on the switch. This isn't practical in a live network. 

The solution for programs with dynamic decision logic is to use a builtin Lucid data structure called a _match table_. A match table is similar to a match statement -- it has a key and a list of cases (which we call _rules_ in a match table) that each have a condition and an action. But unlike a match statement, the rules in a match table are _dynamic_: you can change the rules in a match table at runtime. 

Match tables are powerful, but a little more complicated than match statements. Here's how you _declare_, _use_, and _update_ a match table.

**declaring and using tables**

`filter.dpt`, has a table named `filter_match`. To create the filter table, we call the `Table.create` constructor, which produces a value of type `Table.t`. 


```c
global Table.t<<key_t, bool, (), bool>> filter_table = Table.create(1024, [mk_result], mk_result, false);
```
The arguments to `Table.create` are: 

1. number of entries (`1024`)
2. the actions that the table may use (`[mk_result]`)
3. the default action (`mk_result`)
4. an install-time argument for the default action (`false`)

A table's type is parametric. The parameters define the shape of the key and the action functions that the table may use. There are 4 parameter to `Table.t`: 

1. the type of the key
2. the type of the table's actions install-time argument
3. the type of the table's actions run-time argument
4. the return type of the table's actions


Next, take a look at the action, `mk_result`, that `filter_table` uses: 

```c
action bool mk_result(bool b)() {
	return b;
}
```
Actions take two arguments. The first set of arguments (`(bool b)` in `mk_result`) are arguments bound when a rule with that action is _installed_ into a table. 

The second set of arguments (`()` for `mk_result`, which just means "no argument") are arguments bound when the action is _called_ at runtime. 

So an action has both _install time_ and _run time_ arguments.

Use the filter table with `Table.match`: 

```
	bool r = Table.lookup(
		filter_table, 
		{s=ip#src; d=ip#dst},
		()
		);
```
The first argument is the name of the table, the second argument is the key, and the third argument is the runtime argument to pass to the action. 

Table.lookup's semantics are basically:
```
def table_lookup(tbl, key, arg):
	# invoke first matching rule
	for entry in tbl.entries: 
		if(equal_keys(key, entry.key)):
			return entry.action(entry.install_arg, arg)
	# if nothing matched, call default action
	return tbl.default(tbl.default_install_arg, arg)
```

**installing table rules**

Again, the main point of tables versus match statements is that the rules in a table can be modified at runtime. In the Lucid intepreter, the easiest way to install a rule into a table is with a special "command event" that we put into the interpreter's input trace. `filter.json` has command events to install two rules into `filter_table`. Think of a command event as a special event that is handled by the lucid interpreter itself, rather than by your program. Here's a command event to install a rule into `filter_table`:

```json
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"filter_table", 
        "key":[1, 2], 
        "action":"filter_table.mk_result", 
        "args":["true"]}}

```

This command says, "Run Table.install to add a rule to `filter_table`. The rule matches on the key `[1, 2]` and when it matches it calls the action generated by `result(true)`". 
It is roughly equivalent to having a case in a match statement 
`1, 2 -> {return (mk_result(true))();}`

## modules.dpt

Modules in Lucid let you build re-usable libraries. A module is a a collection of functions, types, events, handlers, and constructors. Modules support abstraction: a module's components can be visible to users of the module, or they can be private.

In this example, there are three modules: 

`ArrayUtils` is a simple module that contains convenience functions for working with arrays. 

`Fwd` is a module that implements a forwarding table data structure. It essentially just wraps the somewhat complicated syntax of a `Table` exposing only a simple type, constructor, and getPort function.

`FlowCounter` is a more complicated data structure. It counts packets by (src, dst) flow and, to handle collisions, generates flow record events to a collection server.

The toplevel of the program uses `Fwd` to select an output port, and `FlowCounter` to count packets from all flows, and flows from unrouted packets.

A module has a *signature* (also called a specification) and an *implementation*. 

The signature lists all the components visible to external users. The implementation defines the components, and also may declare additional internal components. For example, consider the type t in `Fwd`: 

```
module Fwd : {  // Signature
    global type t;    
    // ...
} {             // Implementation
    type t = {
        Table.t<<int, int, (), int>> portTable;
    }    
    // ...
}
```

`t` is declared in the signature, so external users can create variables of type `t`. The "global" keyword means external users can only declare `t` variables globally, i.e., not inside of handlers or functions.

Since the definition of `t` is not defined in the signature, external users cannot see its contents. In the implementation of `Fwd`, we have the full definition of `t`, and so other functions and handlers inside of `Fwd` can "look inside" of it.

### Modules as classes

Modules in Lucid are inspired by Ocaml. If you're not familiar with Ocaml, one way to think about modules are as a restricted form of classes, from object-oriented languages. Here is how the anology works, with `Fwd` as an example. 

- Think of `Fwd` itself is a class. 

- Think of type `t` as a declaration of all the *instance fields* of a `Fwd` object. A single table, in this case. 

- Think of the constructor `create` as creating an instance or object of `Fwd`. So when we call `global Fwd.t fwd = Fwd.create(8);`, we are creating an instance of `Fwd`.

- Think of functions in `Fwd` that take a `t self` as their first argument as instance function. So in an object-oriented language, we'd write `fwd.getPort(x)`, but with modules we write `Fwd.getPort(fwd, x)`. 

So, if we wrote and used Fwd in an object-oriented syntax, it might look like this: 

```
class Fwd : {
    constr create(int len);
    fun int getPort(int addr);
} {
    Table.t<<int, int, (), int>> portTable;
    constr create(int len) {
        portTable = Table.create(len, [out_port], out_port, 0); 
    }
    fun int getPort(int addr) {
        return Table.lookup(portTable, addr, ());
    }    
}
```
And then to use it: 
```
global Fwd fwd = Fwd.create(8);
// ...
int p = fwd.getPort(dst);
```

Classes and objects like this, where there is no inheretence and all allocation can be done at compile time, are equivalent to a subset of Lucid's module system. It is a subset because, for example, a module could define multiple global types. Modules are a little more flexible, and were easier (for the Lucid developers) to work with inside the compiler. However, in the future, we can add convenience syntax for classes, as shown above, that work by simply translating into modules. 

### Polymorphism

*Note: this example is a work in progress. Polymorphism in Lucid can do more than what is describe here. See the wiki for more details.*

Polymorphism allows us to write "generic" functions that handle variables of different types in the same way.

Consider this replace function: 
```
    fun int replace('arr_ty arr, int idx, int newval) {
        int zero = 0;
        int rv = Array.update(arr, idx, curval, zero, swap, newval);
        return rv;
    }
```
All the variables and parameters have concrete types: `arr` is an array of 32-bit ints; all the other arguments and variables are `int`s, which is just shorthand for a 32-bit int.

With polymorphism, we can write a polymorphic `replace` that handles _any_ type (that `Array` supports). For example: 
```
fun 'int_ty replace('arr_ty arr, int<8> idx, 'int_ty newval) {
    auto zero = 0;
    return Array.update(arr, idx, curval, zero, swap, newval);
}

global Array.t<8>  arr8     = Array.create(256);
global Array.t<16> arr16    = Array.create(256);
global Array.t<32> arr32    = Array.create(256);

event pktin(int<8> x, int y) {
    auto rv8   = replace(arr8, x, (int<8>)y);
    auto rv16 = replace(arr16, x, (int<16>)y);
    auto rv32 = replace(arr32, x, y);
}
```
In the code above: 
- `'int_ty` and `'arr_ty` are type variable---symbols that represent a specific type. 
- `auto` is an unnamed type variable---think of it as a "hole" in the program. 

Each call to replace can use different types. Lucid's type checker will solve for the types and fill them automatically.

*Note: besides `auto`, polymorphic variables currently must start with a tick `'`.

**Size polymorphism.** Lucid also supports polymorphic sizes, like `int<'c>`. This is for when you want to restrict something to an integer without restricting it to a particular size. Polymorphic sizes are often used with atomic memory operations, which are only for ints. For example: 
```
memop incr(int<'c> memval, int<'c> incrby) {
    return memval + incrby;
}
```

**Polymorphic standard libraries.** In general, all of Lucid's standard library / builtins (e.g., Array) are polymorphic with respect to either size or type.

**Polymorphic size arguments to user-defined types.** Its also possible to give user-defined types polymorphic size arguments. For example, a type can represent any pair of integers that have the same size: 

```
type pair<'a> = {
    int<'a> fst;
    int<'a> snd;
}
fun int<'a> pair_sum(pair<'a> p) {
    return p#fst + p#snd;
}
```
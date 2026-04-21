## Tuples and events with polymorphic parameters


This branch (26.4.tuples) adds tuples and events with polymorphic parameters to Lucid. 

### Motivation

Lucid programs typically operate at specific protocol layers, meaning they are generic to the packet headers of lower layers and the headers + payload of higher layers. Previously, it was up to the programmer to define every combination of possible underlay headers as separate record types. 

For instance if the programmer wants to handle IP packets that may arrive in either ethernet or vlan packets, they must write two separate handlers for each underlay protocol stack: 
```
packet event eth_ip_packet(eth_t eth_hdr, ip_hdr_t ip_hdr, bytes.t unparsed_payload) {
    ip_hdr_t new_ip_hdr = {ip_hdr with src_addr = ip_hdr#dst_addr; dst_addr = ip_hdr#src_addr;};
    event pkt_out = eth_ip_packet(eth_hdr, new_ip_hdr, unparsed_payload);
    generate_port(ingress_port, pkt_out);
}
packet event eth_vlan_ip_packet(eth_t eth_hdr, vlan_t vlan_hdr, ip_hdr_t ip_hdr, bytes.t unparsed_payload) {
    ip_hdr_t new_ip_hdr = {ip_hdr with src_addr = ip_hdr#dst_addr; dst_addr = ip_hdr#src_addr;};
    event pkt_out = eth_vlan_ip_packet(eth_hdr, vlan_hdr, new_ip_hdr, unparsed_payload);
    generate(ingress_port, pkt_out);
}
```
In the above example, the handler bodies are generic with respect to the underlay headers, yet there still must be separate events and handlers. This gets unweildy very quickly in programs for more realistic networks.

The solution introduced here is to support: 1. tuples as event parameters; 2. polymorphic event parameters. Then we can rewrite the above example like this: 
```
packet event ip_packet(auto underlay_headers, ip_hdr_t ip_hdr, bytes.t unparsed_payload) {
    ip_hdr_t new_ip_hdr = {ip_hdr with src_addr = ip_hdr#dst_addr; dst_addr = ip_hdr#src_addr;};
    event pkt_out = ip_packet(underlay_headers, new_ip_hdr, unparsed_payload);
    generate(pkt_out);
}
```


### New language features

#### Tuples
Tuples are basically records with anonymous fields and whose type is defined dynamically when a variable is declared. Tuples are immutable, but can be constructed, used as arguments, and projected similarly to records.

**Tuple type declarations and expressions**

`tuple<<int, int>> my_pair = (1, 2);`

**Tuple projection**

`int a = my_pair.0; int b = my_pair.1;`

#### Polymorphic event parameters
Parameters in events and handlers may now be polymorphic, using the `auto` type keyword: 

```
packet event ip_packet(auto underlay_headers, ...);
handler ip_packet(auto underlay_headers, ...); 
```

Polymorphism works almost the same as for functions, with one exception. In a function, the type system allows the body of a function to restrict a polymorphic parameter to a specific type by operating on it. For example: 
```
fun foo(auto x) { int y = x + 1;} // the type checker infers type int for x
```

This is not (currently) allowed for events. Any operation on a polymorphic parameter in a handler body that requires the parameter to be a specific type will cause a typing error. 

Events with polymorphic parameters may not be given user-defined tag numbers (because they are eliminated by monomorphization, which duplicates the declarations).

### Implementation

Tuples have dedicated AST nodes in the frontend syntax, and are eliminated before the midend. 

Polymorphic event parameters are unified with the respective handler parameters by the type checker, and handlers are checked to not restrict the type of their polymorphic parameters. 

Polymorphic events are eliminated by creating monomorphic duplicates based on event-typed expressions in the program (i.e., event constructors).

It is currently a runtime error to send a program an event value with an argument that uses a parameter with a type not used elsewhere in the program. 

For example, if the program defines: `packet event foo(auto x, ...);`
and only ever uses `foo(int x)` events, it is a runtime error to pass in an event `foo(bool x)`.


### Test cases

`tuple_event.dpt` -- minimal example of tuples
`tuple_event_wrong.dpt` -- a handler using a polymorphic tuple parameter incorrectly
`tuple_event2.dpt` -- a handler using a polymorphic tuple parameter correctly
`tuple_event3.dpt` -- event using a polymorphic parameter with different tuple types depending on parsing (this is the ip_packet example from the motivation).


### Future considerations

- The interpreter should be updated to support input of non-packet events with tuple types.

- It may be useful for users to define polymorphic events with monomorphic handlers, for specific instances that they want to handle, but are not generated in the program. For example:
```
event foo(auto x, auto y);
handle foo(int x, int y) { ... }
handle foo(bool x, bool y){ ... };
```

- The polymorphic event elimination pass may fail for programs that place transitive restrictions on the types of polymorphic event parameters. See the comment in MonomorphicEventArgs.ml for more information.
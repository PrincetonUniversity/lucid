This document describes our strategy for augmenting Lucid with headers that events can read and manipulate.

0. UPDATE: Although the frontend design below is elegant, we decided not to implement it because it's just suuuch a pain.
  1. Allowing records in the midend both bloats the syntax and makes partial interpretation much less effective, because updating one field of a record generates a bunch of identity assignments for the other fields that can't be eliminated without unrolling the record. We could solve this by adding mutable records, but, well, that's even more mutability.
  2. Attempting to implement this in the midend without records is doable, but is (a) a huge pain to implement (as well as being fragile) since we have to manually match fields of the original record type with unrolled variables, and (b) likely to severely slow down interpretation, since we have to parse and keep track of every header field instead of just the relevant ones (that users passed to the events as arguments).

1. We add two new concepts to Lucid: "headers" and "header stacks".
    1. A _header_ is a non-polymorphic, non-global ordered record.
    2. A _payload_ is a built-in abstract type. It can only be constructed by the P4 parser, or by the builtin constructor Payload.empty.
    3. A _packet_ is an ordered list of headers. It is represented as an ordered record, each of whose fields is a header. The final field must a payload instead.
    4. Headers and packet types are declared with their own declarations, e.g.
    ```
    header eth = {int<48> src; int<48> dst; int<16> etype};
    header ip = {int<32> src; int<32> dst; ...};
    packet_type eth_ip = {eth eth; ip ip; Payload.t payload};
    ```
1. There are now two kinds of event:
    1. _Packet events_ correspond to raw (non-lucid packets). For each declared packet type, an event of the same name is automatically declared. This event takes a single argument, which is a packet of that type. When serialized, a packet event corresponds to a packet with each of its headers in order.
    2. _Background events_ are declared by the user. Unlike packet events, background events any number of arguments. At most one argument may have a packet type, and if one exists it must be the first argument. When serialized, a background event corresponds to a packet with a lucid header containing the event id and arguments. If the first argument was a packet, then the lucid header is followed each of the headers in the packet, in order.
3. The user is responsible for defining packet types and handlers for each different packet type they might encounter. Parsing incoming packets and selecting which event to generate is done manually, in the P4 harness.
4. We will include a built-in event called `raw` which takes a single packet of any type and serializes it.
5. In each control flow of each handler, only one packet event may be generated, and all generated events which take a packet as an argument must take the _same_ packet value.
6. Since this new form is unwieldy, we should add various forms of syntactic sugar at some point:
    1. We can add _views_ of packet events, which automatically extract certain header arguments for the user. Views may also include conditions under which they should be selected, which might help automate parser generation.
    2. We can add _row polymorphism_ to allow events to take in header stacks that have extraneous fields, for use with e.g. ip tunneling.
7. Already implemented: ~~Event values will no longer contain a location at which the event is generated; instead, there will be one generate statement for each location type (switch id, port id, or multicast group), with the appropriate location passed in when the event is generated.~~

...Huh, that was shorter than I expected. Here's a simple stateful firewall (for ethernet packets, to keep it simple):
```
global Array.t<<1>> fw = Array.create(16);

header eth = {int<48> src; int<48> dst; int<16> etype};
packet_type eth_packet {eth eth};

// Only two ports for simplicity
const int<<1>> INSIDE_PORT = 0;
const int<<1>> OUTSIDE_PORT = 1;

event eth_packet(eth_packet pkt) {
  int<<1>> port = ingress_port; // Builtin variable
  if (port == INSIDE_PORT) {
    int<<4>> idx = hash<<4>>(pkt.eth.dst);
    Array.set(fw, idx, 1);
    generate_port(OUSIDE_PORT, raw(pkt));
  } else {
    int<<4>> idx = hash<<4>>(pkt.eth.src);
    int allowed = Array.get(fw, idx, 1);
    if (allowed == 1) { generate_port(INSIDE_PORT, raw(pkt)); }
  }
}

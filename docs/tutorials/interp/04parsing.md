## parsing.dpt

So far we have used Lucid for writing event-based programs. Under the hood, Lucid's compiler translates events into packets, by choosing internal header formats for serialization. 

But what if we want to process packets with an externally-defined wire format (for example, Eth/IP packets)?

Lucid has two features to control packet serialization, which this example shows: **packet events** and **parsers**. 

In general, you should only need to worry about packet formats once you are ready to run or test on real traffic---while figuring out application logic, its often easier to just model packets as events. For example, an `ip(int src, int dst)` event to model an IP packet.

### Packet events

Lucid's compiler serializes regular events (i.e., non "packet" events) as a type-length-value message in an ethernet packet. The format is:

```
eth header with ety = LUCID_ETY | event type and length | event parameters
```

A packet event, on the other hand, serializes as the event parameters packed directly (i.e., without any padding or gaps added).
```
event parameters
```

So, when we need to control the exact format of a message to a non-lucid node (for example, an IP network or an endhost), we can use packet events and user types. 

For example, we can define an ethernet packet as:
```
type eth_t = { int<48> dstMac; int<48> srcMac; int<16> etherTy; }
packet event eth_pkt(eth_t eth, Payload.t pl);
```
A `Payload.t` variable simply represents "the rest of the packet". It can only appear as the last parameter of a packet event.

### Parsers

A parser deserializes a packet into an event. Lucid's compiler generates parsers for programs where there is at most 1 packet event. For programs with more than 1, you need to write your own parser. The commented out parser on lines 25-37 of `parsing.dpt` shows the parser that Lucid's compiler generates for this program with 1 packet event.

A parser looks like a function, but has different restrictions. A parse function can: 
    - extract variables from the packet bitstring using `read`
    - skip bits in the packet bitstring using `skip`
    - branch on extracted values with a `match` statement
Every branch of a parse function ends by either calling another parser, dropping the packet, or generating an event. Parsers cannot be recursive -- *there are no back edges in control flow and a parser never returns.*

The packet entry point to a program is always the `main(bitstring pkt)` parser. `main` must start by extracting an ethernet header and branching to lucid's built in parser when the ethertype is equal to the builtin value `LUCID_ETHERTY`. For example: 

```
type eth_t = { int<48> dstMac; int<48> srcMac; int<16> etherTy; }
parser main(bitstring pkt) {
   eth_t e = read(pkt);
   match e#etherTy with
   | LUCID_ETHERTY -> { do_lucid_parsing(pkt); }
   // ...
}
```
Note that the read does not have to use a structure -- any sequence of reads that totals 14 bytes, followed by a match on the last 2 bytes when the first branch is `| LUCID_ETHERTY -> { do_lucid_parsing(pkt); }` will work. 

### Packets in the interpreter

The interpreter supports packet events as input with hex bitstrings. `parsing.json` shows an example of the input format: 
`{"type":"packet", "bytes":"00000000000300000000000412340000006f000000de"}`
When the interpreter gets a packet event, it will run it through the parser, starting at main, to figure out the event. 

Running the example (`make interp`) shows how the output looks for a packet event, with an exit event of: `bits(00000000000300000000000412340000006f000000de)`

### Summary

- If you need to interface with the outside world using packets that have specific wire formats, tag an event in your program as a `packet` event, which gives you direct control of serialization.
- If your program has only one `packet` event, which is often sufficient, this is all you need to do -- the compiler will generate appropriate parsers. 
- For programs with multiple packet events, you'll also need to write a parser. The general template is: 
```
parser start(eth_t eth, bitstring pkt) {
    // my parsing logic.
}

parser main(bitstring pkt) {
  eth_t e = read(pkt);
  match e#etherTy with
  | LUCID_ETHERTY -> { do_lucid_parsing(pkt); }
  | _ -> {start(e, pkt);}
}
```

- When writing parsers, take care that a packet's output and input formats have the expected relationship. Typically, you will want to make sure that events carry all the arguments necessary for serialization. For example, in `parsing.dpt`, we extended `pktin` with an ethernet header and a payload. Even though the handler only looks at the `src` and `dst` fields, the additional headers are important because they let us output the same packet that came in. 

```
packet event pktin(eth_t eth, int src, int dst, Payload.t payload);
``` 

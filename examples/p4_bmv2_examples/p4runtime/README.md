# `p4runtime`

This uses Lucid's **interpreter interactive mode** to support a dynamic controller in Python.

- `dpt --interactive` reads JSON events on stdin and writes exit
  events as JSON on stdout (one record per line).
- `controller.py` launches the interpreter as a subprocess, feeds it
  packets, reads `packet_in` records, and writes back
  `Table.install` commands in response.

The data plane is a flow cache: misses generate `packet_in`; hits
forward. Same shape as [`flowcache`](../flowcache/), but instead of
the controller being a static JSON spec, it's a Python program.

## Files
- [p4runtime.dpt](p4runtime.dpt) — the Lucid program.
- [p4runtime.json](p4runtime.json) — a near-empty spec
  (`"events": []`). Everything happens via stdin.
- [controller.py](controller.py) — the dynamic controller.

## Running
```bash
./controller.py
```

The above command spawns the controller, interpreter, sends a few 
test packets, reacts to the packet_ins, and prints the 
interleaved transcript on stderr.

Sample transcript (abridged):
```
>>> h1->h2 #1 (expect MISS + controller install)
  dpt: { "printf": "sw 0 : MISS dst=167772674 src=167772417 ingress=1 -> PacketIn(controller)", ... }
  dpt: {"name":"packet_in","args":[167772417,167772674,1],"locations":["0:99"],...}
  controller: learned 10.0.2.2 -> port 2 (dmac 08:00:00:00:02:02)
  controller -> dpt: {"type": "command", "name": "Table.install", ...}

>>> h1->h2 #2 (expect HIT)
  dpt: { "printf": "sw 0 : HIT  dst=167772674 src=167772417 -> port 2 ttl=63", ... }
  dpt: {"type":"packet","bytes":"080000000202080000000100...","locations":["0:2"], ...}
```

## How interactive mode works

The `dpt --interactive` flag turns the interpreter into a long-running
server that can be driven from any process with line-delimited JSON.

> - **Input**: every event is a JSON dict on its own line. Reads from
>   stdin until EOF.
> - **Output**: each exit event is a single-line JSON record on
>   stdout. Printf output goes to stdout (as `{"printf": "...", "switch": N}`).
> - **Lifecycle**: starts polling stdin after the spec's `max_time`
>   has elapsed; events arriving on stdin execute at
>   `max(current_ts, event.timestamp)`.


## Notes

- **The "controller" is just a Python program** with JSON in,
  JSON out. The controller's logic 
  (`react_to_packet_in` in `controller.py`) reads packet_ins and 
  decides what rule to install based on the packet_in's fields.
- **Bidirectional channel from one stdin/stdout pair.** Each event /
  command is one line of JSON. The same channel carries packet
  events, `Table.install` commands, and the `packet_in` notifications
  in the other direction. Adding a new control protocol over this
  channel is just adding a new event type to the Lucid program.
- **Shutdown is currently messy.** Closing stdin causes the interpreter to
  exit with a `Fatal error: ... stdin eof`. The controller catches
  the error stream and the run is complete by that point, so it is just 
  annoying.

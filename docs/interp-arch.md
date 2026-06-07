## Interpreter architecture

The Lucid interpreter is a **discrete-event network simulator** built as four layered modules (in `src/lib/midend/interpreter/`)
### The four layers
- **`InterpSwitch`** — _one switch, in isolation._ Defines `state` (a switch's queues, `global_env`, `pipeline`, `sockets`, handlers, `outbox` mailbox, and `global_time`/`counter`/`retval` refs) and all pure single-switch operations: enqueueing, global lookup/add, the mailbox `emit`, queue draining, printing. It has **no knowledge of other switches**.
    
- **`InterpNetwork`** — _the fabric._ Keeps a `network_state = state array` and moves events between switches: `deliver`/`drain`, `calc_arrival_time`, and the external-I/O paths (`emit_or_log_exit` → socket or stdio "exit"). It's the **only** module that does external I/O. Depends on `InterpSwitch`, never the reverse. There is still some legacy code in here, so the naming and internal structure may seem odd.
    
- **`InterpCore`** — _per-switch execution._ Interpreting declarations in the program populates the state of all the switches at startup, including the switch's handlers, builtins, and pipeline configuration. Handlers and functions are closures over the switch's state, of which the pipeline, queues, and outbox are mutable.
    
- **`Interp`** — _the orchestrator._ The discrete-event loop: advances `global_time`, pops events from switch queues, runs handlers, drains mailboxes, loads input, and exposes `run`/`simulate`. Depends on all of the above.
    
Supporting modules: `Pipeline` (match-action stages backing arrays/tables), `InterpSyntax` (internal `ievent`/`loc`/`event_val`), `InterpControl` (control-plane commands), `InterpSocket`/`InterpStdio`/`InterpJson` (I/O + event formats), `InterpSim`/`InterpTopo` (config + topology links), `InterpSpec`/`Preprocess`/`InterpConfig` (setup), `InterpParsing`/`InterpDeparsing` (packet parse/deparse).

### Core types

- `code = state -> ival list -> ival` — every callable (builtin methods, user functions, actions, parsers), stored in `ival = V of value | F of (cid option * code)`.
- `handler = state -> int -> event_val -> unit` — event handlers; effects are local to the switch.
- `send_intent = FromIngress of ingress_destination * event_val | FromEgress of int * event_val` — a mailbox entry.

### Execution model (actor / mailbox)

1. An event sits in a switch's ingress queue. The orchestrator pops it and calls `execute_event`, which looks up the handler and runs it on that switch's `state`.
2. The handler runs program code (`InterpCore`): it reads/writes globals and mutates the `pipeline` in place, calls builtins/functions/actions (all `code`, dispatched by code block id), and — crucially — `generate` just **appends a `send_intent` to the switch's `outbox`**. It does _not_ deliver.
3. When the handler returns, `Interp` calls `InterpNetwork.drain_switch`, the single **delivery phase**: each queued intent is routed into a peer switch's ingress/egress queue, or out an interface (socket / stdio exit). External I/O happens only here.
4. The loop advances time and processes egress queues (which re-enter `execute_event` for egress handlers / default forwarding) until queues drain or `max_time`.

This is the actor model: a switch is an actor that emits messages into its mailbox; the fabric is the runtime that moves them. Generation and delivery are cleanly separated phases.

### Program / builtin model

The interpreter runs **CoreSyntax** (the midend IR) directly. Stateful globals — `Array`, `Counter`, `Table`, etc. — are **builtin library modules**: each registers a signature pairing types with `code` implementations, dispatched generically by id. Tables in particular are an ordinary `Table.t` builtin type plus `Table.create`/`lookup`/`install` calls (no special AST nodes) — actions are `DActionConstr` declarations, and a `Table.lookup` returning a record is handled by the generic tuple-assign machinery. Global constructors are run in `InterpCore.interp_dglobal` (`Table.create` dispatches to `Tables.create_ctor`; the older array/counter constructors are still inlined there).
# `ecn`

ECN-marks (and drops) IPv4 packets based on a synthesized queue-depth
signal. We also implement a basic queue model:

- A 1-cell `queuedepth` array stands in for the per-port queue.
- Every IPv4 packet bumps the cell atomically and reads back the new
  depth.
- A self-recursive `queue_decr` event drains the cell by 1 each time
  it fires. We launch it once from the spec; the handler re-arms
  itself via `generate(queue_decr())` for the rest of the simulation.

Three regimes:

| Depth (post-incr) | Action                  |
|-------------------|-------------------------|
| `<= ECN_THRESHOLD`  | forward unchanged       |
| `<= DROP_THRESHOLD` | forward with ECN = 0b11 |
| `>  DROP_THRESHOLD` | drop (no generate)      |

With `ECN_THRESHOLD = 4` and `DROP_THRESHOLD = 8`, a burst of 14
back-to-back packets cleanly walks the queue through all three.

## Files
- [ecn.dpt](ecn.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [ecn.json](ecn.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt ecn.dpt --spec ecn.json --silent
```

## Expected trace

```
sw 0 : OK   dst=...  depth=1 -> port 2
sw 0 : OK   dst=...  depth=2 -> port 2
sw 0 : OK   dst=...  depth=3 -> port 2
sw 0 : OK   dst=...  depth=4 -> port 2
sw 0 : MARK dst=...  depth=5 (>4) -> ecn=11
sw 0 : MARK dst=...  depth=6 (>4) -> ecn=11
sw 0 : MARK dst=...  depth=7 (>4) -> ecn=11
sw 0 : MARK dst=...  depth=8 (>4) -> ecn=11
sw 0 : DROP dst=...  (depth=9  > 8)
sw 0 : DROP dst=...  (depth=10 > 8)
...
sw 0 : OK   dst=...  depth=1 -> port 2     # trailing packets after drain
```

Exit packets confirm the marking in the wire bytes — the TOS byte
flips from `0x01` (ECT(1) preserved) to `0x03` (CE marked) right at
the ECN threshold, and the IPv4 checksum updates accordingly.

## Notable Lucid details

- **Recursive event for the drain.** A recursive event can be used to implement a background thread -- a handler that executes periodically over time. `queue_decr`'s handler is:
  ```
  handle queue_decr() {
    Array.setm(queuedepth, 0, sub1_floor, 0);
    generate(queue_decr());
  }
  ```
  The delay between `generate(e)` and `e`'s arrival and handler execution is the drain rate.
- **Memops are restricted enough to be just-barely-enough.** The
  drain uses `sub1_floor`:
  ```
  memop sub1_floor(int mv, int unused) {
    if (mv == 0) { return 0; }
    else         { return mv - 1; }
  }
  ```
  Each branch uses `mv` at most once (in the if condition or in the
  return), which keeps the memop within the "compiles to one atomic
  instruction" budget.
- **`Array.update` with the same memop on both sides** is the
  standard "atomic increment-and-fetch" idiom — get-side returns
  `mv+1`, set-side writes `mv+1`. The returned new depth is what we
  branch on.
- **Drop = don't generate.** No special "drop" call from a handler.
  Just skip the `generate_port` and the packet vanishes.

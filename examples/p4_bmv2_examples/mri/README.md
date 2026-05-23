# `mri`

Per-hop telemetry: every switch that handles a packet pushes a
`(swid, qdepth)` swtrace onto a stack inside the IPv4 options. The
destination host receives a packet whose option-bearing IPv4 header
contains the full hop chain (most-recent first).

## Files
- [mri.dpt](mri.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator, builds topology + table
  installs + initial (count=0) test packets.
- [mri.json](mri.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt mri.dpt --spec mri.json --silent
```

## Wire layout

```
[ eth | ipv4 (ihl≥6) | opt (4 B) | mri(count) | N × swtrace | payload ]
                                                ↑ each 8 B (swid + qdepth)
```

Sender always emits packets with `ihl=6`, `opt_len=4`, `count=0` (no
swtraces yet). Each switch adds 8 bytes per swtrace: `ihl += 2`,
`opt_len += 8`, `total_len += 8`, `count += 1`. The IPv4 header
checksum is recomputed (over the 20-byte ipv4 only, matching the P4
program's `update_checksum` invocation).

## Test cases
| # | Route                    | Expected swtraces in exit packet | Exit |
|---|--------------------------|----------------------------------|------|
| 1 | h1 → h2 direct (s1, s2)  | `[s2, s1]`                       | 1:1  |
| 2 | h1 → h3 direct (s1, s3)  | `[s3, s1]`                       | 2:1  |
| 3 | h1 → h2 detour (s1, s3, s2) — `10.0.99.99` is routed through s3 | `[s2, s3, s1]` | 1:1 |
| 4 | h1 → 10.99.99.99 (no route) | drop at s1                     | —    |

Swtraces appear in *push-front order* in the wire packet, so the
most-recent hop is at swtrace[0] and the oldest is at swtrace[N-1].

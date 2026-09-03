# `load_balance`

Hash-based ECMP forwarding across a 3-switch triangle. The trick: the
*magic IP* `10.0.0.1` indicates "load-balance across {h2, h3} by 5-tuple
hash." s1 is the load balancer; s2 and s3 are plain forwarders for their
own attached hosts. The rewrite happens at s1: the destination IP is
replaced with the chosen host's real IP (`10.0.2.2` or `10.0.3.3`)
before the packet is forwarded, so downstream switches see a normal
unicast packet.

## Files
- [load_balance.dpt](load_balance.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy-based generator. Builds the
  topology block, the table-install events, and the test packets (with
  valid IPv4 checksums) in one place.
- [load_balance.json](load_balance.json) — committed artifact, regenerate
  with `python gen_spec.py`.

## Running
```bash
./gen_spec.py
dpt load_balance.dpt --spec load_balance.json --silent
```

## Topology
3-switch triangle, one host per switch. Node IDs map `s1..s3 → 0..2`.

```
           h1                 h2
            |                  |
            1                  1
   [s1=0] 2 --------- 2 [s2=1] 3
          3                    |
          |                    3
          2                    |
        [s3=2] 1 -- h3 -------- (via s2:3 ↔ s3:3)
```

## Pipeline
The handler walks three tables in series for every TCP packet:

1. **`ecmp_group`** (LPM on `ip#dst`) returns `(grp_base, grp_count, hit)`.
   `count` must be a power of 2 (1 or 2 here). On miss, drop.
2. The handler hashes the 5-tuple
   `(ip#src, ip#dst, ip#protocol, tcp#src_port, tcp#dst_port)` to a
   14-bit value and computes `select = base + (hash & (count-1))`. Lucid
   has no `%` operator, so the count must be a power of two and we use
   bitwise AND.
3. **`ecmp_nhop`** (exact on `select`) returns
   `(nh_dmac, nh_dstip, nh_port, hit)`. The dst-IP rewrite lives here —
   for s1, `nh_dstip` is `10.0.2.2` or `10.0.3.3`, never the original
   `10.0.0.1`.
4. **`send_frame`** (exact on `nh_port`) returns `(fr_smac, fr_hit)`.
   On miss, the input smac is preserved (matches P4's NoAction default).

The handler then rewrites/updates headers and generates the packet event. 

## Test cases
- Six TCP flows from `h1 → 10.0.0.1` with different source ports
  (1111…6666). All 6 hit s1's `ecmp_group` entry for 10.0.0.1 and split
  across `{select=0 → h2, select=1 → h3}` based on hash. Expected: both
  buckets exercised across the run; exact split depends on the seed.
- One direct packet `h1 → 10.0.2.2`. s1 has no `ecmp_group` entry for
  10.0.2.2 (only for 10.0.0.1), so this drops at s1. Confirms s1 is
  *only* a load balancer, not a general router for these hosts.
- One unroutable packet `h1 → 10.99.99.99`. Drops at s1.

After a run, scan the `Exits` list and confirm packets show up at both
`1:1` (h2's port) and `2:1` (h3's port).

## Notes
- **`(int<W>)(rec#field)` not `(int<W>)rec#field`.** Casts bind tighter
  than `#` in Lucid, so the field-access has to be parenthesized.
- **gen_spec.py emits the whole spec.** topology + 11 `Table.install`
  events + 8 packet events.
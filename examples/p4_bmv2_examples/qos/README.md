# `qos`

Plain IPv4 forwarding (same as `basic`) plus a per-protocol DSCP
marking step applied before the table lookup:

| L4 protocol | Action                       | DSCP value |
|-------------|------------------------------|------------|
| UDP (17)    | Expedited Forwarding         | 46         |
| TCP (6)     | Voice Admit                  | 44         |
| anything else | leave diffserv unchanged   | —          |

## Files
- [qos.dpt](qos.dpt) — the Lucid program.
- [gen_spec.py](gen_spec.py) — scapy generator.
- [qos.json](qos.json) — generated artifact.

## Running
```bash
/opt/anaconda3/bin/python3 gen_spec.py
../../../sources/lucid/dpt qos.dpt --spec qos.json --silent
```

## Test cases (in `gen_spec.py`)

Each packet is `h1 → h2` over a single switch.

| Input              | Expected `dscp` | TOS byte in exit |
|--------------------|-----------------|------------------|
| UDP, input `tos=0` | 46              | `0xb8` (46<<2 \| 0) |
| TCP, input `tos=0` | 44              | `0xb0` (44<<2 \| 0) |
| ICMP, input `tos=0`| 0 (unchanged)   | `0x00`            |
| UDP, input `tos=0xfc` (dscp=63, ecn=00) | 46 | `0xb8` (dscp rewritten, ecn preserved) |

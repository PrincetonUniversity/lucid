### P4 Harnesses

A P4 harness is a partial P4 program that converts packets into lucid entry events and does simple handling of lucid exit events (e.g., updates fields of egress packets.)

This directory contains pre-written P4 harnesses.

- **ip_harness**: The IP harness parses ethernet / ip packets. By default it expects a lucid program with the following entry / exit events: 

```
entry event ip_in (int<<9>> igr_port, int src, int dst, int<<16>> len, int<<8>> tos);
```
```
exit event ip_out (int<<9>> egr_port, int src, int dst);
exit event port_out (int<<9>> egr_port);
```

If you change the entry event(s), update ``ip_harness_triggers.json``. If you change the exit event(s), update the p4 code that applies after ``@DPT_HANDERS`` in the ingress block. 
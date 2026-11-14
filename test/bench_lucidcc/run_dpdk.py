#!/usr/bin/env python3
"""Runs DPDK benchmarks. 
   bench.py copies this script to work dirs that it creates and runs it to execute 
   a benchmark. 

   This script expects the work dir its in to contain: 
        build/        the lucidcc --dpdk build dir
        job.json      { "replay": {port: pcap}, 
                        "preload": {port: pcap}, 
                        "output_ports": [port, ...],
                        "warmup_s", "duration_s"}
    It generates: 
        switch.stdout, switch.stderr, and result.json:
            {   "stats": [{rx, tx, recirc, nic-rx, nic-missed}, ...], 
                "measured": n,
                "counters": {...}, 
                "drops": {...}, 
                "preload_rx": {port: n}} or {"error": msg}

Replay ports are pcap vdevs that loop infinite over the input pcap.
Preload ports are pcap vdevs that replay their pcap once.
There are no output pcaps, so all output is dropped.
Output ports are opened with no pcap, just so the packet goes 
all the way through to the vdev. 
The lucid program runs with (--stats 1) to print stats every second.
"""

import json
import os
import re
import select
import signal
import struct
import subprocess
import time

EAL = ["--no-huge", "-l", "0", "-n", "1", "--no-pci", "-m", "2048"]
START_TIMEOUT_S = 60


def wait_for_line(proc, pattern, timeout, log_file):
    deadline, buf = time.time() + timeout, ""
    while time.time() < deadline:
        if proc.poll() is not None:
            return False
        r, _, _ = select.select([proc.stderr], [], [], 0.2)
        if r:
            chunk = os.read(proc.stderr.fileno(), 4096).decode(errors="replace")
            log_file.write(chunk)
            log_file.flush()
            buf += chunk
            if re.search(pattern, buf):
                return True
    return False


STATS_RE = re.compile(r"^stats rx (\d+) tx (\d+) recirc (\d+) nic-rx (\d+) nic-missed (\d+) pkt/s$", re.M)
COUNTERS_RE = re.compile(r"^rx (\d+) tx (\d+) recirc (\d+) dropped: (.*)$", re.M)


def run():
    job = json.load(open("job.json"))
    r = subprocess.run(["make", "-C", "build"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    binf = os.path.join("build", "build", "lucidprog")
    if r.returncode != 0 or not os.path.exists(binf):
        raise RuntimeError("make failed:\n" + "\n".join(r.stdout.strip().splitlines()[-10:]))

    with open("empty.pcap", "wb") as f:   # an output port's (non-replayed) input
        f.write(struct.pack("<IHHiIII", 0xa1b2c3d4, 2, 4, 0, 0, 65535, 1))
    vdevs, ports = [], []
    for port, pcap in sorted(job["replay"].items(), key=lambda x: int(x[0])):
        vdevs.append(f"net_pcap{len(vdevs)},rx_pcap={pcap},infinite_rx=1")
        ports.append(int(port))
    preload_devs = {}   # DPDK port -> program port
    for port, pcap in sorted(job.get("preload", {}).items(), key=lambda x: int(x[0])):
        preload_devs[len(vdevs)] = int(port)
        vdevs.append(f"net_pcap{len(vdevs)},rx_pcap={pcap}")
        ports.append(int(port))
    for port in job["output_ports"]:
        vdevs.append(f"net_pcap{len(vdevs)},rx_pcap=empty.pcap")
        ports.append(int(port))
    cmd = [binf, *EAL]
    for v in vdevs:
        cmd += ["--vdev", v]
    cmd += ["--", "--stats", "1"]
    for i, port in enumerate(ports):
        cmd += ["--port", f"{port}:{i}"]
    print("[run_dpdk] run " + " ".join(cmd[1:]), flush=True)

    stdout_f, stderr_f = open("switch.stdout", "w"), open("switch.stderr", "w")
    sw = subprocess.Popen(cmd, stdout=stdout_f, stderr=subprocess.PIPE)
    try:
        if not wait_for_line(sw, r"handler loop running", START_TIMEOUT_S, stderr_f):
            raise RuntimeError("the switch did not start (see switch.stderr)")
        time.sleep(job["warmup_s"] + job["duration_s"] + 0.5)
        sw.send_signal(signal.SIGINT)
        _, rest = sw.communicate(timeout=30)
        stderr_f.write(rest.decode(errors="replace"))
    finally:
        if sw.poll() is None:
            sw.kill()
        stderr_f.close()
        stdout_f.close()

    err = open("switch.stderr").read()
    stats = [dict(zip(["rx", "tx", "recirc", "nic-rx", "nic-missed"], map(int, m.groups())))
             for m in STATS_RE.finditer(err)]
    m = None
    for m in COUNTERS_RE.finditer(err):
        pass
    if m is None:
        raise RuntimeError("no shutdown counters in switch.stderr")
    counters = {"rx": int(m.group(1)), "tx": int(m.group(2)), "recirc": int(m.group(3))}
    drops = {k: int(v) for k, v in re.findall(r"([a-z-]+) (\d+)", m.group(4))}
    # each preload device's own receive count (the driver's "DPDK port i: rx n ..." lines)
    nic_rx = {int(d): int(n) for d, n in re.findall(r"^DPDK port (\d+): rx (\d+)", err, re.M)}
    preload_rx = {str(port): nic_rx.get(dev, 0) for dev, port in preload_devs.items()}
    # the stats lines after the warm-up (one per second; the last may be partial)
    measured = stats[job["warmup_s"]:job["warmup_s"] + job["duration_s"]]
    return {"stats": stats, "measured": len(measured), "measured_stats": measured,
            "counters": counters, "drops": drops, "preload_rx": preload_rx}


def main():
    try:
        result = run()
    except Exception as e:
        result = {"error": str(e)}
    with open("result.json", "w") as f:
        json.dump(result, f)
    print(f"[run_dpdk] {result.get('error') or 'done'}", flush=True)


if __name__ == "__main__":
    main()

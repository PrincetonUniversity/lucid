#!/usr/bin/env python3
"""Internal to ltf.py: the dpdk target's executor. Not meant to be run directly.

It's a separate script so it can run where DPDK is, while ltf.py runs on the host.

ltf calls this script as:
  ltf_dpdk.py CASE_DIR [CASE_DIR ...]

CASE_DIR is a case's work dir, as produced by ltf.py:
  encoded/in_<port>.pcap  per-port input pcaps from lucidEncode
  dpdk/build/             the lucidcc --dpdk build dir
  dpdk/ports.json         {"ports": [program port, ...]}: one veth pair each

outputs, to CASE_DIR:
  dpdk/out_<port>.pcap    switch output on each port (tcpdump -Q in on the peer)
  dpdk/switch.stdout      switch printfs
  dpdk/switch.stderr      the driver's messages
  dpdk/result.json        {"counters": {...}, "drops": {cause: n, ...}} or {"error": msg}

For each case: make; one veth pair per port (ltf<i> for DPDK, ltf<i>p for the harness);
one capture per port; start the switch and wait for "handler loop running"; replay every
port's inputs on one timeline; wait QUIET_S; SIGINT the switch; read its shutdown counters.
"""
import json
import os
import re
import select
import signal
import socket
import struct
import subprocess
import sys
import time

QUIET_S = 1.0          # wait after the last input before stopping the switch
START_TIMEOUT_S = 60   # for the switch's "handler loop running" (EAL init is slow under emulation)
EAL = ["--no-huge", "-l", "0", "-n", "1", "--no-pci", "-m", "1024"]


def log(msg):
    print(f"[ltf_dpdk] {msg}", flush=True)


def read_pcap(path):
    """[(time_ns, frame)] from a little-endian micro- or nanosecond pcap"""
    data = open(path, "rb").read()
    magic = struct.unpack("<I", data[:4])[0]
    scale = 1 if magic == 0xa1b23c4d else 1000
    out, off = [], 24
    while off + 16 <= len(data):
        sec, frac, incl, _ = struct.unpack("<IIII", data[off:off + 16])
        out.append((sec * 10**9 + frac * scale, data[off + 16:off + 16 + incl]))
        off += 16 + incl
    return out


def sh(cmd, check=True):
    return subprocess.run(cmd, check=check, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)


def make_veths(n):
    pairs = []
    for i in range(n):
        a, b = f"ltf{i}", f"ltf{i}p"
        sh(["ip", "link", "del", a], check=False)
        sh(["ip", "link", "add", a, "type", "veth", "peer", "name", b])
        for iface in (a, b):
            sh(["sysctl", "-w", f"net.ipv6.conf.{iface}.disable_ipv6=1"], check=False)
            sh(["ip", "link", "set", iface, "up"])
        pairs.append((a, b))
    return pairs


def del_veths(pairs):
    for a, _ in pairs:
        sh(["ip", "link", "del", a], check=False)


def wait_for_line(proc, stream, pattern, timeout, log_file):
    """read proc's stream (copying it to log_file) until a line matches pattern"""
    deadline = time.time() + timeout
    buf = ""
    while time.time() < deadline:
        if proc.poll() is not None:
            return False
        r, _, _ = select.select([stream], [], [], 0.2)
        if r:
            chunk = os.read(stream.fileno(), 4096).decode(errors="replace")
            log_file.write(chunk)
            log_file.flush()
            buf += chunk
            if re.search(pattern, buf):
                return True
    return False


def start_captures(pairs, ports, outdir):
    caps = []
    for (a, b), port in zip(pairs, ports):
        path = os.path.join(outdir, f"out_{port}.pcap")
        cap = subprocess.Popen(["tcpdump", "-i", b, "-Q", "in", "-U", "-n", "-B", "4096",
                                "--time-stamp-precision=nano", "-w", path],
                               stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        with open(os.devnull, "w") as sink:
            if not wait_for_line(cap, cap.stderr, r"listening on", 10, sink):
                raise RuntimeError(f"tcpdump on {b} did not start")
        caps.append(cap)
    return caps


def stop_captures(caps):
    time.sleep(0.2)  # let the last frames land
    for cap in caps:
        cap.send_signal(signal.SIGINT)
    for cap in caps:
        try:
            cap.wait(timeout=5)
        except subprocess.TimeoutExpired:
            cap.kill()


def replay(pairs, ports, case_dir):
    """send every port's inputs at their pcap timestamps, on one timeline"""
    socks, timeline = {}, []
    for (a, b), port in zip(pairs, ports):
        path = os.path.join(case_dir, "encoded", f"in_{port}.pcap")
        if not os.path.exists(path):
            continue
        s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW)
        s.bind((b, 0))
        socks[port] = s
        for t, frame in read_pcap(path):
            if len(frame) < 14:
                raise RuntimeError(f"an input on port {port} is {len(frame)} bytes: a frame on a "
                                   "veth needs at least 14 (an ethernet header)")
            timeline.append((t, port, frame))
    timeline.sort(key=lambda x: x[0])   # stable: same-time inputs keep file order
    t0 = time.monotonic()
    late = 0.0
    for t, port, frame in timeline:
        delay = t0 + t / 1e9 - time.monotonic()
        if delay > 0:
            time.sleep(delay)
        else:
            late = max(late, -delay)
        socks[port].send(frame)
    for s in socks.values():
        s.close()
    return len(timeline), late


COUNTERS_RE = re.compile(r"^rx (\d+) tx (\d+) recirc (\d+) dropped: (.*)$", re.M)

def parse_counters(stderr_text):
    m = None
    for m in COUNTERS_RE.finditer(stderr_text):
        pass
    if m is None:
        return None, None
    counters = {"rx": int(m.group(1)), "tx": int(m.group(2)), "recirc": int(m.group(3))}
    drops = {k: int(v) for k, v in re.findall(r"([a-z-]+) (\d+)", m.group(4))}
    return counters, drops


def run_case(case_dir):
    out = os.path.join(case_dir, "dpdk")
    build = os.path.join(out, "build")
    ports = json.load(open(os.path.join(out, "ports.json")))["ports"]
    for f in os.listdir(out):
        if f.startswith("out_") or f.startswith("switch."):
            os.remove(os.path.join(out, f))

    log(f"{case_dir}: make")
    r = sh(["make", "-C", build], check=False)
    binf = os.path.join(build, "build", "lucidprog")
    if r.returncode != 0 or not os.path.exists(binf):
        raise RuntimeError("make failed:\n" + "\n".join(r.stdout.strip().splitlines()[-10:]))

    pairs = make_veths(len(ports))
    caps, sw = [], None
    try:
        caps = start_captures(pairs, ports, out)
        cmd = [binf, *EAL]
        for i, (a, _) in enumerate(pairs):
            cmd += ["--vdev", f"net_af_packet{i},iface={a}"]
        cmd += ["--"]
        for i, port in enumerate(ports):
            cmd += ["--port", f"{port}:{i}"]
        log(f"{case_dir}: run {' '.join(cmd[1:])}")
        stdout_f = open(os.path.join(out, "switch.stdout"), "w")
        stderr_f = open(os.path.join(out, "switch.stderr"), "w")
        sw = subprocess.Popen(cmd, stdout=stdout_f, stderr=subprocess.PIPE)
        if not wait_for_line(sw, sw.stderr, r"handler loop running", START_TIMEOUT_S, stderr_f):
            raise RuntimeError("the switch did not start (see dpdk/switch.stderr)")
        n, late = replay(pairs, ports, case_dir)
        log(f"{case_dir}: replayed {n} input(s)" + (f", up to {late * 1e3:.1f} ms late" if late > 1e-3 else ""))
        time.sleep(QUIET_S)
        sw.send_signal(signal.SIGINT)
        _, rest = sw.communicate(timeout=30)
        stderr_f.write(rest.decode(errors="replace"))
        stderr_f.close()
        stdout_f.close()
        sw = None
    finally:
        if sw is not None and sw.poll() is None:
            sw.kill()
        stop_captures(caps)
        del_veths(pairs)
    counters, drops = parse_counters(open(os.path.join(out, "switch.stderr")).read())
    if counters is None:
        raise RuntimeError("no shutdown counters in dpdk/switch.stderr")
    return {"counters": counters, "drops": drops, "replay_late_ms": round(late * 1e3, 3)}


def main():
    for case_dir in sys.argv[1:]:
        try:
            result = run_case(case_dir)
        except Exception as e:  # report and go on to the next case
            result = {"error": str(e)}
        with open(os.path.join(case_dir, "dpdk", "result.json"), "w") as f:
            json.dump(result, f)
        log(f"{case_dir}: {result}")


if __name__ == "__main__":
    main()

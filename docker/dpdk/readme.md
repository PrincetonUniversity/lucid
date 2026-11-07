# DPDK in a container, for local correctness tests

- `Dockerfile`: Ubuntu 22.04 with DPDK 24.03 built for generic x86-64 
  with only the vdev bus, the ring mempool and the pcap, af_packet 
  and tap drivers. It is an amd64 image.
- `run.sh DIR CMD...`: run a command in the image with 
  DIR mounted as the working directory (with `NET_ADMIN`, for veth tests).

Example usage:
```
docker build --platform linux/amd64 -t lucid-dpdk docker/dpdk   # once, a few minutes (525 MB)
docker/dpdk/run.sh BUILD_DIR make                               # build a program's C in the image
make test-cc-dpdk                                               # the lucidcc dpdk tests (test/lucidcc)
```

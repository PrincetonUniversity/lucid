# DPDK 24.03 for running lucidcc --dpdk programs locally, for correctness only:
# the pcap, af_packet and tap virtual devices, no NIC, no hugepages
# (`--no-huge --no-pci`). Built for generic x86-64 so that it runs under
# emulation on an arm64 laptop (no AVX).
#   docker build --platform linux/amd64 -t lucid-dpdk docker/dpdk
FROM ubuntu:22.04
ARG VER=24.03
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential pkg-config meson ninja-build python3-pyelftools \
      libnuma-dev libpcap-dev libbsd-dev wget xz-utils ca-certificates \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
RUN wget -q https://fast.dpdk.org/rel/dpdk-$VER.tar.xz && tar -xJf dpdk-$VER.tar.xz && rm dpdk-$VER.tar.xz
# only what a pcap/af_packet run needs: the vdev bus, the ring mempool, the three drivers
RUN cd dpdk-$VER && meson setup build \
      -Dplatform=generic \
      -Denable_drivers=bus/vdev,bus/pci,mempool/ring,net/pcap,net/af_packet,net/tap \
      -Denable_apps=dumpcap -Dtests=false -Dexamples= \
    && ninja -C build && ninja -C build install && ldconfig \
    && rm -rf /src
RUN pkg-config --modversion libdpdk
# veth + capture tools for test/ltf's dpdk target (python3-scapy and tcpreplay are no
# longer used by it)
RUN apt-get update && apt-get install -y --no-install-recommends \
      python3-scapy iproute2 tcpdump tcpreplay procps \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /work

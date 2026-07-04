FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# ---- system setup ----

# 1. base packages
RUN apt-get update && apt-get install -y \
    sudo ca-certificates curl git \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    build-essential libpython3-dev tcpdump tcpreplay python3-scapy opam \
    pkg-config libgmp-dev m4 zlib1g-dev \
    iproute2 net-tools iputils-ping iptables \
    vim nano less procps \
    meson ninja-build python3-pyelftools libnuma-dev libpcap-dev libelf-dev \
    wget xz-utils \
    && rm -rf /var/lib/apt/lists/*

# 2. passwordless sudo
RUN echo "ubuntu ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/ubuntu \
    && chmod 0440 /etc/sudoers.d/ubuntu

# ---- DPDK 24.03 (dev only: net/pcap + net/af_packet PMDs) ----
# Pinned to 24.03 to match our other project. The driver set is reduced to the
# two virtual PMDs we test with (pcap / af_packet), which keeps the build
# tractable under emulation; performance is not a goal here. To build the full
# driver set instead, drop the `-Denable_drivers=...` flag.
#
# On amd64 the CPU baseline is pinned to westmere (SSE4.2, no AVX) so the binary
# runs under x86-on-ARM emulation -- Rosetta/qemu expose a CPU without AVX, and a
# DPDK built assuming AVX aborts in rte_eal_init ("unsupported cpu type"). It's
# harmless on real x86 hosts (just no AVX-optimized paths). arm64 uses DPDK's
# default baseline.
ARG TARGETARCH
RUN wget -q https://fast.dpdk.org/rel/dpdk-24.03.tar.xz \
    && tar -xJf dpdk-24.03.tar.xz && rm dpdk-24.03.tar.xz \
    && cd dpdk-24.03 \
    && case "$TARGETARCH" in \
         amd64) ISA_OPT="-Dcpu_instruction_set=westmere" ;; \
         *)     ISA_OPT="" ;; \
       esac \
    && meson setup build -Denable_drivers=net/pcap,net/af_packet -Dtests=false $ISA_OPT \
    && ninja -C build \
    && meson install -C build \
    && ldconfig \
    && cd .. && rm -rf dpdk-24.03

# ---- user setup ----
USER ubuntu
WORKDIR /home/ubuntu

# 3. opam setup + switch 
RUN opam init -y --auto-setup --disable-sandboxing \
    && opam switch create 4.12.0

# 4. opam env setup covering bash + entrypoint
RUN echo 'test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh' >> ~/.bashrc
RUN printf '#!/bin/bash\neval "$(opam env)"\nexec "$@"\n' > /home/ubuntu/entrypoint.sh \
    && chmod +x /home/ubuntu/entrypoint.sh
ENTRYPOINT ["/home/ubuntu/entrypoint.sh"]

# 5. lucid opam deps (the long list — changes when deps change)
RUN opam install -y --confirm-level=unsafe-yes \
        odoc integers "batteries=3.5.1" ounit ANSITerminal menhir \
        ppx_deriving ppx_string_interpolation zarith visitors fileutils \
        ppx_import "core<=v0.14.1" "dune=3.15.3" ocamlgraph angstrom \
        "yojson=2.2.2" pyml pprint z3 "pp<=1.2.0" "cstruct=6.2.0" "ppx_cstruct=6.2.0"

# 5b. editor tooling: LSP + formatter (ocamlformat pinned to match .ocamlformat).
# Pre-installing these stops the VSCode OCaml Platform extension from trying to
# install them into the switch on first attach.
RUN opam install -y --confirm-level=unsafe-yes \
        ocaml-lsp-server "ocamlformat=0.19.0"

# 6. (optional) install claude
RUN curl -fsSL https://claude.ai/install.sh | bash
ENV PATH="/home/ubuntu/.local/bin:${PATH}"

CMD ["bash"]
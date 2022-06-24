FROM ocaml/opam:ubuntu-18.04-ocaml-4.11
MAINTAINER John Sonchack
RUN sudo apt-get update -y && sudo apt-get install -y \
    software-properties-common \
    python3 \
    python3-pip \ 
    m4 \
    curl \
    make \
    build-essential \
    libgmp-dev \
    pkg-config \
    tcpreplay \
    expect && \
    pip3 install dpkt
RUN opam install -y integers \
    batteries \
    ounit \
    ansiterminal \
    menhir \
    ppx_deriving \
    ppx_string_interpolation \
    zarith \
    visitors \
    fileutils \
    ppx_import \
    core.v0.14.1 \
    dune \
    ocamlgraph \
    angstrom \
    yojson.1.7.0 \
    pyml \
    z3
RUN opam init
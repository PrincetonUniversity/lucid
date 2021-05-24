# install DPT dependencies for ubuntu 18.04
echo "----installing packages from apt ----"
sudo apt-get update -y
sudo apt-get install -y software-properties-common sudo
sudo apt-get update -y
sudo apt-get install -y python3 m4 curl make build-essential python2.7 libgmp-dev pkg-config

echo "----initializing opam + ocaml packages ----"
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -y
sudo apt-get install -y opam
opam init -a -y --compiler=4.11.1 
# opam switch create 4.11.1
eval $(opam env)
echo "eval 'opam config env'" >> ~/.bashrc
opam install -y \
    integers \
    batteries \
    ounit \
    ansiterminal \
    menhir \
    ppx_deriving \
    ppx_deriving_argparse \
    ppx_string_interpolation \
    zarith \
    visitors \
    fileutils \
    ppx_import \
    core \
    dune \
    ocamlgraph \
    z3 \
    yojson \
    angstrom
eval $(opam env)
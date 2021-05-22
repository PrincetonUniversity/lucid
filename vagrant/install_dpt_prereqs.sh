# install DPT dependencies for ubuntu 18.04
# (as of 2/19/21)
echo "----installing prereqs----"
sudo apt-get update -y
sudo apt-get install -y software-properties-common sudo
sudo apt-get update -y
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -y
sudo apt-get install -y python3 m4 curl opam make build-essential python2.7 libgmp-dev pkg-config

echo "----initializing opam----"
opam init -a -y
eval $(opam env)
opam switch create 4.11.1
eval $(opam env)
echo "eval 'opam config env'" >> ~/.bashrc
opam install z3
eval $(opam env)
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
# opam install ANSITerminal batteries core ocamlgraph ounit2 ppx_deriving ppx_import ppx_string_interpolation visitors menhir
eval $(opam env)

echo "----downloading up Lucid----"
cd ~/; git clone --single-branch --branch sigcomm21_artifact https://github.com/PrincetonUniversity/lucid

echo "----building lucid----"
cd ~/lucid; make
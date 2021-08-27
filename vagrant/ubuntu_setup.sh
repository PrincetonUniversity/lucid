echo "***** installing bf-sde *****"
SDE_PARENT="/home/vagrant"
SDE_FILE="bf-sde-9.5.0.tgz"
SDE_DIR="bf-sde-9.5.0"
cd /lucid/vagrant
if [ -e $SDE_FILE ]
then 
  # install bf-sde-9.5.0.tgz
  cp $SDE_FILE $SDE_PARENT
  cp set_sde.bash $SDE_PARENT
  cd $SDE_PARENT
  tar -xzf $SDE_FILE
  cd $SDE_DIR
  echo "vm_sde_profile:
    global_configure_options: ''
    package_dependencies:
    - thrift
    - grpc
    packages:
    - bf-syslibs:
      - bf_syslibs_configure_options: ''
    - bf-utils:
      - bf_utils_configure_options: ''
    - bf-drivers:
      - bf_drivers_configure_options: ''
      - bf-runtime
      - p4-runtime
      - pi
    - ptf-modules
    tofino_architecture: tofino" > ./p4studio_build/profiles/vm_sde_profile.yaml
  ./p4studio_build/p4studio_build.py -up vm_sde_profile
  echo ". $SDE_PARENT/set_sde.bash" >> ~/.bashrc
else
  echo "not installing p4 studo SDE: $SDE_FILE not found"
fi
echo "***** installing Lucid toolchain dependencies *****"
# install DPT dependencies for ubuntu 18.04
echo "----installing packages from apt ----"
sudo apt-get update -y
sudo apt-get install -y software-properties-common sudo
sudo apt-get update -y
sudo apt-get install -y python3 m4 curl make build-essential python2.7 libgmp-dev pkg-config tcpreplay expect
pip3 install dpkt

echo "----setting up hugepages----"
echo 'vm.nr_hugepages=128' | sudo tee /etc/sysctl.d/hugepages.conf
sudo mount -t hugetlbfs none /dev/hugepages
sudo sysctl -w vm.nr_hugepages=128

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


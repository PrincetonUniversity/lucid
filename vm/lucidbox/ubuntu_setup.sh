# this script installs all lucid dependencies
# run this script from the same directory as 
# bf-sde-9.5.0.tgz and set_sde.bash
# to also install the p4-studio sde

function abs_path() {
    echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"    
}


if [[ $1 ]]
then
  echo "setting up for $1"
  mode=$1
  if [ $mode == "compiler" ]
  then 
    if [[ $2 ]]
    then 
      SDE_FILE=$(abs_path $2)
      SDE_DIR=$(basename $SDE_FILE .tgz)
      SDE_DIR="$(pwd)/$SDE_DIR"
    else
      echo "usage: ubuntu_setup.sh compiler <bf-sde-9.5.0.tgz> <set_sde.bash> -- setup lucid requirements and tofino sde (bf-sde)."
    fi
    if [[ $3 ]]
    then 
      SET_SCRIPT=$(abs_path $3)
    else
      echo "usage: ubuntu_setup.sh compiler <bf-sde-9.5.0.tgz> <set_sde.bash> -- setup lucid requirements and tofino sde (bf-sde)."
    fi
  fi
else
  echo "usage: ubuntu_setup.sh compiler <bf-sde-9.5.0.tgz> <set_sde.bash> -- setup lucid requirements and tofino sde (bf-sde)."
  echo "usage: ubuntu_setup.sh interpreter -- setup lucid requirements only (no bf-sde)."
  exit 1
fi

echo "***** installing requirements to run: lucid $mode *****"

# install compiler requirements if selected.
if [ $mode == "compiler" ]
then
  if [[ -f $SDE_FILE && -f $SET_SCRIPT ]]
  then   
    echo "installing sde from: $SDE_FILE --> $SDE_DIR"
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

    echo "----setting up hugepages for p4studio----"
    echo 'vm.nr_hugepages=128' | sudo tee /etc/sysctl.d/hugepages.conf
    sudo mount -t hugetlbfs none /dev/hugepages
    sudo sysctl -w vm.nr_hugepages=128
    echo "----adding SDE env variables to bashrc----"
    echo ". $SET_SCRIPT" >> ~/.bashrc
  else
    echo "p4 studio cannot install: either $SDE_FILE or $SET_SCRIPT not found"
    exit 1
  fi
fi

# install interpreter requirements no matter what. 
echo "----installing Lucid dependencies----"
# install DPT dependencies for ubuntu 18.04
echo "----installing packages from apt ----"
sudo apt-get update -y
sudo apt-get install -y software-properties-common sudo
sudo apt-get update -y
sudo apt-get install -y python3 python3-pip m4 curl make build-essential python2.7 libgmp-dev pkg-config tcpreplay expect
pip3 install dpkt

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
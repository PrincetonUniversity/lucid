BOX_BASE="lucid"
BUILD_DIR="lucidbox"
# configurable build parameters
CPUS=6

echo "checking if vagrant and virtualbox are installed..."
if which vagrant > /dev/null ; then 
    echo "vagrant installed"
else
    echo "vagrant not installed! Install from: https://www.vagrantup.com/downloads"
    echo "(originally built with vagrant 2.2.16)"
    exit
fi

if which vboxmanage > /dev/null ; then 
    echo "virtualbox installed"
else
    echo "virtualbox not installed! Install from: https://www.virtualbox.org/wiki/Downloads"
    echo "(originally built with virtualbox 6.1.8)"
    exit
fi


function abs_path() {
    echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"    
}
function my_path() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
    echo $SCRIPT_DIR
}

if [[ $1 ]]
then
  mode=$1
  echo "building box for $1"
  if [[ $mode == "compiler" ]]
  then 
    if [[ $2 ]]
    then 
      SDE=$(abs_path $2)
      SET_SCRIPT=$(abs_path $(my_path)/lucidbox/set_sde.bash)
      echo "barefoot sde: $SDE"
      echo "set_sde.bash: $SET_SCRIPT"
    else
      echo "usage: buildbox.sh compiler <bf-sde-9.5.0.tgz> -- build a vm with lucid requirements and the tofino sde (bf-sde)."
      exit
    fi 
  fi
else
  echo "usage: buildbox.sh interpreter -- build a vm with lucid requirements."
  echo "usage: buildbox.sh compiler <bf-sde-9.5.0.tgz> -- build a vm with lucid requirements and the tofino sde (bf-sde)."
  exit
fi

# build either lucid_compiler.box or lucid_interpreter.box
BOX="$BOX_BASE$mode.box"


if [ -e $BOX ]
then 
    echo "$BOX already present"
else
    if [ -e $BUILD_DIR/$BOX ]
    then 
        echo "$BOX already built"
        mv $BUILD_DIR/$BOX ./
    else
        echo "Building new $BOX"
        # construct vagrant command. 
        cmd="vagrant"
        cmd="$cmd --cpus=$CPUS"
        if [[ $mode == "compiler" ]]
        then
          cmd="$cmd --sde=$(abs_path $SDE) --set_script=$(abs_path $SET_SCRIPT)"
        fi
        cmd="$cmd --mode=$mode up --provider=virtualbox"
        # execute command in build directory. 
        cd "$(my_path)/$BUILD_DIR"
        echo "running vagrant command: $cmd"
        echo "******************"
        $cmd \
        && vagrant reload \
        && echo "clearing unused space for smaller image" \
        && vagrant ssh -c "sudo dd if=/dev/zero of=/EMPTY bs=4096k; sudo rm -f /EMPTY" \
        && echo "packaging into $BOX" \
        && vagrant package --output "$BOX" \
        && echo "destroying default box" \
        && vagrant destroy -f \
        && echo "done! lucid vm built in: $BOX" \
        && mv $BOX ../ 
    fi
fi
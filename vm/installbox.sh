BOX_BASE="lucid"
# 1) make sure vagrant and virtualbox are installed.
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

# 2) set box file name
if [[ $1 == "compiler" ]]
then
  mode=$1
else
    if [[ $1 == "interpreter" ]]
    then
        mode=$1
    else
        echo "usage: setupvm.sh interpreter -- setup a vm based on lucidinterpreter.box"
        echo "usage: setupvm.sh compiler -- setup a vm based on lucidcompiler.box"
    fi
fi

BOX_FN="$BOX_BASE$mode.box"

# 3) add box named "lucid" to vagrant
if [ -e $BOX_FN ]
then 
    # remove the current lucid box, if it exists.
    echo "attempting to remove current lucid box (if it exists)"
    vagrant box remove lucid
    echo "initializing lucid box from $BOX_FN"
    vagrant box add lucid $BOX_FN
    # vagrant init lucid
    echo "vagrant setup complete."
    echo "Start vm: 'vagrant up'\nLogin to vm: 'vagrant ssh'"
else
    echo "error: lucid vagrant box ($BOX_FN) not found."
    echo "use createbox.sh to build it -- see readme.md for instructions"
fi
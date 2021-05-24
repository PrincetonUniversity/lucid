# set up the vm.
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

# 2) initialize vm
if [ -e lucidvm.box ]
then 
    echo "initializing lucid vm lucidvm.box"
    if [ -e Vagrantfile ]
    then 
        echo "vm initialization was already done (Vagrantfile exists). Do a vagrant delete to re-init."
    else
        vagrant box add lucidvm lucidvm.box
        vagrant init lucidvm
        vagrant up --provider=virtualbox
        echo "vm initialization complete. use 'vagrant ssh' to login"
    fi
else
    echo "error: lucidvm.box not found."
    echo "see vagrant/readme.md for instructions to build it from source"
fi

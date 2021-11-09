BOX="lucid.box"
# 1) make sure vagrant and virtualbox are installed.
echo "checking if vagrant and virtualbox are installed..."
if which vagrant > /dev/null ; then
    echo "vagrant installed"
else
    echo "vagrant not installed! Install from: https://www.vagrantup.com/downloads"
    echo "(originally built with vagrant 2.2.16)"
    exit
fi

if (which vboxmanage > /dev/null) || (which VBoxManage > /dev/null); then
    echo "virtualbox installed"
else
    echo "virtualbox not installed! Install from: https://www.virtualbox.org/wiki/Downloads"
    echo "(originally built with virtualbox 6.1.8)"
    exit
fi

# 2) add box to vagrant
if [ -e $BOX ]
then
    echo "initializing lucid vm $BOX"
    vagrant box add lucid $BOX
    # vagrant init lucid
    echo "vagrant setup complete."
    echo "Start vm: 'vagrant up'\nLogin to vm: 'vagrant ssh'"
else
    echo "error: lucid vagrant box ($BOX) not found."
    echo "use createbox.sh to build it -- see readme.md for instructions to build it"
fi

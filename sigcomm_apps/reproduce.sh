# this should be run inside the VM!

# compile lucid to p4
make p4

# if the sde is installed, also compile the p4 to tofino bins
if [ -d "/home/vagrant/bf-sde-9.5.0" ]
then
    export SDE=/home/vagrant/bf-sde-9.5.0
    export SDE_INSTALL=/home/vagrant/bf-sde-9.5.0/install
    export PATH=$PATH:$SDE_INSTALL/bin
    make tofino
fi

# report stats
make stats
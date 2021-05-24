# copy sde to ~/ in the vm and install it to ~/bf-sde-9.5.0
if [ -e bf-sde-9.5.0.tar ]
then 
    vagrant upload bf-sde-9.5.0.tar
    vagrant upload vagrant/install_bf_sde.sh
    echo "copied sde files to ~vagrant in vm"
    echo "installing sde (this may take a while)"
    # ssh into vm and run ~/install_bf_sde.sh
    vagrant ssh -c "./install_bf_sde.sh"
else
    echo "cannot install tofino sde, bf-sde-9.5.0.tar not found."
fi

#### Setting up a Lucid VM to use the sigcomm artifact

0. put ``Vagrantfile`` and ``install_dpt_prereqs.sh`` in the directory where you want the vm. 
1. If you have a copy of the barefoot sde "bf-sde-9.5.0.tar", put it in the directory as well. This is only necessary if you want to compile Lucid code to the Tofino.
2. From that directory, run ``vagrant up`` to set up the VM. This will take 30-60 minutes. 
3. A local copy of the lucid repo will be in ``~/lucid``, with Lucid compiled. 
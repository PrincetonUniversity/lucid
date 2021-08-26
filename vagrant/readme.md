#### Lucid Vagrant VM

**To build a Lucid virtualbox VM:**

1. Put a copy of bf-sde-9.5.0.tgz in this directory if you want the VM to be built with a local installation of the p4 studio sde.
2. Run ``vagrant up``
3. Enter the VM with ``vagrant ssh``.
4. The main repo directory is shared to ``/lucid`` inside of the VM.

**To build a Vagrant appliance box**, lucid.box, run ``./build_vm.sh``

#### Lucid development VMs

``./buildbox.sh interpreter`` -- builds a lucid vm box with everything preinstalled to run the lucid interpreter or lucid to P4 compiler. 

``./buildbox.sh compiler <bf-sde-9.5.0.tgz>`` -- builds a lucid vm box with everything preinstalled to run the lucid interpreter, lucid compiler, and also the p4 to tofino 
compiler in the tofino sde (bf-sde). Note that you need to get a copy of the tofino 
sde yourself (bf-sde-9.5.0.tgz). Also, this has only been tested with sde 9.5.0, 
though it may also work with 9.5.1. The box may take 1-2 hours to build.

``./installbox.sh <interpreter | compiler>`` -- sets up a local lucid vm based on a lucid box prepared with ``buildbox``. 

``./lucidbox/ubuntu_setup.sh`` -- Installs lucid dependencies and p4 studio. Runs inside the VM that ``buildbox.sh`` creates, but you can also use it on your own vm or server. When you put ``bf-sde-9.5.0.tgz`` and ``set_sde.bash`` in the same directory as the script, the script will install bf-sde-9.5.0 to ``~/``.
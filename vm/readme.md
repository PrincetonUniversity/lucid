#### Lucid development VM

``./buildbox.sh`` -- builds the lucid vm box, ``lucid.box``. Put a copy bf-sde-9.5.0.tgz in this directory beforehand to build a box that can compile and run P4. Else, the box will just run the lucid interpreter. The box may take 1-2 hours to build.

``./setupvm.sh`` -- sets up a local lucid vm based on the lucid box.

``./lucidbox/ubuntu_setup.sh`` -- Installs lucid dependencies and p4 studio. Runs inside the VM that ``buildbox.sh`` creates, but you can also use it on your own vm or server. Put ``bf-sde-9.5.0.tgz`` and ``set_sde.bash`` in the same directory to install bf-sde-9.5.0 in ``~``.
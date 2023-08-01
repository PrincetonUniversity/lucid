#!/bin/bash
# run this every time you log into the docker image.
add_hugepage() {
    sudo sysctl -w vm.nr_hugepages=128
    # sudo sh -c 'echo "#Enable huge pages support for DMA purposes" >> /etc/sysctl.conf'
    # echo "vm.nr_hugepages = 128" >> /etc/sysctl.conf'
}

dma_setup() {
    echo "Setting up DMA Memory Pool"
    hp=$(sudo sysctl -n vm.nr_hugepages)

    if [ $hp -lt 128 ]; then
        if [ $hp -eq 0 ]; then
            add_hugepage
        else
            nl=$(egrep -c vm.nr_hugepages /etc/sysctl.conf)
            if [ $nl -eq 0 ]; then
                add_hugepage
            else
                sudo sed -i 's/vm.nr_hugepages.*/vm.nr_hugepages = 128/' /etc/sysctl.conf
            fi
        fi
        sudo sysctl -p /etc/sysctl.conf
    fi

    if [ ! -d /mnt/huge ]; then
        sudo mkdir /mnt/huge
    fi
    sudo mount -t hugetlbfs nodev /mnt/huge
}

dma_setup

hp=$(sudo sysctl -n vm.nr_hugepages)
if [ $hp -lt 128 ]; then
    echo "ERROR SETTING UP HUGEPAGES. make sure to run docker container with --privileged"
    exit 1;
fi

bash


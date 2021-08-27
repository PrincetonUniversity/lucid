echo "building appliance: lucidvm.box"
vagrant up --provider=virtualbox
vagrant reload
echo "clearing unused space for smaller image"
vagrant ssh -c "sudo dd if=/dev/zero of=/EMPTY bs=1M; sudo rm -f /EMPTY"
echo "packaging into lucidvm.box"
vagrant package --output lucidvm.box
echo "destroying default box"
vagrant destroy
echo "done! lucid vm built in: lucidvm.box"
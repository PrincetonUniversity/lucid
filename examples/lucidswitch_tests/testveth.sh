### create the fake interfaces on macos
# Create two fake ethernet interfaces
sudo ifconfig feth0 create
sudo ifconfig feth1 create

# Peer them together (connect them)
sudo ifconfig feth0 peer feth1

# Bring them up
sudo ifconfig feth0 up
sudo ifconfig feth1 up

# teardown
# sudo ifconfig feth0 destroy
# sudo ifconfig feth1 destroy
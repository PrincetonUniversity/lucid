# 1. run the simulator
# make sim &
# sleep 10
# 2. run reflector
sudo ./reflector -i veth297 &
# 3. generate a packet using cmd.py
sudo ./cmd.py send 148 6
# 4. query the counters using cmd.py
sudo ./cmd.py query

# 1. run the simulator
# make sim &
# sleep 10
# 2. run reflector
sudo ./reflector -i veth297 &
# 3. initialize a flow or two
sudo ./cmd.py init 148 6
sudo ./cmd.py init 148 6
# 4. start sending
sudo ./cmd.py send 148
# 4. query the counters using cmd.py
sudo ./cmd.py query

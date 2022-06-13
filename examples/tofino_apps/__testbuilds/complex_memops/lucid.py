
import sys, os, time
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/libs")
from mgr import *
m = Manager()
if ((len(sys.argv) > 1) and (sys.argv[1] == 'ports_up')):
    # bring up all the 10 gig ports.
    ports_10g = [128, 129, 130, 131]
    for dpid in ports_10g:
        m.port_up(dpid, pal_port_speed_t.BF_SPEED_10G, pal_fec_type_t.BF_FEC_TYP_NONE)
    # bring up all the 40 gig ports.
    ports_40g = [136]
    for dpid in ports_40g:
        m.port_up(dpid, pal_port_speed_t.BF_SPEED_40G, pal_fec_type_t.BF_FEC_TYP_NONE)
m.add_multinode_mc_group(1066, [(196, 1)])
m.add_multinode_mc_group(1067, [(196, 1), (196, 2)])
m.add_multinode_mc_group(1068, [(196, 1), (196, 2), (196, 3)])
m.add_multinode_mc_group(1069, [(196, 1), (196, 2), (196, 3), (196, 4)])

m.disconnect()


import sys, os, time
sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/libs")
from mgr import *
m = Manager()
mgr.add_multinode_mc_group(1066, [(196, 1)])

m.disconnect()

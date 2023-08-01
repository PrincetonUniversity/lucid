#!/usr/bin/env python2
# minimal P4 16 control program
# All this script does is:  
# 1) set up the DPT mirroring session to the DPT recirc 
# port. 
# 2) set the DPT recirc port to be in recirc mode
import time, sys, argparse, os, signal, subprocess
from os.path import dirname
import logging

# import control manager.
from baseControl.grpcManager import *

DPT_RECIRC_PORT = 196
DPT_MC_GRP   = 1066

parser = argparse.ArgumentParser(description='Parse input arguments')
parser.add_argument('-i', '--prog', dest='prog', type=str, action='store', help='P4 program name.')
parser.add_argument('-s', '--sde', dest='sdeDir', type=str, action='store', help='Barefoot SDE directory.', default=os.environ['SDE'])
parser.add_argument('-hw', '--hardware', dest='hardware', action='store_true', help='Run on physical switch.')
parser.add_argument('--nodelay', dest='noDelay', action='store_true', default=False, help='skip delay PFC thread.')
usageString ="""\nUsage: ./control.py -i <p4 program name> -s <$SDE> [ -hw ] """

def main():
    args = parser.parse_args()
    global noDelay
    noDelay = args.noDelay
    print ("NODELAY: ")
    print (noDelay)
    print ("using $SDE in {0}".format(args.sdeDir))
    if (None in [args.sdeDir]):
        print ("error: $SDE not set. Please set the $SDE environment variable, or pass in the SDE directory with -s")
        return
    if (None in [args.prog]):
        print(usageString)
        return
    # wait for bf_switchd to start up
    cmd = "python {0}/install/lib/python2.7/site-packages/p4testutils/bf_switchd_dev_status.py".format(args.sdeDir)
    os.system(cmd)
    useSimulator = not args.hardware

    # initialize the connections to bf_switchd
    global cm
    cm = ControlManager(args.sdeDir, args.prog, useSimulator)
    cm.setup()
    print ("grpc startup complete")


    if useSimulator:
        print ("setting up multicast to recirc group")
        setup_dpt_mc_recirc(DPT_RECIRC_PORT)
        return
    else:
        # bring up one port.
        print ("bringing up port.")
        cm.fixedMgr.port_up("1/0", "10G", "NONE")
        # enable recirc port? (not needed for 196)
        # cm.fixedMgr.port_up("8/0", "100G", "NONE")
        # cm.fixedMgr.port_loopback("8/0", 1)
        time.sleep(1)
        # simple DPT setup for 1 generate per packet.
        print ("setting up multicast to recirc group")
        setup_dpt_mc_recirc(DPT_RECIRC_PORT)
    # pollX(cm)

def pollX(cm):
    print ("polling open ct")
    while (1):        
        openCt = cm.readRegister("pipe.Ingress.X", 0)
        print ("X[0]: %s"%(openCt))
        time.sleep(1)


def setup_dpt_mc_recirc(recirc_port): 
    cm.fixedMgr.add_mc_group(DPT_MC_GRP, [recirc_port])
    time.sleep(1)


# bring up some other port as a recirc port



def signal_handler(signal, frame):
    print('Exiting.')
    cleanupAndExit()

def cleanupAndExit():
    cm.teardown()
    # remove mirror sessions. 
    cm.fixedMgr.remove_mirror_sessions()
    # remove multicast groups.
    cm.fixedMgr.remove_mc_groups()
    cm.fixedMgr.end()
    sys.exit(0)
signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGHUP, signal_handler)



if __name__ == '__main__':
    main()
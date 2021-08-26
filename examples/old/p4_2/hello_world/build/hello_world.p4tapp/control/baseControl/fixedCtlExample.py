#!/usr/bin/env python
"""
The main control application.
"""
import time, select, sys, os, re, binascii, signal, argparse

# arguments.
parser = argparse.ArgumentParser(description='Parse input arguments')
parser.add_argument('-hw', '--hardware', dest='SIMULATION', action='store_false', help='Run in hardware', default=True)
args = parser.parse_args()

mgr = None


def updatePath():
    sdeDir = "/home/jsonch/bf_sde/bf-sde-9.1.0"
    bfPyPaths = [
    "{0}/install/lib/python2.7/site-packages/p4testutils".format(sdeDir),
    "{0}/install/lib/python2.7/site-packages/tofino".format(sdeDir),
    "{0}/install/lib/python2.7/site-packages".format(sdeDir)
    ]
    for pyPath in bfPyPaths:
        sys.path.append(pyPath.format(sdeDir))


def main():
    updatePath()
    from fixedManager import FixedManager
    # manager configuration flags. 
    if (not args.SIMULATION):
        portMapFile = "portmaps/portMap.json"
    else:
        portMapFile = "portmaps/simPortMap.json"

    global mgr
    # initialize control manager, only for fixed functions...
    mgr = FixedManager(portMapFile, args.SIMULATION)
    mgr.start()
    mgr.port_up('veth3', '10G', 'NONE')
    mgr.port_loopback('veth3', 1)
    time.sleep(10)
    mgr.port_loopback('veth3', 0)
    print('Press Ctrl+C or hang up to exit.')
    signal.pause()

# exit handler
def signal_handler(signal, frame):
    print('Exiting.')
    cleanupAndExit()

def cleanupAndExit():
    # remove mirror sessions. 
    mgr.remove_mirror_sessions()

    # remove multicast groups.
    mgr.remove_mc_groups()
    mgr.end()
    sys.exit(0)
signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGHUP, signal_handler)


if __name__ == '__main__':
    main()

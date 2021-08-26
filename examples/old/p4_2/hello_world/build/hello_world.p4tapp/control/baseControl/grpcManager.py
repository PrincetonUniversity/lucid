# Basic python controller to interface with the dataplane 
# through gRPC (to add rules, set up mirroring sessions, 
# and in general control dataplane elements defined by 
# p4 code)
# used by: control.py
import time, sys, argparse
from os.path import dirname
import logging
# import bf utils
def doBfImports(sdeDir):
    bfPyRoot = "{0}/install/lib/python2.7/".format(sdeDir)
    print ("using BF python2 libs from: %s"%bfPyRoot)
    bfPyPaths = [
    "{0}/install/lib/python2.7/site-packages/p4testutils",
    "{0}/install/lib/python2.7/site-packages/tofino",
    "{0}/install/lib/python2.7/site-packages"]
    for pyPath in bfPyPaths:
        sys.path.append(pyPath.format(sdeDir))
    global gc
    global BfRuntimeTest
    from bfruntime_client_base_tests import BfRuntimeTest
    import bfrt_grpc.client as gc
    # disable logging from gc
    gc.logger.setLevel(logging.CRITICAL)

    # import our fixed function manager.
    global FixedManager
    from fixedManager import FixedManager



class ControlManager(object):
    def __init__(self, sdeDir, p4Name, useSimulator=True):
        doBfImports(sdeDir)
        self.p4Name = p4Name
        self.sids = [] # mirroring sessions.
        curDir = dirname(os.path.abspath(__file__))
        if (useSimulator):
            self.fixedMgr = FixedManager("{0}/portmaps/simPortMap.json".format(curDir), useSimulator)
        else:
            self.fixedMgr = FixedManager("{0}/portmaps/portMap.json".format(curDir), useSimulator)
        return
    # ========================================
    # =           Setup / teardown           =
    # ========================================    
    def setup(self, client_id=0):
        print ("starting ControlManager for p4 program {0}".format(self.p4Name))
        grpc_addr = 'localhost:50052'
        # set up the interface that maintains the connection to the driver.
        print ("setting up gRPC client interface...")
        self.interface = gc.ClientInterface(grpc_addr, 
            client_id = client_id, device_id=0, 
            is_master=False,notifications=None)

        # bind to the P4 program.
        print ("binding to the P4 program ({0})...".format(self.p4Name))
        self.interface.bind_pipeline_config(self.p4Name)
        # start the fixed manager.
        self.fixedMgr.start()

    def teardown(self):
        for sid in self.sids:
            print ("deleting mirroring session {0}".format(sid))
            self.deleteMirrorSes(sid)
        self.interface._tear_down_stream()

    # ===============================================
    # =           Add a mirroring session           =
    # ===============================================    
    def addMirrorSes(self, sid, port):
        target = gc.Target(device_id=0, pipe_id=0xffff)
        direction = "INGRESS"
        bfrt_info = self.interface.bfrt_info_get(self.p4Name)
        mirror_cfg_table = bfrt_info.table_get("$mirror.cfg")
        max_pkt_len = 128 # only used for truncating sessions. Not here.

        mirror_cfg_table.entry_add(
            target,
            [mirror_cfg_table.make_key([gc.KeyTuple('$sid', sid)])],
            [mirror_cfg_table.make_data([gc.DataTuple('$direction', str_val=direction),
                                         gc.DataTuple('$ucast_egress_port', port),
                                         gc.DataTuple('$ucast_egress_port_valid', bool_val=True),
                                         gc.DataTuple('$session_enable', bool_val=True)],
                                        '$normal')]
        )
        print ("added mirroring session {0} for port {1}".format(sid, port))
        self.sids.append(sid)
    def deleteMirrorSes(self, sid):
        target = gc.Target(device_id=0, pipe_id=0xffff)
        bfrt_info = self.interface.bfrt_info_get(self.p4Name)
        mirror_cfg_table = bfrt_info.table_get("$mirror.cfg")
        mirror_cfg_table.entry_del(
            target,
            [mirror_cfg_table.make_key([gc.KeyTuple('$sid', sid)])])

    # =============================================
    # =           Table manipulation              =
    # =============================================
    
    
    
    # ======  End of Table manipulation     =======
    def addExactEntry(self, tableName, fieldNames, fieldVals, actionName, actionArgs={}):
        print ("adding exact entry: {0}[{1}=={2}] --> {3}({4})".format(tableName, str(fieldNames), str(fieldVals), actionName, actionArgs))
        bfrt_info = self.interface.bfrt_info_get(self.p4Name)
        target = gc.Target(device_id=0, pipe_id=0xffff)
        table = bfrt_info.table_get(tableName)        
        key_list = [table.make_key([gc.KeyTuple(fieldName, fieldVal)]) for (fieldName, fieldVal) in zip(fieldNames, fieldVals)]
        data_list = []
        for argName, argVal in actionArgs.items():
            data_list.append(table.make_data([gc.DataTuple(argName, argVal)], actionName))
        table.entry_add(target,key_list, data_list)


        # target = gc.Target(device_id=0, pipe_id=0xffff)
        # resp = self.register_table.entry_get(
        #     target,
        #     [self.register_table.make_key(
        #         [gc.KeyTuple('$REGISTER_INDEX', idx)])],
        #     {"from_hw": True})


    # =============================================
    # =           Register manipulation           =
    # =============================================
    # register names can be found in bf-rt.json
    # they are typically fully qualified, e.g.:
    # pipe.Ingress.dptManager.handlePacketIn.countReg
    def readRegister(self, registerName, idx):
        # get a reference to the register
        self.bfrt_info = self.interface.bfrt_info_get(self.p4Name)
        self.register_table = self.bfrt_info.table_get(registerName)

        target = gc.Target(device_id=0, pipe_id=0xffff)
        resp = self.register_table.entry_get(
            target,
            [self.register_table.make_key(
                [gc.KeyTuple('$REGISTER_INDEX', idx)])],
            {"from_hw": True})

        # parse the value in the response.
        data, key = resp.next()
        dataObj = data.field_dict.values()[0]
        field = dataObj.name
        val = data._get_val(dataObj)

        # parse the index in the response.
        keyObj = key.field_dict.values()[0]
        idx = key._get_val(keyObj)["value"]
        idxField = keyObj.name

        # print ("{0}[{1}] : {2}".format(field, idx, val))
        return val


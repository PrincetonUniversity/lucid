# Routines to control fixed function parts of the tofino 
# that are not accessible through grpc. 
# used by: grpcManager.py
import time, sys, os, re, importlib, binascii, json

# these modules are in: 
# bfPyPaths = [
# "{0}/install/lib/python2.7/site-packages/p4testutils".format(sdeDir),
# "{0}/install/lib/python2.7/site-packages/tofino".format(sdeDir),
# "{0}/install/lib/python2.7/site-packages".format(sdeDir)
# ]

from thrift.transport import TSocket, TTransport
from thrift.protocol import TBinaryProtocol, TMultiplexedProtocol
from res_pd_rpc.ttypes import * # DevTarget_t
from ptf.thriftutils import * # hex_to_i16
from mirror_pd_rpc.ttypes import *
from devport_mgr_pd_rpc.ttypes import *
from pal_rpc.ttypes import * # pal_port_speed_t, pal_fec_type_t
from tm_api_rpc.ttypes import tm_pfc_cos_map_t 


class FixedManager(object):
    rateMap = {"10G":pal_port_speed_t.BF_SPEED_10G, 
               "40G":pal_port_speed_t.BF_SPEED_40G, 
               "25G":pal_port_speed_t.BF_SPEED_25G,
               "100G":pal_port_speed_t.BF_SPEED_100G}
    fecMap = {"NONE" :pal_fec_type_t.BF_FEC_TYP_NONE,
        "FC" : pal_fec_type_t.BF_FEC_TYP_FIRECODE,
        "RS" : pal_fec_type_t.BF_FEC_TYP_REED_SOLOMON}
    def __init__(self, port_map_file, simulatorMode):
        self.controlModules = []
        self.mirrorSessions = []
        self.mcGroups = []
        self.mc_client_module = importlib.import_module(".".join(["mc_pd_rpc", "mc"]))
        try: 
            self.tm_client_module = importlib.import_module(".".join(["tm_api_rpc", "tm"]))
            print ("LOADED TM CLIENT MODULE")
        except:
            print ("COULD NOT LOAD TM CLIENT MODULE")
        try:
            self.mirror_client_module = importlib.import_module(".".join(["mirror_pd_rpc", "mirror"]))
        except:
            self.mirror_client_module = None
        try:
            self.sd_client_module = importlib.import_module(".".join(["sd_pd_rpc", "sd"]))
        except:
            self.sd_client_module = None
        try:
            self.plcmt_client_module = importlib.import_module(".".join(["plcmt_pd_rpc", "plcmt"]))
        except:
            self.plcmt_client_module = None
        try:
            self.devport_mgr_client_module = importlib.import_module(".".join(["devport_mgr_pd_rpc", "devport_mgr"]))
        except:
            self.devport_mgr_client_module = None
        try:
            self.port_mgr_client_module = importlib.import_module(".".join(["port_mgr_pd_rpc", "port_mgr"]))
        except:
            self.port_mgr_client_module = None
        try:
            self.pal_client_module = importlib.import_module(".".join(["pal_rpc", "pal"]))
        except:
            self.pal_client_module = None
        self.conn_mgr_client_module = importlib.import_module(".".join(["conn_mgr_pd_rpc", "conn_mgr"]))
        try:
            self.pkt_client_module = importlib.import_module(".".join(["pkt_pd_rpc", "pkt"]))
        except:
            self.pkt_client_module = None
        try:
            self.pltfm_pm_client_module = importlib.import_module(".".join(["pltfm_pm_rpc", "pltfm_pm_rpc"]))
        except:
            self.pltfm_pm_client_module = None
        try:
            self.pltfm_mgr_client_module = importlib.import_module(".".join(["pltfm_mgr_rpc", "pltfm_mgr_rpc"]))
        except:
            self.pltfm_mgr_client_module = None
        try:
            self.diag_client_module = importlib.import_module(".".join(["diag_rpc", "diag_rpc"]))
        except:
            self.diag_client_module = None
        try:
            self.knet_mgr_client_module = importlib.import_module(".".join(["knet_mgr_pd_rpc", "knet_mgr"]))
        except:
            self.knet_mgr_client_module = None

        self.simulatorMode = simulatorMode
        if (simulatorMode):
            # if simulator, get mapping right from config file.
            port_map_config = json.load(open(port_map_file, "r"))
            self.port_map = {"veth%s"%v['veth2']:v['device_port'] for v in port_map_config["PortToVeth"]}
        else:
            self.port_map = json.load(open(port_map_file, "r"))
            self.port_map["CPU"] = 192 # add the CPU port. Hard coded.
        self.activeDpids = []



    def start(self):
        # Set up thrift client and contact server
        thrift_server = 'localhost'
        self.transport = TSocket.TSocket(thrift_server, 9090)

        self.transport = TTransport.TBufferedTransport(self.transport)
        bprotocol = TBinaryProtocol.TBinaryProtocol(self.transport)

        # And the diag server as well
        self.transport_diag = None
        if self.diag_client_module:
            thrift_server = 'localhost'
            self.transport_diag = TSocket.TSocket(thrift_server, 9096)
            self.transport_diag = TTransport.TBufferedTransport(self.transport_diag)
            #bprotocol_diag = TBinaryProtocol.TBinaryProtocol(self.transport_diag)

        if self.tm_client_module:
            self.tm_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "tm")
            print ("INITIALIZED TM CLIENT")            
        self.mc_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "mc")
        if self.mirror_client_module:
            self.mirror_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "mirror")
        if self.sd_client_module:
            self.sd_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "sd")
        if self.plcmt_client_module:
            self.plcmt_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "plcmt")
        if self.devport_mgr_client_module:
            self.devport_mgr_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "devport_mgr")
        else:
            self.devport_mgr_protocol = None
        if self.port_mgr_client_module:
            self.port_mgr_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "port_mgr")
        else:
            self.port_mgr_protocol = None
        if self.pal_client_module:
            self.pal_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "pal")
        else:
            self.pal_protocol = None
        self.conn_mgr_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "conn_mgr")
        if self.pkt_client_module:
            self.pkt_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "pkt")
        else:
            self.pkt_protocol = None

        if self.pltfm_pm_client_module:
            self.pltfm_pm_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "pltfm_pm_rpc")
        else:
            self.pltfm_pm_protocol = None
        if self.pltfm_mgr_client_module:
            self.pltfm_mgr_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "pltfm_mgr_rpc")
        else:
            self.pltfm_mgr_protocol = None
        if self.diag_client_module and self.transport_diag:
            self.diag_protocol = TBinaryProtocol.TBinaryProtocol(self.transport_diag)
        else:
            self.diag_protocol = None
        if self.knet_mgr_client_module:
            self.knet_mgr_protocol = TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "knet_mgr")
        else:
            self.knet_mgr_protocol = None

        self.mc = self.mc_client_module.Client(self.mc_protocol)
        if (self.tm_client_module):
            print ("CONNECTED TO TM")
            self.tm = self.tm_client_module.Client(self.tm_protocol)
        if self.mirror_client_module:
            self.mirror = self.mirror_client_module.Client(self.mirror_protocol)
        else:
            self.mirror = None
        if self.sd_client_module:
            self.sd = self.sd_client_module.Client(self.sd_protocol)
        else:
            self.sd = None
        if self.plcmt_client_module:
            self.plcmt = self.plcmt_client_module.Client(self.plcmt_protocol)
        else:
            self.plcmt = None
        if self.devport_mgr_client_module:
            self.devport_mgr = self.devport_mgr_client_module.Client(self.devport_mgr_protocol)
        else:
            self.devport_mgr = None
        if self.port_mgr_client_module:
            self.port_mgr = self.port_mgr_client_module.Client(self.port_mgr_protocol)
        else:
            self.port_mgr = None
        if self.pal_client_module:
            self.pal = self.pal_client_module.Client(self.pal_protocol)
        else:
            self.pal = None
        self.conn_mgr = self.conn_mgr_client_module.Client(self.conn_mgr_protocol)
        if self.pkt_client_module:
            self.pkt = self.pkt_client_module.Client(self.pkt_protocol)
        else:
            self.pkt = None

        if self.pltfm_pm_client_module:
            self.pltfm_pm = self.pltfm_pm_client_module.Client(self.pltfm_pm_protocol)
        else:
            self.pltfm_pm = None
        if self.pltfm_mgr_client_module:
            self.pltfm_mgr = self.pltfm_mgr_client_module.Client(self.pltfm_mgr_protocol)
        else:
            self.pltfm_mgr = None
        if self.diag_client_module and self.transport_diag:
            self.diag = self.diag_client_module.Client(self.diag_protocol)
        else:
            self.diag = None
            if self.knet_mgr_client_module:
                self.knet_mgr = self.knet_mgr_client_module.Client(self.knet_mgr_protocol)
            else:
                self.knet_mgr = None

        try: 
            self.transport.open()
        except: 
            print "Did not connect to thrift server"
            quit()

        if self.transport_diag:
                try:
                    self.transport_diag.open()
                except:
                    print "Did not connect to diag thrift server"
                    self.transport_diag = None
                    self.diag = None


        self.sess_hdl = self.conn_mgr.client_init()
        self.dev_tgt = DevTarget_t(0, hex_to_i16(0xFFFF))
        self.mc_sess_hdl = self.mc.mc_create_session()
        if (not self.simulatorMode):
            self.platform_type = "mavericks"
            board_type = self.pltfm_pm.pltfm_pm_board_type_get()
            if re.search("0x0234|0x1234|0x4234", hex(board_type)):
                self.platform_type = "mavericks"
            elif re.search("0x2234|0x3234", hex(board_type)):
                self.platform_type = "montara"
        else:
            self.platform_type = "SIMULATOR"
        print ("platform type: %s"%self.platform_type)

    def end(self):
        self.remove_mirror_sessions()
        self.ports_down()

        # close client connection.
        print ("closing connection to asic.")
        self.mc.mc_destroy_session(self.mc_sess_hdl)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.conn_mgr.client_cleanup(self.sess_hdl)        
        if self.transport_diag:
            self.transport_diag.close()
        self.transport.close()


    # put a port into loopback mode.
    # class pal_loopback_mod_t:
    #   BF_LPBK_NONE = 0
    #   BF_LPBK_MAC_NEAR = 1
    #   BF_LPBK_MAC_FAR = 2
    #   BF_LPBK_PCS_NEAR = 3
    #   BF_LPBK_SERDES_NEAR = 4
    #   BF_LPBK_SERDES_FAR = 5
    def port_loopback(self, fp_port, enable):
        dpid = self.port_map[fp_port]
        # turn on loopback at the MAC.
        if enable == 1:
            print ("putting port {0} [dpid: {1}] into LOOPBACK MODE".format(fp_port, dpid))
            self.pal.pal_port_loopback_mode_set(0, dpid, pal_loopback_mod_t.BF_LPBK_MAC_NEAR)
        # turn off loopback.
        else:
            print ("putting port {0} [dpid: {1}] into NON LOOPBACK MODE".format(fp_port, dpid))

            self.pal.pal_port_loopback_mode_set(0, dpid, pal_loopback_mod_t.BF_LPBK_NONE)

    def get_dpid(self, fp_port):
        return self.port_map[fp_port]
    
    # bring up ports based on name and rate.
    def port_up(self, fp_port, rate, fec):
        dpid = self.port_map[fp_port]
        print ("enabling port: %s (dev id: %s) at rate: %s"%(fp_port, dpid, rate))
        # try:            
        self.pal.pal_port_add(0, dpid,
                               self.rateMap[rate],
                               self.fecMap[fec])
        # turn autonegotiation OFF (0 = default, 1 = on, 2 = off)
        self.pal.pal_port_an_set(0, dpid, 2)
        self.pal.pal_port_enable(0, dpid)
        # except:
        #     print ("\tport not enabled (already up?)")

        self.activeDpids.append(dpid)
        return dpid

    # bring down ports.
    def ports_down(self):
        for i in self.activeDpids:
            print ("deleting port: %s"%i)
            if (not self.simulatorMode ):
                self.pal.pal_port_del(0, i)
        # self.pal.pltfm_pm_switchd_port_cleanup(0)

    def add_ingress_mirror_session(self, mirror_port, sid):
        # add each port to session. 
        port = mirror_port
        print ("adding port %s to mirror session %s"%(port, sid))
        info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_INGRESS, sid, port, True)
        self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.mirrorSessions.append(sid)

    def add_ingress_truncating_mirror_session(self, mirror_port, sid, pktLen):
        # add each port to session. 
        port = mirror_port
        print ("adding port %s to mirror session %s"%(port, sid))
        info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_INGRESS, sid, port, True, max_pkt_len = pktLen)
        self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.mirrorSessions.append(sid)

    def add_egress_mirror_session(self, mirror_port, sid):
        # add each port to session. 
        port = mirror_port
        print ("adding port %s to mirror session %s"%(port, sid))
        info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_EGRESS, sid, port, True)
        self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.mirrorSessions.append(sid)
        
    def add_egress_truncating_mirror_session(self, mirror_port, sid, pktLen):
        # add each port to session. 
        port = mirror_port
        print ("adding port %s to mirror session %s"%(port, sid))
        info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_EGRESS, sid, port, True, max_pkt_len = pktLen)
        self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.mirrorSessions.append(sid)


    def add_coal_mirror_session(self, port, sid):
        # mirror_session(mir_type, mir_dir, sid, egr_port=0, egr_port_v=False,
        #                    egr_port_queue=0, packet_color=0, mcast_grp_a=0,
        #                    mcast_grp_a_v=False, mcast_grp_b=0, mcast_grp_b_v=False,
        #                    max_pkt_len=0, level1_mcast_hash=0, level2_mcast_hash=0,
        #                    cos=0, c2c=0, extract_len=0, timeout=0, int_hdr=[])
        # add each port to session. 
        # PD_MIRROR_TYPE_COAL
        # port = mirror_port
        # port = 144
        # COAL_MIRROR_ID = 1014
        # sid = COAL_MIRROR_ID
        print ("adding port %s to coal mirror session %s"%(port, sid))
        # info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_EGRESS, sid, port, True)
        info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_COAL, Direction_e.PD_DIR_EGRESS, sid, port, True, timeout = 100, extract_len=24)#, timeout=100, extract_len = 24)
        self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)

        self.mirror.mirror_session_enable(self.sess_hdl, Direction_e.PD_DIR_EGRESS, self.dev_tgt, sid)
        # self.mirror.mirror_session_update(self.sess_hdl, self.dev_tgt, info, True)
        self.conn_mgr.complete_operations(self.sess_hdl)
        self.mirrorSessions.append(sid)

        # # now add repeated cloner.
        # RECIRC_MIRROR_ID = 1015
        # sid = RECIRC_MIRROR_ID
        # print ("adding port %s to mirror session %s"%(port, sid))
        # info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_EGRESS, RECIRC_MIRROR_ID, port, True)
        # self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)
        # self.conn_mgr.complete_operations(self.sess_hdl)
        # self.mirrorSessions.append(sid)

    def remove_mirror_sessions(self):
        for sid in self.mirrorSessions:
            self.mirror.mirror_session_delete(self.sess_hdl, self.dev_tgt, sid)
            self.conn_mgr.complete_operations(self.sess_hdl)


    def set_negative_mirror_dest(self, pipe, port, queue): 
        print ("set_negative_mirror_dest pipe = %s port (dpid)=%s queue=%s"%(pipe, port, queue))
        # thrift signature: 
        # void      tm_set_negative_mirror_dest(1:tm_dev_t dev, 2:tm_pipe_t pipe, 3:tm_port_t port, 4:tm_q_t q) throws         
        # typedef i32 tm_dev_t
        # typedef i32 tm_pipe_t
        # typedef i32 tm_port_t
        # typedef i32 tm_q_t
        print ("dev = %i; pipe = %i; port = %i; q = %i;"%(self.dev_tgt.dev_id, pipe, port, queue))
        self.tm.tm_set_negative_mirror_dest(self.dev_tgt.dev_id, pipe, port, queue)
        return 

    # ===============================================
    # =           Queues and Pause Frames           =
    # ===============================================
    def set_queue_rate_pps(self, port, q, burstsize, rate):
        # set rate and burst parameters for queue q
        print ("setting queue rate:\ndev = %i; port = %i; q = %i; burstsize = %i; rate = %i;"%(self.dev_tgt.dev_id, 
            port, q, burstsize, rate))

        self.tm.tm_set_q_shaping_rate(self.dev_tgt.dev_id, 
            port, q, True, burstsize, rate) # true parameter indicates that rate is in terms of packets per second (false would mean bits per second)
        # enable the shaping
        self.tm.tm_enable_q_max_shaping_rate(self.dev_tgt.dev_id, port, q)

    def disable_queue_rate(self, port, q):
        self.tm.tm_disable_q_max_shaping_rate(self.dev_tgt.dev_id, port, q)


    def enable_pfc_queues(self, p):
        # set up queues and CoS mapping for pause frame rx on port p

        print ("enable_pfc_queues for port=%i"%(p))
        # 1) map port queue to a PFC CoS 
        # param names are "CoSx_to_iCos", but this should be setting a queue number for each cos. 
        # queue_cos_map = tm_pfc_cos_map_t(0, 1, 2, 3, 4, 5, 6, 7)
        # print ("actually, not doing anything.. python is missing all the apis...")
        # return
        # wrong method! need to use bf_tm_q_pfc_cos_mapping_set
        # self.tm.tm_set_port_pfc_cos_mapping(self.dev_tgt.dev_id,
        #     port=p, cos_icos_map=queue_cos_map)

        # # 2) enable PFC flowcontrol in traffic manager. 
        # # tm_set_port_flowcontrol_mode is for GENERATION of PFCs
        # self.tm.tm_set_port_flowcontrol_mode(self.dev_tgt.dev_id, 
        #     port=p, fctype=1) # 1 is BF_TM_PAUSE_PFC (2 is for port-pause, 0 is for none)
        # bf_tm_port_flowcontrol_rx is for RECEPTION of PFCs.
        # but we need to configure that in C... because there's no pd_fixed / thrift / python API...

        

        # # 3) enable PFC rx for queues on the port?
        # all_queues_bitmap = 15 # 1111 (should be all queues?) 
        # self.pal.pal_port_flow_control_pfc_set(0, p, all_queues_bitmap, all_queues_bitmap)
        
        # def pal_port_flow_control_pfc_set(self, device, dev_port, tx_en_map, rx_en_map):


    
    
    # ======  End of Queues and Pause Frames  =======
    



    def add_mc_group(self, mc_id, mc_ports):
        """
        create a multicast flood group for a set of ports.
        """
        lag_map = set_port_or_lag_bitmap(256, [])
        # program flood mcidx
        flood_ports = []
        flood_ports = [int(p) for p in mc_ports]
        print ("mc id: %s ports: %s"%(mc_id, str(mc_ports)))
        port_map = set_port_or_lag_bitmap(288, list(flood_ports))
        print port_map
        mc_grp_hdl = self.mc.mc_mgrp_create(self.mc_sess_hdl, self.dev_tgt.dev_id, mc_id)
        mc_node_hdl = self.mc.mc_node_create(self.mc_sess_hdl, self.dev_tgt.dev_id, 0, port_map, lag_map)
        self.mc.mc_associate_node(self.mc_sess_hdl, self.dev_tgt.dev_id, mc_grp_hdl, mc_node_hdl, 0, 0)

        self.mcGroups.append((mc_node_hdl, mc_grp_hdl))
        # self.mc.mc_mgrp_destroy(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i32(mc_grp_hdl))
        return (mc_node_hdl, mc_grp_hdl)

        # brid = mc_id
        # self.mc.mc_mgrp_create(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i16(brid))
        # l1_hdl = self.mc.mc_node_create(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i16((~brid)&0xFFFF), port_map, lag_map)
        # for port in mc_ports:
        #     print ("adding port %s to mirror %s"%(port, sid))
        #     info = mirror_session(MirrorType_e.PD_MIRROR_TYPE_NORM, Direction_e.PD_DIR_EGRESS, sid, port, True)
        #     self.mirror.mirror_session_create(self.sess_hdl, self.dev_tgt, info)

    def remove_mc_groups(self):
        for (mc_node_hdl, mc_grp_hdl) in self.mcGroups:
            # dissassociate.
            self.mc.mc_dissociate_node(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i32(mc_grp_hdl), hex_to_i32(mc_node_hdl))
            # destroy group.
            self.mc.mc_mgrp_destroy(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i32(mc_grp_hdl))
            # delete node.
            self.mc.mc_node_destroy(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i32(mc_node_hdl))

    def setPortMap(self):
        """
        Add a rule that sets the port id field for all ports.
        """
        resubmitDpids = [64, 68, 192, 196]
        for dpid in self.port_map.values():
            if dpid in resubmitDpids:
                continue
            print ("mapping port %s"%dpid)
            matchspec = switch_tiPortToIndex_match_spec_t(ig_intr_md_ingress_port=dpid)
            actnspec = switch_aiPortToIndex_action_spec_t(dpid)
            result = self.client.tiPortToIndex_table_add_with_aiPortToIndex(self.sess_hdl,self.dev_tgt,matchspec,actnspec)

    def clearPortMap(self):
        """
        clear port map. 
        """
        self.cleanup_table("tiPortToIndex")

    def enableRecircPort(self, port_id):
        try:
            self.devport_mgr.devport_mgr_remove_port(self.dev_tgt.dev_id, port_id)
        except InvalidDevportMgrOperation as e:
            print ("fail for port: %s"%port)
            pass
        self.devport_mgr.devport_mgr_add_port(self.dev_tgt.dev_id, port_id, self.rateMap["100G"], 0)
        self.conn_mgr.recirculation_enable(self.sess_hdl, self.dev_tgt.dev_id, port_id)

    def enableRecirculationOnCpuPort(self):
        # pipeline engine 1.
        # remove all recirc ports from devport manager. (192 and 196)
        for port in range(192, 193):
            try:
                self.devport_mgr.devport_mgr_remove_port(self.dev_tgt.dev_id, port)
            except InvalidDevportMgrOperation as e:
                print ("fail for port: %s"%port)
                pass
        # change port 192 from CPU port --> recirc port
        # self.devport_mgr.devport_mgr_add_port(self.dev_tgt.dev_id, 192, self.rateMap["100G"], 0)
        self.conn_mgr.recirculation_enable(self.sess_hdl, self.dev_tgt.dev_id, 192)
        return
        # pipeline engine 2.
        # remove all recirc ports from devport manager. (64 and 68)
        for port in range(64, 72):
            try:
                self.devport_mgr.devport_mgr_remove_port(self.dev_tgt.dev_id, port)
            except InvalidDevportMgrOperation as e:
                print ("fail for port: %s"%port)
                pass
        # change port 64 from CPU port --> recirc port
        self.devport_mgr.devport_mgr_add_port(self.dev_tgt.dev_id, 64, 64, 0)
        self.conn_mgr.recirculation_enable(self.sess_hdl, self.dev_tgt.dev_id, 64)

    # ==============================
    # =           Meters           =
    # ==============================
    def setByteMeterConfig(self, meterName, meterIdx, cIr, cBurst, pIr, pBurst):
        """ Configure one slot in a meter """
        # cIr       : committed information rate (Kb/s)
        # cBurst    : committed burst rate (Kb)
        # pIr       : peak information rate (Kb/s)
        # pBurst    : peak burst rate (Kb)
        # print dir(self.client)
        print ("setting byte meter %s[%s] to (committed rate: %s [%s] peak rate: %s [%s])"%(meterName, meterIdx, cIr, cBurst, pIr, pBurst))
        meterSpec = switch_bytes_meter_spec_t(cIr, cBurst, pIr, pBurst, False) # last variable is color aware or not.
        # self.client.meter_set_meter1(self.sess_hdl, self.dev_tgt, meterIdx, meterSpec)
        eval("self.client.meter_set_%s(self.sess_hdl, self.dev_tgt, meterIdx, meterSpec)"%meterName)
        self.conn_mgr.complete_operations(self.sess_hdl)
            # eval(table + '_table_delete')(self.sess_hdl, self.dev_tgt.dev_id, hdl)

    def setPacketMeterConfig(self, meterName, meterIdx, cIr, cBurst, pIr, pBurst):
        """ Configure one slot in a meter """
        # cIr       : committed information rate (Pkts/s)
        # cBurst    : committed burst rate (Pkts)
        # pIr       : peak information rate (Pkts/s)
        # pBurst    : peak burst rate (Pkts)
        # print dir(self.client)
        print ("setting packet meter %s[%s] to (committed rate: %s [%s] peak rate: %s [%s])"%(meterName, meterIdx, cIr, cBurst, pIr, pBurst))
        meterSpec = switch_bytes_meter_spec_t(cIr, cBurst, pIr, pBurst, False) # last variable is color aware or not.
        # self.client.meter_set_meter1(self.sess_hdl, self.dev_tgt, meterIdx, meterSpec)
        eval("self.client.meter_set_%s(self.sess_hdl, self.dev_tgt, meterIdx, meterSpec)"%meterName)
        self.conn_mgr.complete_operations(self.sess_hdl)
            # eval(table + '_table_delete')(self.sess_hdl, self.dev_tgt.dev_id, hdl)

    
    
    # ======  End of Meters  =======
    


def port_to_pipe(port):
    return port >> 7
def port_to_pipe_local_id(port):
    return port & 0x7F
def port_to_bit_idx(port):
    pipe = port_to_pipe(port)
    index = port_to_pipe_local_id(port)
    return 72 * pipe + index

def set_port_or_lag_bitmap(bit_map_size, indicies):
    bit_map = [0] * ((bit_map_size+7)/8)
    for i in indicies:
        index = port_to_bit_idx(i)
        bit_map[index/8] = (bit_map[index/8] | (1 << (index%8))) & 0xFF
    return bytes_to_string(bit_map)

def mirror_session(mir_type, mir_dir, sid, egr_port=0, egr_port_v=False,
                   egr_port_queue=0, packet_color=0, mcast_grp_a=0,
                   mcast_grp_a_v=False, mcast_grp_b=0, mcast_grp_b_v=False,
                   max_pkt_len=0, level1_mcast_hash=0, level2_mcast_hash=0,
                   cos=0, c2c=0, extract_len=0, timeout=0, int_hdr=[]):
  return MirrorSessionInfo_t(mir_type,
                             mir_dir,
                             sid,
                             egr_port,
                             egr_port_v,
                             egr_port_queue,
                             packet_color,
                             mcast_grp_a,
                             mcast_grp_a_v,
                             mcast_grp_b,
                             mcast_grp_b_v,
                             max_pkt_len,
                             level1_mcast_hash,
                             level2_mcast_hash,
                             cos,
                             c2c,
                             extract_len,
                             timeout,
                             int_hdr,
                             len(int_hdr))
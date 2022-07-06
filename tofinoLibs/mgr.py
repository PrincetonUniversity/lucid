# ===================================================================
# =           Helper functions for simple python control.           =
# ===================================================================

import time, sys, os, re, importlib, binascii, json, logging


# paths to tofino python libs
pylib_paths = [
"/lib/python3.6/site-packages/tofino", 
"/lib/python3.6/site-packages/tofino/bfrt_grpc", 
"/lib/python3.6/site-packages", 
"/lib/python3.6/site-packages/p4testutils", 
"/lib/python3.6/site-packages/bf-ptf"]
pylib_paths = [os.getenv('SDE_INSTALL')+p for p in pylib_paths]
for p in pylib_paths:
  print ("adding path: %s"%str(p))
  sys.path.append(p)

from thrift.transport import TSocket, TTransport
from thrift.protocol import TBinaryProtocol, TMultiplexedProtocol
from res_pd_rpc.ttypes import * # DevTarget_t
from ptf.thriftutils import * # hex_to_i16
from mirror_pd_rpc.ttypes import *
from devport_mgr_pd_rpc.ttypes import *
from pal_rpc.ttypes import * # pal_port_speed_t, pal_fec_type_t

import pal_rpc.pal as pal_i
import conn_mgr_pd_rpc.conn_mgr as conn_mgr_client_module
import mc_pd_rpc.mc as mc_client_module

from bfruntime_client_base_tests import BfRuntimeTest
import bfrt_grpc.client as gc
# disable logging from gc
gc.logger.setLevel(logging.CRITICAL)

class Manager(object):

  def __init__(self, p4Name=None):
    self.p4Name = p4Name
    self.connect()

  def connect(self):
    self.grpc_connect()
    self.fixed_function_connect()

  def grpc_connect(self):
    if (self.p4Name != None):
      grpc_addr = 'localhost:50052' 
      client_id = 0
      print ("setting up gRPC client interface...")
      self.interface = gc.ClientInterface(grpc_addr, 
        client_id = client_id, device_id=0,
        notifications=None) 
      self.interface.bind_pipeline_config(self.p4Name)

  def fixed_function_connect(self):
    # master connection
    self.transport = TTransport.TBufferedTransport(TSocket.TSocket('localhost', 9090))
    self.transport.open()
    bprotocol = TBinaryProtocol.TBinaryProtocol(self.transport)

    # module connections
    self.pal = pal_i.Client(TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "pal"))
    self.conn_mgr = conn_mgr_client_module.Client(TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "conn_mgr"))
    self.mc = mc_client_module.Client(TMultiplexedProtocol.TMultiplexedProtocol(bprotocol, "mc"))

    # session and device handlers
    self.sess_hdl = self.conn_mgr.client_init()
    self.mc_sess_hdl = self.mc.mc_create_session()  
    self.dev_tgt = DevTarget_t(0, hex_to_i16(0xFFFF))

  def disconnect(self):
    # grpc 
    if(self.p4Name != None):
      self.interface.tear_down_stream()

    # fixed function
    self.mc.mc_destroy_session(self.mc_sess_hdl)
    self.conn_mgr.complete_operations(self.sess_hdl)
    self.conn_mgr.client_cleanup(self.sess_hdl)        
    self.transport.close()
    print ("mgr.py disconnect complete.")

  def port_up(self, dpid, rate, fec_type):  
    print ("bringing port %s up"%dpid)
    self.pal.pal_port_add(0, dpid, rate, fec_type)
    # disable and then enable auto-negotiation
    self.pal.pal_port_an_set(0, dpid, 2)
    self.pal.pal_port_enable(0, dpid)
    self.pal.pal_port_an_set(0, dpid, 1)

  def add_mc_group(self, mc_gid, dpids):
    lag_map = set_port_or_lag_bitmap(256, [])
    flood_ports = [int(p) for p in dpids]
    print("mc gid: %s; ports: %s"%(mc_gid, str(dpids)))
    port_map = set_port_or_lag_bitmap(288, list(flood_ports))
    mc_grp_hdl = self.mc.mc_mgrp_create(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i16(mc_gid))
    mc_node_hdl = self.mc.mc_node_create(self.mc_sess_hdl, self.dev_tgt.dev_id, 0, port_map, lag_map)
    self.mc.mc_associate_node(self.mc_sess_hdl, self.dev_tgt.dev_id, mc_grp_hdl, mc_node_hdl, 0, 0)
    # mc.mc_mgrp_destroy(mc_sess_hdl, dev_tgt.dev_id, hex_to_i32(mc_grp_hdl))
    self.conn_mgr.complete_operations(self.sess_hdl)
    self.mc.mc_complete_operations(self.mc_sess_hdl)
    return (mc_node_hdl, mc_grp_hdl)

  def add_multinode_mc_group(self, mc_gid, dpids_and_rids):
    # create the mc group
    mc_grp_hdl = self.mc.mc_mgrp_create(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i16(mc_gid))
    # add one node for each (dpid, rid) pair
    lag_map = set_port_or_lag_bitmap(256, [])
    mc_node_hdls = []
    for (dpid, rid) in dpids_and_rids:
      port_map = set_port_or_lag_bitmap(288, [dpid])
      mc_node_hdl = self.mc.mc_node_create(self.mc_sess_hdl, self.dev_tgt.dev_id, rid, port_map, lag_map)
      self.mc.mc_associate_node(self.mc_sess_hdl, self.dev_tgt.dev_id, mc_grp_hdl, mc_node_hdl, 0, 0)
      mc_node_hdls.append(mc_node_hdl)
    return mc_node_hdls, mc_grp_hdl

  def delete_mc_group(self, mc_grp_hdl):
    self.mc.mc_mgrp_destroy(self.mc_sess_hdl, self.dev_tgt.dev_id, hex_to_i32(mc_grp_hdl))

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


# helpers 
def port_to_pipe(port):
  return port >> 7
def port_to_pipe_local_id(port):
  return port & 0x7F
def port_to_bit_idx(port):
  pipe = port_to_pipe(port)
  index = port_to_pipe_local_id(port)
  return int(72 * pipe + index)
def set_port_or_lag_bitmap(bit_map_size, indicies):
  bit_map = [0] * ((bit_map_size+7)/8)
  for i in indicies:
    index = port_to_bit_idx(i)
    bit_map[index//8] = (bit_map[index//8] | (1 << (index%8))) & 0xFF
  return bytes_to_string(bit_map)
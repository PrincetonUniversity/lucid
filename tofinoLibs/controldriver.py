from ctypes import *
from pathlib import Path
import json

DEBUG=True

# example usage. note that if you wrap 
# this in a main function, dict(locals) must change to dict(globals)

## pass a script with these contents to bf_switchd with the -b flag, 
## or launch it with bfshell after bf_switchd has 
## started: $SDE_INSTALL/bin/bfshell -b /lucid/pydriver.py
# import os, sys
# sys.path.append(os.path.dirname(os.path.realpath(__file__)))
# from controldriver import *
# clib_driver_so = find_clib_driver_file(dict(locals()))
# c = Controller(clib_driver_so)
# # add a multicast node
# c.add_multicast_group(611, [(129, 0), (130, 0)])
# c.cleanup() # only cleanup if you want to delete rules
# c.close()

# updates: 
# 2/24/23: added support for ternary keys
# 2/24/23: changed all dictionary keys to bytes
# 2/24/23: support adding exact entries to ternary tables
# 3/20/23: all keys are strings, get entry method, array set and get functions in Controller
# 3/21/23: table_install and table_get functions in Controller
# 4/26/23: added pktgen support, boolean fields

# helper for globals.json: given a node from globals.json, resolve 
# name of a lucid global to a P4 object
def resolve_global_name(name, node):    
    """resolve object name to a materialized global from node in directory"""
    names = name.split(".")
    # the name must be resolved in this node
    if (len(names) == 1):
      if (name in node):
        leaf = node[name]
        if ('compiled_name' in leaf):
          return leaf['compiled_name']
        else:
          return None
      elif ("actions" in node):
        actions = {a["name"]:a["compiled_name"] for a in node["actions"]}
        if (names[0] in actions):      
            return actions[names[0]]
        else:
          return None
      else:
        return None
    # the name is compound, we have to traverse the directory more.
    else:
      if (names[0] in node):
        node = node[names[0]]
        name = ".".join(names[1::])
        return resolve_global_name(name, node)
      # tables are leaves also
      else:
        return None

def local_port_to_global_dpid(local_port, pipe):
  return pipe << 7 | local_port


class Controller(object):
  """ the high-level controller interface. 
      opens an interface to bf_switchd, 
      loads the tables of the program, 
      and provides methods to do common 
      modifications, e.g., adding rules, mc groups, etc """
  def __init__(self, clib_driver_obj_fn):
    if (clib_driver_obj_fn == None):
      raise (ValueError("error finding driver library .so file")) 
      return
    self.libc = LibcInterface(clib_driver_obj_fn)
    prognames = list(self.libc.progs.keys())
    self.next_mc_node_id = 1
    if (len(prognames) == 0):
      raise ValueError("no program appears to be loaded into bf_switchd")
    if (len(prognames) > 1):
      raise ValueError("more than 1 program appears to be loaded into bf_switchd")
    self.progname = prognames[0]
    self.tables = self.libc.progs[self.progname]

    # handles that should be destroyed on cleanup.
    # (table, entry_hdl pairs)
    self.installed_entries = []

    self.pktgen_app_hdl = None

  def cleanup(self):
    dprint ("deleting {0} entries".format(len(self.installed_entries)))
    for (tbl, key_hdl) in self.installed_entries:
      dprint ("deleting entry from table: {0}".format(tbl.name))
      tbl.del_entry(key_hdl)
  def close(self):
    self.libc.close_session()    

  def add_multicast_group(self, mc_gid, ports_rids):
    """Add a basic multicast group that clones to all (port, rid) in ports_rids"""
    print ("[add_multicast_group] adding mc group: {0}--> [{1}]".format(str(mc_gid), str(ports_rids)))
    node_tbl = self.tables['$pre.node']
    node_ids = []
    for i, (port, rid) in enumerate(ports_rids):    
      node_id = self.next_mc_node_id
      node_ids.append(node_id)
      self.next_mc_node_id += 1
      key_hdl = node_tbl.add_entry(
        {'$MULTICAST_NODE_ID':node_id}, None,
        {'$DEV_PORT':[port], '$MULTICAST_RID':rid}, ret_hdl=True)
      self.installed_entries.append((node_tbl, key_hdl))
    mc_tbl = self.tables['$pre.mgid']
    key_hdl = mc_tbl.add_entry({'$MGID':mc_gid}, None,
      {'$MULTICAST_NODE_ID':node_ids,
       '$MULTICAST_NODE_L1_XID':[0 for i in ports_rids],
       '$MULTICAST_NODE_L1_XID_VALID':[False for i in ports_rids]}, ret_hdl=True)
    self.installed_entries.append((mc_tbl, key_hdl))
    print ("[add_multicast_group] done")

  def array_set(self, array_id, idx, val): 
    """ set array_id[idx] = val, for all pipes. array_id must 
        be the fully qualified array name, i.e., as defined in the 
        bfrt.json file. """
    dprint ("[array_set] %s[%s] = %s"%(array_id, idx, val))
    arr = self.tables[array_id]
    arr.add_entry(idx, None, val)

  def array_get(self, array_id, idx):
    """ get array_id[idx] from all pipes. array_id must 
        be the fully qualified array name, i.e., as defined in the 
        bfrt.json file. Return value is a list, with one value 
        from each pipeline """
    arr = self.tables[array_id]
    # output will be (None [because no action], {"f1"=[...]})
    _, vals = arr.get_entry(idx)
    vals = list(vals.values())
    return vals

  def table_install(self, table_id, key, action_id, args):
    """ Install an entry into a table. Return the entry handle for deletion. """
    tbl = self.tables[table_id]
    tbl.add_entry(key, action_id, args)

  def table_get(self, table_id, key):
    """ Get the action associated with a key in a table """
    tbl = self.tables[table_id]
    acn_id, acn_args = tbl.get_entry(key)
    return acn_id, acn_args

  ### pktgen helpers
  def start_periodic_pktgen(self, pkt, timer_ns=1_000_000_000, pktgen_port=196, generator_id=1):
    """start the packet generater on port pktgen_port, 
       with id generator_id, sending pkt every timer_ns ns"""
    # notes: pktgen pkts have a mandatory 6-byte header prepended to them.
    # the parser has to adjust for this, by skipping the first 6 bytes of packets from the pktgen port

    # for now, we assume that the pktgen port is either 196 or 68. 
    # These are the two pktgen capable ports on a 2-pipeline tofino1.
    # Different chip models will support other pktgen ports.
    if (pktgen_port not in [196, 68]):
      raise ValueError("pktgen port must be either 196 or 68")
    # 1. put the port into pktgen mode
    port_cfg_tbl = self.tables['tf1.pktgen.port_cfg']
    dprint("loaded port_cfg tbl")
    port_cfg_key = {'dev_port':pktgen_port}
    port_cfg_acn = {'pktgen_enable':True}
    port_cfg_tbl.add_entry(port_cfg_key, None, port_cfg_acn)
    dprint("added entry to port_cfg tbl")
    # 2. load the packet into the pktgen buffer
    pkt_len = len(pkt)
    pkt_buf_tbl = self.tables['tf1.pktgen.pkt_buffer']
    pkt_buf_key = {"pkt_buffer_offset":0, "pkt_buffer_size":pkt_len}
    pkt_buf_val = {"buffer":bytearray(pkt)}
    pkt_buf_tbl.add_entry(pkt_buf_key, None, pkt_buf_val)
    dprint("added packet to pkt_buffer tbl")
    # 3. configure the pktgen app ("app_cfg" table) and start pktgen. 
    # note: to start pktgen later, you add a configuration with app_enable=False
    # and then modify the entry later and set app_enable=True
    app_cfg_tbl = self.tables['tf1.pktgen.app_cfg']
    app_cfg_key = {"app_id":generator_id}
    app_cfg_acn_params = {
      "app_enable":True,
      "timer_nanosec":timer_ns, 
      "pkt_len":pkt_len, 
      "pkt_buffer_offset":0, 
      "pipe_local_source_port":68, # I think always 68 on tofino1
      "increment_source_port":False,
      "batch_count_cfg":0, # number of packets per batch + 1
      "packets_per_batch_cfg":0 # number of batches + 1
      }
      # other parameters that we don't need for a simple periodic packetgen:
      # "ibg":1,"ibg_jitter":0,"ipg":2_000_000_000,"ipg_jitter":0,"batch_counter":0,"pkt_counter":0,"trigger_counter":0}
    app_cfg_tbl.add_entry(app_cfg_key, "trigger_timer_periodic", app_cfg_acn_params)

  def stop_periodic_pktgen(self, generator_id):
    """stop the pktgen generator_id Note: packet buffer is not reset."""
    app_cfg_tbl = self.tables['tf1.pktgen.app_cfg']
    app_cfg_key = {"app_id":generator_id}
    app_cfg_acn_params = {"app_enable":False}
    app_cfg_tbl.add_entry(app_cfg_key, "trigger_timer_periodic", app_cfg_acn_params)

class BfRtTable:
    """Class to operate on tables, both defined in P4 and also 
       fixed tables. A program is just a list of named tables. """
    def __init__(self, cintf, handle):
      self._cintf = cintf
      self._handle = handle

      self.name = self._cintf.table_name_get(self._handle).value.decode('ascii')
      self.bf_rt_id = self._cintf.table_id_get(self._handle).value
      self.has_const_default_action = self._cintf.table_has_const_default_action(self._handle)

      dprint ("TABLE %s"%(self.name))
      ### loading table definitions ###
      self.load_key()
      self.load_actions()
      self.load_data()

    def load_key(self):
      # ref: _init_key
      self.key_fields = {}
      field_ids = self._cintf.key_field_id_list_get_array(self._handle)
      dprint ("----key fields---- ")
      for idx, field_id in enumerate(field_ids):
          field_name = self._cintf.key_field_name_get(self._handle, field_id).value.decode('ascii')
          field_type = self._cintf.key_field_type_get(self._handle, field_id).value
          field_data_type = self._cintf.key_field_data_type_get(self._handle, field_id).value
          field_size = self._cintf.key_field_size_get(self._handle, field_id).value
          field_is_ptr = self._cintf.key_field_is_ptr_get(self._handle, field_id).value
          readable = "{!s:30} type={!s:10} size={:^2}".format(field_name, key_type_map(field_type), field_size)
          self.key_fields[field_name] = {
            "name":field_name,
            "index":idx, # position of the field in the key
            "id":field_id,
            "type":field_type,
            "data_type":field_data_type,
            "size":field_size,
            "is_ptr":field_is_ptr
          }
          dprint(readable)
      # make sure fields are sorted in metadata dict (probably not necessary)
      self.key_fields = ensure_sorted(self.key_fields)

    def load_actions(self):
      # ref: _init_actions
      self.actions = {}
      self.action_id_name_map = {}
      is_action_applicable = self._cintf.action_id_applicable(self._handle)
      if (not is_action_applicable):
        return 0
      action_ids = self._cintf.action_id_list_get_array(self._handle)
      dprint ("----actions----")
      for action_id in action_ids:
        action_name = self._cintf.action_name_get(self._handle, action_id).value.decode('ascii')
        dprint (action_name)
        self.actions[action_name] = {"id" : action_id, "data_fields" : {}}
        self.action_id_name_map[action_id] = action_name
      return 0


    def load_field_annotation(self, field_id, action_id):
      """load annotations of a data field """
      if (action_id == None):
        annotations_arr = self._cintf.data_field_annotations_get_array(self._handle, field_id)
      else:
        annotations_arr = self._cintf.data_field_annotations_with_action_get_array(self._handle, field_id, action_id)
      return [(ann.name.decode('ascii'), ann.value.decode('ascii')) for ann in annotations_arr]

    def load_data_field(self, data_fields, idx, field_id, action_id=None, action_name=None, depth=0):
      """ load information about a data field """
      # get annotation for this field -- we must do this before setting default action_id, as 
      # there are different driver functions for getting annotations on fields not associated 
      # with actions.
      annotations = self.load_field_annotation(field_id, action_id)
      # if there is no action id, just pass identifier 0
      if not action_id:
        action_id = c_uint(0)
      field_type = self._cintf.data_field_type_with_action_get(self._handle, field_id, action_id).value
      field_name = self._cintf.data_field_name_with_action_get(self._handle, field_id, action_id).value.decode('ascii')
      field_size = self._cintf.data_field_size_with_action_get(self._handle, field_id, action_id).value
      field_is_ptr = self._cintf.data_field_is_ptr_with_action_get(self._handle, field_id, action_id).value
      read_only = self._cintf.data_field_is_read_only_with_action_get(self._handle, field_id, action_id).value
      mandatory = self._cintf.data_field_is_mandatory_with_action_get(self._handle, field_id, action_id).value
      readable = "{!s:30} type={!s:10} size={:^2}".format(field_name, data_type_map(field_type), field_size)
      data_fields[field_name] = {
        "name":field_name,
        "id":field_id,
        "index":idx, 
        "size":field_size,
        "is_ptr":field_is_ptr,
        "read_only":read_only,
        "mandatory":mandatory,
        "action_name":action_name,
        "action_id":action_id,
        "data_type":field_type,
        "cont_data_fields":{},
        "annotations":annotations
      }
      dprint (readable)        
      if (len(annotations) == 0):
        dprint ("\tno annotations")
      else:
        for (name, val) in annotations:
          dprint("\tannotation: %s = %s"%(name, val))
      if (data_type_map(field_type) == "CONTAINER"):
        arr_t = c_uint * field_size
        c_fields = arr_t()
        self._cintf.bf_rt_container_data_field_list_get(self._handle, field_id, c_fields)

        # container_fields = self._cintf.container_data_field_list_get_array(self._handle, field_id)
        # print ("***Container with %i fields"%(len(c_fields)))
        # recurse on fields of the container
        for idx, c_field_id in enumerate(c_fields):
          self.load_data_field(data_fields[field_name]["cont_data_fields"], idx, c_field_id, depth=depth+1)
          data_fields[field_name]["cont_data_fields"] = ensure_sorted(data_fields[field_name]["cont_data_fields"])
        # print ("***")
      return 0

    def load_data(self):
      # ref: _init_data
      self.data_fields = {}
      dprint ("----data fields----")
      if len(self.actions) == 0:
        field_ids = self._cintf.data_field_id_list_get_array(self._handle)
        for idx, field_id in enumerate(field_ids):
          self.load_data_field(self.data_fields, idx, field_id)
        # ensure ordering (not necessary?)
        self.data_fields = ensure_sorted(self.data_fields)

      else:
        for action_name, action_info in self.actions.items():
          field_ids = self._cintf.data_field_list_with_action_get_array(self._handle, action_info["id"])          
          for idx, field_id in enumerate(field_ids):
            # note that we pass the action_info dict because 
            # it may contain nested fields that get filled recursively.
            self.load_data_field(action_info["data_fields"], idx, field_id, action_info["id"], action_name)
            action_info["data_fields"] = ensure_sorted(action_info["data_fields"])

    ### table update helpers ###
    def create_key(self, entry):      
      # _set_key_fields
      dprint ("filling key")
      key_hdl = self._cintf.table_key_allocate(self._handle)
      # if the key is a list, transform it into a dict
      if (type(entry) == list):
        # priority, the last field, is missing -- use default of 0
        if (len(entry) == len(self.key_fields) - 1):
          entry = entry + [0]
        entry = {k:v for (k, v) in zip(self.key_fields.keys(), entry)}
      for field_name, field_val in entry.items():
        field_info = self.key_fields[field_name]
        dprint (field_info["data_type"])
        if (key_type_map(field_info["type"]) == "EXACT"):
          if (data_type_map(field_info["data_type"]) != "String"):
            if (not field_info["is_ptr"]):
              dprint ("filling entry[{0}] : {1}".format(field_name, field_val))
              self._cintf.key_field_set_value(key_hdl, field_info["id"], c_ulonglong(field_val))
              continue
            else:
              print ("unexpected: key field pointer")
          else:
            print ("unexpected: key field is string")
        elif (key_type_map(field_info["type"]) == "TERNARY"):
          if (data_type_map(field_info["data_type"]) != "String"):
            if (not field_info["is_ptr"]):
              # default mask is 2^(size) - 1 (e.g., 0xff for an 8-bit field)
              value, mask = 0, ((1<<field_info["size"]) - 1)
              if (type(field_val) == tuple):
                value, mask = field_val
              dprint ("ternary key column: (value:%s, mask%s)"%(value, mask))
              value = c_ulonglong(value)
              mask = c_ulonglong(mask)
              self._cintf.key_field_set_value_and_mask(key_hdl, field_info["id"], value, mask)
              continue
            else:
              print ("unexpected: key field pointer")
          else:
            print ("unexpected: key field is string")
        else:
          # TODO: other match types
          print ("unexpected key field type")
      return key_hdl

    # fill all the data fields
    def fill_data_fields(self, data_hdl, args, fields_info):
      dprint ("[fill_data_fields] args:%s"%(str(args)))
      # args may be a list, which we transform into a dictionary
      if (type(args) == list):        
        args = {k:v for (k, v) in zip(fields_info.keys(), args)}

      for field_name, field_val in args.items():
        field_info = fields_info[field_name]
        data_type = data_type_map(field_info["data_type"])
        dprint ("setting field: {0} = {1}".format(field_name, field_val))
        if (data_type == "BYTE_STREAM"):
          cval, n_bytes = to_c_byte_arr(field_val, field_info["size"])
          dprint ("[fill_data_fields / BYTE_STREAM] field: {0} value: {1} n_bytes: {2}".format(field_name, cval, n_bytes))
          self._cintf.data_field_set_value_ptr(data_hdl, field_info["id"], cval, n_bytes)
        elif (data_type == "UINT64"):
          if (not field_info["is_ptr"]):
            c_value = c_ulonglong(field_val)
            self._cintf.data_field_set_value(data_hdl, field_info["id"], c_value)
          else:
            value, n_bytes = self.to_c_byte_arr(field_val, field_info["size"])
            self._cintf.data_field_set_value_ptr(data_hdl, field_info["id"], value, n_bytes)
        elif (data_type == "INT_ARR"):
          # handle 8-bit integer arrays as a byte stream -- 
          # this is how bf_rt_pktgen_table_data_impl.cpp is implemented
          if(field_info["size"] == 8):
            if (type(field_val) == bytearray):
              n_bytes = len(field_val)
              value_type = c_ubyte * n_bytes
              value = value_type()
              for i in range(0, len(field_val)):
                value[i] = field_val[i]
              self._cintf.data_field_set_value_ptr(data_hdl, field_info["id"], value, n_bytes)
            else:
              print ("ERROR: a data field of type INT_ARR with size 8 must be a bytearray")
              exit(1)              
          # untested, and no idea what to do with arrays of integers of other sizes -- 
          # this is consistent with BfTableEntry, but seems to assume 32-bit integers...
          else:
            arrlen = len(field_val)
            arrty = c_uint * arrlen
            value = arrty()
            for idx, v in enumerate(field_val):
              value[idx] = v
        elif (data_type == "BOOL_ARR"):
          arrlen = len(field_val)
          arrty = c_uint * arrlen
          value = arrty()
          for idx, v in enumerate(field_val):
            value[idx] = v
          self._cintf.data_field_set_value_bool_array(data_hdl, field_info["id"], value, arrlen)
        elif (data_type == "BOOL"):
          c_value = c_bool(field_val)          
          self._cintf.data_field_set_bool(data_hdl, field_info["id"], c_value)
        else:
          # TODO: other field types 
          print ("unhandled field data type: %s"%(data_type))
          return False
      return True

    def create_action(self, action_name, action_args):
      """ 
        create a data struct for an entry's action, 
        fill it with the action identifier and the action args
        action : string
        action_args : dict(arg_name -> arg_val)
      """
      # ref set_data_fields
      action_hdl = self._cintf.table_action_data_allocate(self._handle, self.actions[action_name]["id"])
      succ = self.fill_data_fields(action_hdl, action_args, self.actions[action_name]["data_fields"])
      # ref _set_data_field
      return succ, action_hdl

    def create_data(self, args):
      """ Create an actionless data entry """
      data_hdl = self._cintf.table_data_allocate(self._handle)
      succ = self.fill_data_fields(data_hdl, args, self.data_fields) 
      return succ, data_hdl

    def create_empty_data(self):
      """ create an empty data handle for this table (for gets) """
      return self._cintf.table_data_allocate(self._handle)

    def cintf_table_add(self, key_hdl, data_hdl):
      # wrapper for table_entry_add that includes fixed args
      return self._cintf.table_entry_add(
          self._handle, 
          self._cintf.get_session(), 
          self._cintf.get_dev_tgt(), 0, # session info + "flags"
          key_hdl, data_hdl)  

    def cintf_table_get(self, key_hdl, data_hdl):
      # wrapper for table_entry_get that includes fixed args
      return self._cintf.table_entry_get(
          self._handle,
          self._cintf.get_session(),
          self._cintf.get_dev_tgt(), 1,# 1 is flag to read from hardware
          key_hdl, data_hdl) 

    def get_data_fields(self, data_handle, action): 
      """ read the fields of table or action """
      data_fields = self.data_fields
      # if the entry is an action entry, get that action's data fields
      if (action != None):
        data_fields = self.actions[action]["data_fields"]
      # actually get the data fields.
      results = {}
      for name, info in data_fields.items():
        is_active = c_bool(False)
        # not sure what field_is_active checks
        self._cintf.data_field_is_active(data_handle, info["id"], byref(is_active))
        dtype = data_type_map(info["data_type"])     
        if (is_active):
          if (dtype == "BYTE_STREAM"):
            # case: register read
            if (('$bfrt_field_class', 'register_data') in info["annotations"]):
              raw_field_vals = self._cintf.data_field_get_value_u64_array(data_handle, info["id"])
              field_vals = [raw_field_vals[i] for i in range(len(raw_field_vals))]
              results[name] = field_vals
            # case: action argument
            else:
              cval, n_bytes = to_c_byte_arr(0, info["size"])
              # fill val
              self._cintf.data_field_get_value_ptr(data_handle, info["id"], n_bytes, cval)
              results[name] = from_c_byte_arr(cval, info["size"])
          else:
            dprint("[get_data_fields] unsupported field type: %s"%(dtype))
      return (action, results)

    def get_entry(self, key): 
      """ get an entry from a table or array by key. 
          For an array, the key is just the index and the return is a list of values, 
          with 1 value from each pipeline's copy of the array. """
      # make key of entry we are getting
      key_hdl = self.create_key(key)
      # make handle that stores value(s) read
      data_hdl = self.create_empty_data()
      retcode = self.cintf_table_get(key_hdl, data_hdl)
      if (retcode != 0):
          print ("WARNING: table_entry_get failed. retcode: %i"%retcode)
          self._cintf.table_key_deallocate(key_hdl)
          self._cintf.table_data_deallocate(data_hdl)
          return None

      if (len(self.actions) > 0):
        action_name = self.action_id_name_map[self._cintf.data_action_id_get(data_hdl).value]
        action_args = self.get_data_fields(data_hdl, action_name)      
        self._cintf.table_key_deallocate(key_hdl)
        self._cintf.table_data_deallocate(data_hdl)

        return action_name, action_args
      else:
        dprint("calling get_data_fields...")
        res = self.get_data_fields(data_hdl, None)      
        self._cintf.table_key_deallocate(key_hdl)
        self._cintf.table_data_deallocate(data_hdl)
        return res

    def add_entry(self, key, action_name, args, ret_hdl=False):
      """ add or update a table entry by key. 
          Returns the key handle if ret_hdl=True and the install succeeded """
      # key : dict( name_string -> match val(for exact) | match tuple(for ternary / range / etc))
      # action_name : string
      # args : dict (name_string -> arg val)
      # returns the key handle if ret_hdl=True and the install succeeded
      dprint ("add entry in %s"%self.name)
      key_hdl = self.create_key(key)
      if (action_name != None):
        succ, data_hdl = self.create_action(action_name, args)
        if (not succ):
          print ("WARNING: could not build action struct.")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return None

        dprint ("calling table_entry_add")
        retcode = self.cintf_table_add(key_hdl, data_hdl)

        if (retcode != 0):
          print ("WARNING: table_entry_add failed. retcode: %i"%retcode)
          self._cintf.table_key_deallocate(key_hdl)
          self._cintf.table_data_deallocate(data_hdl)
          return None
        else:
          dprint ("table_entry_add success")
          self._cintf.table_data_deallocate(data_hdl)
          if (ret_hdl):
            return key_hdl
          else:
            self._cintf.table_key_deallocate(key_hdl)
            return None
      else:
        dprint ("adding NO ACTION ENTRY")        
        succ, data_hdl = self.create_data(args)
        if (succ == False):
          print ("WARNING: could not build entry data struct.")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return None
        retcode = self.cintf_table_add(key_hdl, data_hdl)
        if (retcode != 0):
          print ("WARNING: table_entry_add failed")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return None
        else:
          dprint ("table_entry_add success")
          self._cintf.table_data_deallocate(data_hdl)    
          if (ret_hdl):
            return key_hdl
          else:
            self._cintf.table_key_deallocate(key_hdl)
            return None

      raise (ValueError("Internal error: add_entry should not reach this point."))

    def del_entry(self, key_handle):
      """delete an entry, given an already existing key handle """
      sts = self._cintf.table_entry_del(
        self._handle, 
        self._cintf.get_session(),
        self._cintf.get_dev_tgt(),
        0,
        key_handle)
      if (sts != 0):
        print("del entry failed")
        print(self._cintf.err_str(sts))
      # deallocate key
      self._cintf.table_key_deallocate(key_handle)

# type maps
def key_type_map(key_type):
    if key_type == 0:
        return "INVALID"
    elif key_type == 1:
        return "EXACT"
    elif key_type == 2:
        return "TERNARY"
    elif key_type == 3:
        return "RANGE"
    elif key_type == 4:
        return "LPM"
    elif key_type == 5:
        return "OPTIONAL"
    else:
        return "BFRT_CLI_KEY_TYPE_NOT_IMPLEMENTED"

def data_type_map(data_type):
    if data_type == 0:
        return "INT_ARR"
    elif data_type == 1:
        return "BOOL_ARR"
    elif data_type == 2:
        return "UINT64"
    elif data_type == 3:
        return "BYTE_STREAM"
    elif data_type == 4:
        return "FLOAT"
    elif data_type == 5:
        return "CONTAINER"
    elif data_type == 6:
        return "STRING"
    elif data_type == 7:
        return "BOOL"
    elif data_type == 8:
        return "STR_ARR"
    else:
        return "BFRT_CLI_DATA_TYPE_NOT_IMPLEMENTED"


class LibcInterface(object):
  """ Low level interface to the C driver library. """
  def __init__(self, libdriver_fn):
    # open dll
    self._driver = CDLL(libdriver_fn)
    self._dev_id = 0
    # init types
    self.init_tys()
    # add wrapped functions
    self.wrap_clib_fcns()
    # load program info
    self.load_progs()
    # start session
    if (not self.start_session()):
      raise(ValueError("Critical error: could not start session."))
    # set the target to all pipelines
    self._dev_tgt = BfDevTgt(self._dev_id, 0xFFFF, 0xff, 0xff)

  def init_tys(self):
    """ setup c struct types """
    self.sess_type = POINTER(c_uint)
    self.handle_type = POINTER(BfRtHandle)
    self.annotation_type = self.BfAnnotation    

  class BfAnnotation(Structure):
      _fields_ = [("name", c_char_p), ("value", c_char_p)]


  def load_progs(self):
    """ get names of p4 programs loaded into switch, 
        then load their names and other info """
    # get names of p4 programs loaded onto switchd
    num_names = c_int(-1)
    self._driver.bf_rt_num_p4_names_get(self._dev_id, byref(num_names))
    array_type = c_char_p * num_names.value
    p4_names = array_type()
    self._driver.bf_rt_p4_names_get(self._dev_id, p4_names)
    # load programs. each progam is just a dictionary of tables.
    self.progs = {}
    for name in p4_names:
      prog_handle = self.info_get(self._dev_id, name)
      tables = {}
      for table_hdl in self.tables_get_array(prog_handle):
        tbl_obj = BfRtTable(self, table_hdl)      
        tables[tbl_obj.name] = tbl_obj
      self.progs[name] = tables

  def start_session(self):
    self._session = self.sess_type()
    sts = self._driver.bf_rt_session_create(byref(self._session))
    if (sts != 0):
      return False    
    return True

  def close_session(self):
    # close the session
    print ("closing session")
    self._driver.bf_rt_session_destroy(self._session)

  # public helpers 
  def get_session(self):
      return self._session
  def get_dev_tgt(self):
      return byref(self._dev_tgt)
  def err_str(self, sts):
      estr = c_char_p()
      self._driver.bf_rt_err_str(c_int(sts), byref(estr))
      return estr.value.decode('ascii')

  # command wrapping
  def run_cmd(self, fcn, *args):
    return fcn(*args)

  def try_cmd_with_err(self, emsg, fcn, *args):
    if (0 != fcn(*args)):
      raise ValueError(emsg)

  def wrap_clib_fcn(self, fname, ret_obj_constr, ret_obj_constr_args, fcn):
    """ Wrap a clib function that fills the object referenced by the last argument """
    def fcn_wrapper(*args):
      ret_obj = ret_obj_constr(*ret_obj_constr_args)
      args = args + (byref(ret_obj),)
      # print ("calling try_cmd with args: %s"%(str(args)))
      self.try_cmd_with_err("error calling c lib function %s"%(fname), fcn, *args)
      return ret_obj
    setattr(self, fname, fcn_wrapper)

  def wrap_clib_method(self, fname, fcn):
    """ Wrap a clib function, throw error if retcode is wrong """
    def method_wrapper(*args):
      # print ("calling try_cmd with args: %s"%(str(args)))
      self.try_cmd_with_err("error calling clib method %s"%(fname), fcn, *args)
    setattr(self, fname, method_wrapper)

  def wrap_clib_method_get_returncode(self, fname, fcn):
    """ Wrap a clib function and get its return code """
    def method_wrapper(*args):
      return self.run_cmd(fcn, *args)
    setattr(self, fname, method_wrapper)

  def wrap_clib_array_fcn(self, fname, ret_obj_constr, ret_obj_constr_args, size_fcn, fcn):
    """ Wrap a clib function that fills the _array of objects_ referenced by the last argument """
    def array_fcn_wrapper(*args):
      # get size
      size_obj = c_uint(0)
      size_args = args + (byref(size_obj),)
      self.try_cmd_with_err(fname+" error", size_fcn, *size_args)
      size = size_obj.value
      # use size to build proper return obj, pass to array getter
      ret_arr_obj = (size * ret_obj_constr)(*ret_obj_constr_args)
      args = args + (byref(ret_arr_obj),)
      self.try_cmd_with_err(fname + " error", fcn, *args)
      return ret_arr_obj
    setattr(self, fname, array_fcn_wrapper)

  def wrap_clib_fcns(self):
    """ Wrap all the functions from the driver that we use """
    fcn_sigs = [
      ("info_get", self.handle_type, (), self._driver.bf_rt_info_get),
      ("table_name_get", c_char_p, (), self._driver.bf_rt_table_name_get),
      ("table_id_get", c_uint, (0,), self._driver.bf_rt_table_id_from_handle_get),
      ("table_has_const_default_action", c_bool, (False,), self._driver.bf_rt_table_has_const_default_action),
      ("key_field_name_get", c_char_p, (), self._driver.bf_rt_key_field_name_get),
      ("key_field_type_get", c_int, (-1,), self._driver.bf_rt_key_field_type_get),
      ("key_field_data_type_get", c_int, (-1,), self._driver.bf_rt_key_field_data_type_get),
      ("key_field_size_get", c_uint, (), self._driver.bf_rt_key_field_size_get),
      ("key_field_is_ptr_get", c_bool, (), self._driver.bf_rt_key_field_is_ptr_get),

      ("data_field_name_with_action_get", c_char_p, (), self._driver.bf_rt_data_field_name_with_action_get),
      ("data_field_type_with_action_get", c_int, (-1,), self._driver.bf_rt_data_field_type_with_action_get),
      ("data_field_size_with_action_get", c_uint, (), self._driver.bf_rt_data_field_size_with_action_get),
      ("data_field_is_ptr_with_action_get", c_bool, (), self._driver.bf_rt_data_field_is_ptr_with_action_get),
      ("data_field_is_read_only_with_action_get", c_bool, (), self._driver.bf_rt_data_field_is_read_only_with_action_get),
      ("data_field_is_mandatory_with_action_get", c_bool, (), self._driver.bf_rt_data_field_is_mandatory_with_action_get),
      ("action_id_applicable", c_bool, (), self._driver.bf_rt_action_id_applicable),
      ("action_name_get", c_char_p, (), self._driver.bf_rt_action_name_get),

      ("table_key_allocate", self.handle_type, (), self._driver.bf_rt_table_key_allocate),
      ("table_action_data_allocate", self.handle_type, (), self._driver.bf_rt_table_action_data_allocate),
      ("table_data_allocate", self.handle_type, (), self._driver.bf_rt_table_data_allocate),
      ("data_action_id_get", c_uint, (-1,), self._driver.bf_rt_data_action_id_get)

    ]
    method_sigs = [
      ("key_field_set_value", self._driver.bf_rt_key_field_set_value),
      ("key_field_set_value_and_mask", self._driver.bf_rt_key_field_set_value_and_mask),
      ("bf_rt_container_data_field_list_get", self._driver.bf_rt_container_data_field_list_get),
      ("data_field_set_value_ptr", self._driver.bf_rt_data_field_set_value_ptr),
      ("data_field_get_value_ptr", self._driver.bf_rt_data_field_get_value_ptr),
      ("data_field_set_value", self._driver.bf_rt_data_field_set_value),
      ("table_key_deallocate", self._driver.bf_rt_table_key_deallocate),
      ("table_data_deallocate", self._driver.bf_rt_table_data_deallocate),
      ("data_field_set_value_array", self._driver.bf_rt_data_field_set_value_array),
      ("data_field_set_value_bool_array", self._driver.bf_rt_data_field_set_value_bool_array),
      ("data_field_set_bool", self._driver.bf_rt_data_field_set_bool),
      ("table_entry_key_get", self._driver.bf_rt_table_entry_key_get),
      ("data_field_is_active", self._driver.bf_rt_data_field_is_active),
    ]
    method_with_retcode = [
      ("table_entry_add", self._driver.bf_rt_table_entry_add),
      ("table_entry_del", self._driver.bf_rt_table_entry_del),
      ("table_entry_mod", self._driver.bf_rt_table_entry_mod),
      ("table_entry_get", self._driver.bf_rt_table_entry_get),
    ]
    array_fcn_sigs = [
      # wrapped function name, array element type, array type args, length getter, element getter
      ("tables_get_array", self.handle_type, (),
        self._driver.bf_rt_num_tables_get,
        self._driver.bf_rt_tables_get),
      ("key_field_id_list_get_array", c_uint, (), 
        self._driver.bf_rt_key_field_id_list_size_get,
        self._driver.bf_rt_key_field_id_list_get),
      ("action_id_list_get_array", c_uint, (), 
        self._driver.bf_rt_action_id_list_size_get,
        self._driver.bf_rt_action_id_list_get),
      ("data_field_id_list_get_array", c_uint, (),
        self._driver.bf_rt_data_field_id_list_size_get,
        self._driver.bf_rt_data_field_list_get),
      ("data_field_list_with_action_get_array", c_uint, (),
        self._driver.bf_rt_data_field_id_list_size_with_action_get,
        self._driver.bf_rt_data_field_list_with_action_get),
      ("data_field_annotations_get_array", self.annotation_type, (),
        self._driver.bf_rt_data_field_num_annotations_get,
        self._driver.bf_rt_data_field_annotations_get),
      ("data_field_annotations_with_action_get_array", self.annotation_type, (),
        self._driver.bf_rt_data_field_num_annotations_with_action_get,
        self._driver.bf_rt_data_field_annotations_with_action_get),
      ("data_field_get_value_u64_array", c_ulonglong, (), # args: (data_handle, field_id)
        self._driver.bf_rt_data_field_get_value_u64_array_size,
        self._driver.bf_rt_data_field_get_value_u64_array)

    ]

    for fcn_sig in fcn_sigs:
      self.wrap_clib_fcn(*fcn_sig)

    for afcn_sig in array_fcn_sigs:
      self.wrap_clib_array_fcn(*afcn_sig)

    for sig in method_sigs:
      self.wrap_clib_method(*sig)

    for sig in method_with_retcode:
      self.wrap_clib_method_get_returncode(*sig)



# classes representing c structs
class BfDevTgt(Structure):
    _fields_ = [("dev_id", c_int), ("pipe_id", c_uint), ("direction", c_uint), ("prsr_id", c_ubyte)]
    def __str__(self):
        ret_val = ""
        for name, type_ in self._fields_:
            ret_val += name + ": " + str(getattr(self, name)) + "\n"
        return ret_val

class BfRtHandle(Structure):
    _fields_ = [("unused", c_int)]


def to_c_byte_arr(py_value, size):
    """Converts an array of python ints into a c byte array of the given size in bits"""
    n_bytes = (size + 7) // 8
    value_type = c_ubyte * n_bytes
    value = value_type()
    for i in range(0, n_bytes):
        value[i] = (py_value >> 8 * (n_bytes - (i + 1))) & 0xFF
    return value, n_bytes

def from_c_byte_arr(value, size):
    n_bytes = (size + 7) // 8
    py_value = 0
    for i in range(0, n_bytes):
        py_value += value[i] << (n_bytes - (i + 1)) * 8
    return py_value


def check_sde_version(sde):
  supported = False
  major, minor = 0, 0
  if (sde != None):
    v = sde.split(".")
    if (len(v) == 2):
      major, minor = v
    elif (len(v) == 3):
      major, minor, _ = v
  if int(major) == 9 and int(minor) >= 5:
    supported = True
  return supported

def ensure_sorted(mydict):
  return dict(sorted(mydict.items(), key=lambda x: x[1]["index"]))

def find_clib_driver_file(toplevel_env):
  """ get the clib driver file from the running bf_switchd. 
      And check to make sure the sde is a supported version 
      toplevel_env is either globals() 
        (if this is called inside a main function)
      or locals()
        (if this is called in the toplevel)
      """
  # we expect to be running inside bf_switchd, 
  # in the python interpreter created when bfshell 
  # runs bfrt_python. At that point, the global 
  # object "bfrt" is defined.
  if ('bfrt' in toplevel_env):
    driver_clib_fn = toplevel_env['bfrt']._cintf._driver._name
    clibp = Path(driver_clib_fn).absolute()
    cur_p = clibp
    sde_ver = None
    while (cur_p != Path("/")):
      cur_p = cur_p.parent
      if (cur_p.is_dir()):
        # find the sde install manifest
        for fn in cur_p.glob('*.*'):
          if ".manifest" in fn.name:
            manifest = open(fn, "r").read()
            import re
            ver = re.findall(r'bf-drivers.*?:\s*?(.*)', manifest)
            if len(ver)>0:
              sde_ver = ver[0].strip()

    sde_supported = check_sde_version(sde_ver)
    if (not sde_supported):
      print ("ERROR: unsupported sde version: {0}".format(sde_ver))
      return None
    return clibp
  else:
    print ("ERROR: bfrt is not in globals. This script was not called in the expected way.")
    return None

def dprint(*args):
  if (DEBUG):
    print(*args)
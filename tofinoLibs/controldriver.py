from ctypes import *
from pathlib import Path

DEBUG=False

# example usage. note that if you wrap 
# this in a main function, locals must change to globals

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
    node_tbl = self.tables["$pre.node"]
    node_ids = []
    for i, (port, rid) in enumerate(ports_rids):    
      node_id = self.next_mc_node_id
      node_ids.append(node_id)
      self.next_mc_node_id += 1  
      key_hdl = node_tbl.add_entry(
        {b'$MULTICAST_NODE_ID':node_id}, None,
        {b'$DEV_PORT':[port], b'$MULTICAST_RID':rid})
      self.installed_entries.append(key_hdl)
    mc_tbl = self.tables["$pre.mgid"]
    key_hdl = mc_tbl.add_entry({b'$MGID':mc_gid}, None,
      {b'$MULTICAST_NODE_ID':node_ids,
       b'$MULTICAST_NODE_L1_XID':[0 for i in ports_rids],
       b'$MULTICAST_NODE_L1_XID_VALID':[False for i in ports_rids]})
    self.installed_entries.append(key_hdl)
    print ("[add_multicast_group] done")

class BfRtTable:
    """Class to operate on tables, both defined in P4 and also 
       fixed tables. A program is just a list of named tables. """
    def __init__(self, cintf, handle):
      self._cintf = cintf
      self._handle = handle

      self.name = self._cintf.table_name_get(self._handle).value
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
      for field_id in field_ids:
          field_name = self._cintf.key_field_name_get(self._handle, field_id)
          field_type = self._cintf.key_field_type_get(self._handle, field_id)
          field_data_type = self._cintf.key_field_data_type_get(self._handle, field_id)
          field_size = self._cintf.key_field_size_get(self._handle, field_id)
          field_is_ptr = self._cintf.key_field_is_ptr_get(self._handle, field_id)
          readable = "{!s:30} type={!s:10} size={:^2}".format(field_name.value.decode('ascii'), key_type_map(field_type.value), field_size.value)
          self.key_fields[field_name.value] = {
            "name":field_name.value,
            "id":field_id,
            "type":field_type.value,
            "data_type":field_data_type.value,
            "size":field_size.value,
            "is_ptr":field_is_ptr.value
          }
           # self.BfRtTableField(field_name.value, field_id, field_size.value, field_is_ptr.value, False, True, "key", self, data_type_=field_data_type.value, type_=field_type.value)
          dprint(readable)

          # print (field_name.value.decode('ascii'))
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
        action_name = self._cintf.action_name_get(self._handle, action_id)
        dprint (action_name.value.decode('ascii'))
        self.actions[action_name.value] = {"id" : action_id, "data_fields" : {}}
      return 0

    def load_data_field(self, data_fields, field_id, action_id=None, action_name=None, depth=0):
      if not action_id:
        action_id = c_uint(0)
      field_type = self._cintf.data_field_type_with_action_get(self._handle, field_id, action_id)
      field_name = self._cintf.data_field_name_with_action_get(self._handle, field_id, action_id)
      field_size = self._cintf.data_field_size_with_action_get(self._handle, field_id, action_id)
      field_is_ptr = self._cintf.data_field_is_ptr_with_action_get(self._handle, field_id, action_id)
      read_only = self._cintf.data_field_is_read_only_with_action_get(self._handle, field_id, action_id)
      mandatory = self._cintf.data_field_is_mandatory_with_action_get(self._handle, field_id, action_id)
      readable = "{!s:30} type={!s:10} size={:^2}".format(field_name.value.decode('ascii'), data_type_map(field_type.value), field_size.value)
      data_fields[field_name.value] = {
        "name":field_name.value,
        "id":field_id,
        "size":field_size.value,
        "is_ptr":field_is_ptr.value,
        "read_only":read_only.value,
        "mandatory":mandatory.value,
        "action_name":action_name,
        "action_id":action_id,
        "data_type":field_type.value,
        "cont_data_fields":{}
      }
      dprint (readable)        
      if (data_type_map(field_type.value) == "CONTAINER"):
        arr_t = c_uint * field_size.value
        c_fields = arr_t()
        self._cintf.bf_rt_container_data_field_list_get(self._handle, field_id, c_fields)

        # container_fields = self._cintf.container_data_field_list_get_array(self._handle, field_id)
        # print ("***Container with %i fields"%(len(c_fields)))
        # recurse on fields of the container
        for c_field_id in c_fields:
          self.load_data_field(data_fields[field_name.value]["cont_data_fields"], c_field_id, depth=depth+1)
        # print ("***")
      return 0

    def load_data(self):
      # ref: _init_data
      self.data_fields = {}
      dprint ("----data fields----")
      if len(self.actions) == 0:
        field_ids = self._cintf.data_field_id_list_get_array(self._handle)
        for field_id in field_ids:
          self.load_data_field(self.data_fields, field_id)
      else:
        for action_name, action_info in self.actions.items():
          field_ids = self._cintf.data_field_list_with_action_get_array(self._handle, action_info["id"])
          for field_id in field_ids:
            self.load_data_field(action_info["data_fields"], field_id, action_info["id"], action_name)

    ### table update helpers ###
    def create_key(self, entry):      
      # _set_key_fields
      dprint ("filling key")
      key_hdl = self._cintf.table_key_allocate(self._handle)
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
            print ("unexpected: key field i string")
        elif (key_type_map(field_info["type"]) == "TERNARY"):
          # TODO: ternary matches
          print ("UNIMPLEMENTED: ternary key fields")
        else:
          # TODO: other match types
          print ("unexpected key field type")
      return key_hdl

    # fill all the data fields
    def fill_data_fields(self, data_hdl, args, fields_info):
      dprint ("[fill_data_fields] args:%s"%(str(args)))
      for field_name, field_val in args.items():
        field_info = fields_info[field_name]
        data_type = data_type_map(field_info["data_type"])
        dprint ("setting field: {0} = {1}".format(field_name, field_val))
        if (data_type == "BYTE_STREAM"):
          value, n_bytes = fill_c_byte_arr(field_val, field_info["size"])
          dprint ("[fill_data_fields / BYTE_STREAM] field: {0} value: {1} n_bytes: {2}".format(field_name, value, n_bytes))
          self._cintf.data_field_set_value_ptr(data_hdl, field_info["id"], value, n_bytes)
        elif (data_type == "UINT64"):
          if (not field_info["is_ptr"]):
            c_value = c_ulonglong(field_val)
            self._cintf.data_field_set_value(data_hdl, field_info["id"], c_value)
          else:
            value, n_bytes = self.fill_c_byte_arr(field_val, field_info["size"])
            self._cintf.data_field_set_value_ptr(data_hdl, field_info["id"], value, n_bytes)
        elif (data_type == "INT_ARR"):
          arrlen = len(field_val)
          arrty = c_uint * arrlen
          value = arrty()
          for idx, v in enumerate(field_val):
            value[idx] = v
          self._cintf.data_field_set_value_array(data_hdl, field_info["id"], value, arrlen)
        elif (data_type == "BOOL_ARR"):
          arrlen = len(field_val)
          arrty = c_uint * arrlen
          value = arrty()
          for idx, v in enumerate(field_val):
            value[idx] = v
          self._cintf.data_field_set_value_bool_array(data_hdl, field_info["id"], value, arrlen)
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

    def cintf_table_add(self, key_hdl, data_hdl):
      return self._cintf.table_entry_add(
          self._handle, 
          self._cintf.get_session(), self._cintf.get_dev_tgt(), 0, # session info + "flags"
          key_hdl, data_hdl)

    def add_entry(self, key, action_name, args):
      # key : dict( name_string -> match val(for exact) | match tuple(for ternary / range / etc))
      # action_name : string
      # args : dict (name_string -> arg val)
      dprint ("add entry in %s"%self.name)
      key_hdl = self.create_key(key)
      if (action_name != None):
        succ, data_hdl = self.create_action(action_name, args)
        if (not succ):
          print ("WARNING: could not build action struct.")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return self, None
        dprint ("calling table_entry_add")
        retcode = cintf_table_add(key_hdl, data_hdl)
        if (retcode != 0):
          print ("WARNING: table_entry_add failed")
          self._cintf.table_key_deallocate(key_hdl)
          self._cintf.table_data_deallocate(data_hdl)
          return self, None
        else:
          dprint ("table_entry_add success")
          self._cintf.table_data_deallocate(data_hdl)
          return self, key_hdl
      else:
        dprint ("adding NO ACTION ENTRY")        
        succ, data_hdl = self.create_data(args)
        if (succ == False):
          print ("WARNING: could not build entry data struct.")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return self, None
        retcode = self.cintf_table_add(key_hdl, data_hdl)
        if (retcode != 0):
          print ("WARNING: table_entry_add failed")
          self._cintf.table_key_deallocate(key_hdl)        
          self._cintf.table_data_deallocate(data_hdl)        
          return self, None
        else:
          dprint ("table_entry_add success")
          self._cintf.table_data_deallocate(data_hdl)        
          return self, key_hdl
      raise (ValueError("Internal error: add_entry should not reach this point."))

    def del_entry(self, key_handle):
      """delete an entry by its key """
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
        tables[tbl_obj.name.decode('ascii')] = tbl_obj
      self.progs[name.decode('ascii')] = tables

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
      ("table_data_allocate", self.handle_type, (), self._driver.bf_rt_table_data_allocate)
    ]
    method_sigs = [
      ("key_field_set_value", self._driver.bf_rt_key_field_set_value),
      ("bf_rt_container_data_field_list_get", self._driver.bf_rt_container_data_field_list_get),
      ("data_field_set_value_ptr", self._driver.bf_rt_data_field_set_value_ptr),
      ("data_field_set_value", self._driver.bf_rt_data_field_set_value),
      ("table_key_deallocate", self._driver.bf_rt_table_key_deallocate),
      ("table_data_deallocate", self._driver.bf_rt_table_data_deallocate),
      ("data_field_set_value_array", self._driver.bf_rt_data_field_set_value_array),
      ("data_field_set_value_bool_array", self._driver.bf_rt_data_field_set_value_bool_array),
      ("table_entry_key_get", self._driver.bf_rt_table_entry_key_get)
    ]
    method_with_retcode = [
      ("table_entry_add", self._driver.bf_rt_table_entry_add),
      ("table_entry_del", self._driver.bf_rt_table_entry_del),
      ("table_entry_mod", self._driver.bf_rt_table_entry_mod)
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


def fill_c_byte_arr(py_value, size):
    n_bytes_ = (size + 7) // 8 # floor division
    value_type = c_ubyte * n_bytes_
    value = value_type()
    for i in range(0, n_bytes_):
        value[i] = (py_value >> 8 * (n_bytes_ - (i + 1))) & 0xFF
    return value, n_bytes_



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
import time
def array_get(ctlmgr, arr, idx):
  # wrap the controldriver.py array_get function to not require indices
  res = ctlmgr.array_get(arr, [idx])
  return res[0]

def poll_flowtable(ctlmgr, flow_ids, flow_cts, tbllen, pipe_id=1):
  flowtable = []
  for i in (range(0, tbllen)):
    fid = array_get(ctlmgr, flow_ids, i)[pipe_id]
    ct = array_get(ctlmgr, flow_cts, i)[pipe_id]
    flowtable.append((i, fid, ct))
  return flowtable

def print_flowtable(flowtable):
  print("---- non-zero flowtable entries ----")
  for idx, fid, ct in flowtable:
    if (fid != 0 and ct != 0):
      print("idx:"+str(idx)+", flow id: "+str(fid)+", pkt ct: "+str(ct))
  

def lucid_ctl(ctlmgr, globals_dict):
  # use the compiler-generated globals directory to get the p4 name of the arr_flow_ids array
  flow_ids = globals_dict["arr_flow_ids"]["compiled_name"]  
  flow_ids_len = globals_dict["arr_flow_ids"]["length"]
  flow_cts = globals_dict["arr_cts"]["compiled_name"]
  while True:
    flowtable = poll_flowtable(ctlmgr, flow_ids, flow_cts, flow_ids_len)
    print_flowtable(flowtable)
    time.sleep(1)

  
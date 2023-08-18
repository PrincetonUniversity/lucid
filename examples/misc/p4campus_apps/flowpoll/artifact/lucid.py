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

  
### lucid generated control plane. run inside of bfshell.

### static lucid-compiler generated code
import sys, os, subprocess, argparse, json

sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/libs")
from controldriver import *

def main():     
  # parsing                                                               
  parser = argparse.ArgumentParser()                                         
  parser.add_argument("--ports_up", help="set ports_up to true",action="store_true", default=(not is_model_running()))
  parser.add_argument("--globals_json", help="JSON file of global variables", default="globals.json")
  args = parser.parse_args()                                                 
  ports_up = args.ports_up                                                   
  globals_json_file = find_file_here(args.globals_json)                                                                            

  try:
    with open(globals_json_file, "r") as f:                                
      globals_dict = json.load(f)
      print("globals_dict:", globals_dict)
  except (FileNotFoundError, json.JSONDecodeError) as e:                     
    print(f"Error loading globals_json file: {e}")                         
    exit(1)                                                                

  # load the controller        
  clib_driver_so = find_clib_driver_file(dict(globals()))
  c = Controller(clib_driver_so)

  # bring ports up and add multicast nodes
  init(c, ports_up)
  # run custom controller
  if "lucid_ctl" in globals():                                                
    lucid_ctl(c, globals_dict)  
  # teardown
  c.close()
  print ('control startup complete.')  

def init(c, ports_up):
  if ports_up: 
    for port in ports:    
      c.port_up(*port)
  for mc_group in mc_groups:
    c.add_multicast_group(*mc_group)
# *)*)
# some helpers
def find_file(filename, directory):
  """
  Recursively searches for a file in a directory and its parent directories. 

  :param filename: The name of the file to search for.                       
  :param directory: The directory to start the search from.                  
  :return: The absolute path of the file if found, otherwise None.           
  """
  abs_directory = os.path.abspath(directory)
  file_path = os.path.join(abs_directory, filename)
  if os.path.isfile(file_path):
    return file_path
  parent_directory = os.path.dirname(abs_directory)
  if parent_directory == abs_directory:                                      
    # We have reached the root directory and haven't found the file        
    return None
  return find_file(filename, parent_directory)
def find_file_here(filename):
  return find_file(filename, os.path.dirname(os.path.abspath(__file__)))

def is_model_running():                                                        
  """                                                                        
  Checks if a process named "tofino-model" is running.                       
                                                                            
  :return: True if the process is running, otherwise False.                  
  """                                                                        
  try:                                                                       
    output = subprocess.check_output(["pgrep", "-f", "tofino-model"])      
    return len(output.strip().split(b"\n")) > 0                            
  except subprocess.CalledProcessError:                                      
    return False                       

port_speeds = {1 : (1 << 0), 10 : (1 << 1), 25 : (1 << 2), 40 : (1 << 3), 50 : (1 << 5), 100 : (1 << 6)}
fecs = {"BF_FEC_TYP_NONE": 0,
  "BF_FEC_TYP_FIRECODE": (1 << 0),
  "BF_FEC_TYP_REED_SOLOMON": (1 << 1),
  "BF_FEC_TYP_REED_SOLOMON_INTERLEAVED": (1 << 2),
  "BF_FEC_TYP_REED_SOLOMON_KL": (1 << 3),
  "BF_FEC_TYP_FC": (1 << 0),
  "BF_FEC_TYP_RS": (1 << 1),
  "BF_FEC_TYP_RS_IN": (1 << 2),
  "BF_FEC_TYP_RS_KL": (1 << 3)}
### dynamic compiler-generated definitions
ports = []
mc_groups = [(1000, [(128, 0), (129, 0), (130, 0), (131, 0)]), (1, [(196, 1)]), (2, [(196, 1), (196, 2)])]

main()

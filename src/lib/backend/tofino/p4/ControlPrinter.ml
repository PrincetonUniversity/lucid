(* Print the control plane elements of a Lucid program:
  - A (currently empty) C++ controller
  - A python control script that adds multicast groups and port up cmds
 *)

(* open Batteries *)
open Syntax
open SyntaxUtils
open Printing
open P4TofinoSyntax
open Str

exception Error of string
let error s = raise (Error s)

let comma_sep f ls = List.map f ls |> String.concat ", "

(* static code *)

let static_code = [%string {|
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
port_speeds = {speed:"BF_SPEED_%sG" % speed for speed in [1, 10, 25, 40, 50, 100]}
# port_speeds = {1 : (1 << 0), 10 : (1 << 1), 25 : (1 << 2), 40 : (1 << 3), 50 : (1 << 5), 100 : (1 << 6)}
fecs = {"BF_FEC_TYP_NONE": 0,
  "BF_FEC_TYP_FIRECODE": (1 << 0),
  "BF_FEC_TYP_REED_SOLOMON": (1 << 1),
  "BF_FEC_TYP_REED_SOLOMON_INTERLEAVED": (1 << 2),
  "BF_FEC_TYP_REED_SOLOMON_KL": (1 << 3),
  "BF_FEC_TYP_FC": (1 << 0),
  "BF_FEC_TYP_RS": (1 << 1),
  "BF_FEC_TYP_RS_IN": (1 << 2),
  "BF_FEC_TYP_RS_KL": (1 << 3)}
|}]
;;
(* end static_code *)

(* dynamic code -- port and multicast groups *)
let dynamic_code port_defs mc_groups = 
  let port_list port_defs =
    let port (dpid, speed) =
      [%string {|(%{dpid#Int}, port_speeds[%{speed#Int}])|}]
    in  
    [%string {|[%{comma_sep port port_defs}]|}]
  in
  let mc_group_list mc_groups = 
    let mc_group (mc_gid, mc_nodes) =
      let mc_node (dpid, rid) =
        [%string "(%{dpid#Int}, %{rid#Int})"]
      in
      [%string "(%{mc_gid#Int}, [%{comma_sep mc_node mc_nodes}])"]
    in
    [%string "[%{comma_sep mc_group mc_groups}]"]
  in
  [%string "### dynamic compiler-generated definitions\n"]
  ^[%string "ports = %{port_list port_defs}\n"]
  ^[%string "mc_groups = %{mc_group_list mc_groups}\n"]
;;

let read_file (filename : string) : string =                                   
  let ic = open_in filename in                                                 
  let len = in_channel_length ic in                                            
  let str = Bytes.create len in                                                
  really_input ic str 0 len;                                                   
  close_in ic;                                                                 
  Bytes.to_string str
;;

(* simple analysis of python control script -- make sure it defines a lucid_ctl function *)
let find_global_functions (input : string) : (string * string list) list =
  (* define regular expression for function definition with arguments *)
  let re_function_def = regexp "def +\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.*\\)):" in
  (* split input into lines and search for function definitions *)
  let lines = String.split_on_char '\n' input in
  let function_info = List.fold_left (fun acc line ->
    match search_forward re_function_def line 0 with
    | exception Not_found -> acc
    | _ ->
      (* extract function name and arguments from regular expression match *)
      let name = matched_group 1 line in
      let arg_list = matched_group 2 line in
      let args = Str.split (regexp ", *") arg_list in
      (name, args) :: acc
  ) [] lines in
  (* return list of function name and argument lists *)
  List.rev function_info
;;

let check_global_functions (required_functions : string list) (input : string) : bool =
  (* get list of global function names from input *)
  let global_functions = List.map fst (find_global_functions input) in
  (* check that all required functions are present in the global functions list *)
  List.for_all (fun required_function ->
    List.mem required_function global_functions
  ) required_functions
;;

let check_py_control input =
  if (check_global_functions ["lucid_ctl"] input)
  then ()
  else (error "control script error: the control script does not define a 'lucid_ctl' function.")
;;

let pyctl_of_prog tofino_prog user_control_fn_opt = 
  let mc_groups = mcgroups tofino_prog.control_config in
  let port_defs = ports   tofino_prog.control_config in
  let user_control = match (user_control_fn_opt) with
    | None -> ""
    | Some(fn) -> 
      print_endline ("including user control code from "^(fn));
      let user_control = read_file fn in 
      check_py_control user_control;
      "### user-provided control logic\n"^user_control
  in
  static_code
  ^(dynamic_code port_defs mc_groups)
  ^(user_control)
  ^"\nmain()\n"
;;

let cppctl_of_prog tofino_prog = 
  let _ = tofino_prog in
  ""
;;
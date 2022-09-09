(* Print the control plane elements of a Lucid program:
  - A (currently empty) C++ controller
  - A python control script that adds multicast groups and port up cmds
 *)
open Printf
open Format
open PPrint
open P4TofinoSyntax

exception Error of string
let error s = raise (Error s)

(* simple syntax for indented code lines *)
type srcline =
  | S of string
  | L of (srcline list) (* list of stmts *)
  | B of (srcline list) (* indented list *)
  | F of (string * (srcline list)) (* function with body indented *)
;;

let rec string_of_srcline n srcline : string =
  match srcline with
    | S s -> (String.make n ' ')^s
    | L pylines -> 
      List.map (string_of_srcline (n)) pylines
      |> String.concat "\n"
    | B pylines -> 
      List.map (string_of_srcline (n+2)) pylines
      |> String.concat "\n"
    | F(flabel, fbody) -> 
      ((String.make n ' ')^flabel)::
      (List.map (string_of_srcline (n+2)) fbody)
      |> String.concat "\n"
;;

let s str = S(str)
let l ls = L(ls)
let b ls = B(ls)
let f s ls = F(s, ls)

(*** string constructors ***)
let tup_int (a, b) = [%string "(%{a#Int}, %{b#Int})"]

let tup_int_list tups =
  [%string "[%{List.map tup_int tups |> String.concat \", \"}]"]
;;
let int_list ls =  
  [%string "[%{String.concat \", \" ls}]"]
  
;;

(*** python controller ***)
let py_header = l [
  s "import sys, os, time";
  s {|sys.path.append(os.path.dirname(os.path.realpath(__file__))+"/libs")|};
  s "from mgr import *";
  s "m = Manager()"
]

let py_footer = l [
  s "ports_up()";
  s "add_mc_nodes()";
  s "m.disconnect()"
]

let fcn_ports_up port_defs =
  f "def ports_up():" [
    f "if ((len(sys.argv) > 1) and (sys.argv[1] == 'ports_up')):" 
      (List.map 
        (fun (dpid, speed) -> 
          s [%string "m.port_up(%{dpid#Int}, pal_port_speed_t.BF_SPEED_%{speed#Int}G, pal_fec_type_t.BF_FEC_TYP_NONE)"]
        )
        port_defs
      )
    ]
;;

let fcn_add_mc_nodes node_defs =
  f "def add_mc_nodes():" [
    l (List.map (fun (mcid, replicas) -> 
        s [%string "m.add_multinode_mc_group(%{mcid#Int}, %{tup_int_list replicas})"])
       node_defs)
  ]
;;  

(* complete python manager script *)
let py_mgr port_defs node_defs = 
  l [
    py_header;
    fcn_ports_up port_defs;
    fcn_add_mc_nodes node_defs;
    py_footer
  ]
;;

(*** c controller -- just a shell for now ***)
let c_mgr = 
  l [
    s {|#include "libs/mgr.h"|};
    f "int main(int argc, char **argv) {" [
      s "start_switchd(argc, argv);";
      s "return join_switchd();"
    ];
    s "}"
  ]
;;

let pyctl_of_prog tofino_prog = 
  let mc_specs = mcgroups tofino_prog.control_config in 
  let port_specs = ports tofino_prog.control_config in 
  let py_str = py_mgr port_specs mc_specs |> string_of_srcline 0 in
  py_str
;;

let cppctl_of_prog tofino_prog = 
  let _ = tofino_prog in
  string_of_srcline 0 c_mgr 
;;

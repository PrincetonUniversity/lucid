(*** syntax for generating one-off p4 tables from 
       string identifiers and integer values. 
       mainly used for generating code from json 
     configuration blocks. ***)

module CL = Caml.List
open Printf
open Format
open Core
open MiscUtils

type field = string

type value =
  | VInt of int
  | VBool of bool
  | VName of string
  | VAny

type command = string

type action =
  { aname : string
  ; aparams : field list
  ; acmds : command list
  }

type pattern =
  { field : field
  ; value : value
  }

type guard = pattern list

type rule =
  { guard : guard
  ; action : action
  ; action_args : value list
  }

type ktype =
  | Exact
  | Ternary

type key =
  { kfield : field
  ; ktype : ktype
  }

type table =
  { tname : string
  ; tkeys : key list
  ; tactions : action list
  ; trules : rule list
  }

(* a bit of syntax for control flow printing *)
type bexpr =
  | Eq of string * string
  | Neq of string * string

type stmt =
  | TblCall of string
  | If of bexpr * stmt
  | IfElse of string * stmt * stmt
  | Seq of stmt * stmt

(* constructors *)

let param_to_p4field ev_name param =
  sprintf "%s.%s.%s" LLConstants.md_instance_prefix ev_name param
;;

let cassign dst src : command = sprintf "%s = %s;" dst src

let cmd_set dst i : command = 
  sprintf "%s = %i;" dst i
;;

let cmd_valid hdr_cid : command = 
  sprintf "%s.setValid();" hdr_cid
;;
let cmd_invalid hdr_cid : command = 
  sprintf "%s.setInvalid();" hdr_cid
;;

(* processing *)
let pattern_to_tup p = p.field, p.value
let tup_to_pattern (f, v) = { field = f; value = v }

let all_fields_of_rules rs =
  (* get a list of all fields used in all the rules. *)
  let fold_fields fields r =
    let new_fields = CL.map (fun pattern -> pattern.field) r.guard in
    fields @ new_fields
  in
  let all_fields = CL.fold_left fold_fields [] rs |> unique_list_of in
  all_fields
;;

let normalize_rules rs =
  (* update all rules so that they all test the same fields in the same order. *)
  let all_fields = all_fields_of_rules rs in
  let default_guard_tups = CL.map (fun field -> field, VAny) all_fields in
  (* update a rule so that it tests the default_guard, updated with 
     values for any indicated fields. *)
  let normalize_rule r =
    let guard_tups = CL.map pattern_to_tup r.guard in
    let replace_in tups (k, v) = replace_or_app (k, v) tups in
    let normalized_tups =
      CL.fold_left replace_in default_guard_tups guard_tups
    in
    let normalized_guard = CL.map tup_to_pattern normalized_tups in
    { r with guard = normalized_guard }
  in
  CL.map normalize_rule rs
;;

let actions_from_rules rules =
  CL.map (fun r -> r.action) rules |> unique_list_of
;;

(* assemble a table from a name 
   and a list of rules *)
let assemble_table name rules =
  let rules = normalize_rules rules in
  let keys =
    all_fields_of_rules rules
    |> CL.map (fun field -> { kfield = field; ktype = Ternary })
  in
  let table =
    { tname = name
    ; tkeys = keys
    ; tactions = actions_from_rules rules
    ; trules = rules
    }
  in
  table
;;

let actions_from_table t = actions_from_rules t.trules

(***** to string *****)

let indent_block s = "  " ^ Str.global_replace (Str.regexp "\n") "\n  " s
let field_to_string f = f

let fields_to_string (fs : field list) : string =
  CL.map field_to_string fs |> String.concat ~sep:", "
;;

let value_to_string v =
  match v with
  | VInt v -> string_of_int v
  | VBool v -> string_of_bool v
  | VName v -> v
  | VAny -> "_"
;;

let values_to_string vs = CL.map value_to_string vs |> String.concat ~sep:", "
let command_to_string c = c

let commands_to_string cs =
  CL.map command_to_string cs |> String.concat ~sep:"\n"
;;

let action_to_string a =
  sprintf
    "action %s(%s) {\n%s\n}"
    a.aname
    (fields_to_string a.aparams)
    (commands_to_string a.acmds |> indent_block)
;;

let actions_to_string a_s =
  CL.map action_to_string a_s |> String.concat ~sep:"\n"
;;

let action_name_to_string a = a.aname

let actions_names_to_string a_s =
  (CL.map action_name_to_string a_s |> String.concat ~sep:"; ") ^ ";"
;;

let pattern_value_to_string p = value_to_string p.value

let pattern_values_to_string ps =
  CL.map pattern_value_to_string ps |> String.concat ~sep:", "
;;

let rule_to_string r =
  "("
  ^ pattern_values_to_string r.guard
  ^ ") : "
  ^ r.action.aname
  ^ "("
  ^ ""
  ^ ");"
;;
let rules_to_string rs = CL.map rule_to_string rs |> String.concat ~sep:"\n"


(*** create and print control flow syntax tree ***)

let ktype_to_string m =
  match m with
  | Ternary -> "ternary"
  | Exact -> "exact"
;;

let key_to_string k =
  field_to_string k.kfield ^ " : " ^ ktype_to_string k.ktype ^ ";"
;;

let keys_to_string ks = CL.map key_to_string ks |> String.concat ~sep:"\n"

let table_to_string t =
  let key_string =
    "key = {\n" ^ (keys_to_string t.tkeys |> indent_block) ^ "\n}"
  in
  let actions_string =
    "actions = {\n"
    ^ (actions_names_to_string t.tactions |> indent_block)
    ^ "\n}"
  in
  let entries_string =
    "const entries = {\n" ^ (rules_to_string t.trules |> indent_block) ^ "\n}"
  in
  let body_string =
    indent_block key_string
    ^ "\n"
    ^ indent_block actions_string
    ^ "\n"
    ^ indent_block entries_string
  in
  sprintf "table %s {\n%s\n}" t.tname body_string
;;

let table_to_printer t = 
  let print_fcn t tbl_id = 
    let tbl_name = P4tPrint.str_of_private_oid tbl_id in 
    let actions_def_str = actions_to_string t.tactions in 
    let key_string = "key = {\n"^(keys_to_string t.tkeys |> indent_block)^"\n}" in 
    let actions_string = "actions = {\n"^(actions_names_to_string t.tactions |> indent_block)^"\n}" in 
    let entries_string = "const entries = {\n"^(rules_to_string t.trules |> indent_block)^"\n}" in 
    let body_string = (indent_block key_string)^"\n"^(indent_block actions_string)^"\n"^(indent_block entries_string) in     
    [%string "$actions_def_str\ntable $tbl_name {\n$body_string\n}"]
  in 
  (print_fcn t)
;;
(* convert the table into a nativeblock object in the Lucid backend IR. *)
let nativeblock_of_tbl location tid t = 
  LLSyntax.new_native_block tid location (table_to_printer t)
;; 


(*** create and print control flow syntax tree ***)

let scall t = TblCall t.tname
let sif bexp s = If (bexp, s)
let eqtest_to_bexpr lhs rhs = Eq (lhs, rhs)
let neqtest_to_bexpr lhs rhs = Neq (lhs, rhs)

let bexpr_to_string b =
  match b with
  | Eq (lhs, rhs) -> lhs ^ " == " ^ rhs
  | Neq (lhs, rhs) -> lhs ^ " != " ^ rhs
;;

let rec stmt_to_string s =
  match s with
  | TblCall t -> t ^ ".apply();"
  | If (c, s) -> "if (" ^ bexpr_to_string c ^ ") { " ^ stmt_to_string s ^ "}"
  | IfElse (c, s1, s2) ->
    "if ("
    ^ c
    ^ ") { "
    ^ stmt_to_string s1
    ^ "} else { "
    ^ stmt_to_string s2
    ^ "}"
  | Seq (s1, s2) -> stmt_to_string s1 ^ stmt_to_string s2
;;

let table_to_call_string t = t.tname ^ ".apply();"

let test_printers () =
  let test_action =
    { aname = "test_action"; aparams = []; acmds = ["do_foo()"] }
  in
  let test_guard = [{ field = "ip.src"; value = VInt 10 }] in
  let test_rule =
    { guard = test_guard; action = test_action; action_args = [] }
  in
  let test_table =
    { tname = "test_table"
    ; tkeys = [{ kfield = "ip.src"; ktype = Ternary }]
    ; tactions = [test_action]
    ; trules = [test_rule]
    }
  in
  print_endline "test action: ";
  print_endline (action_to_string test_action);
  print_endline "test_table: ";
  print_endline (table_to_string test_table)
;;

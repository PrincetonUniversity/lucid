open Batteries
open LLSyntax
open Format
open Base
open Consts
open PrintUtils
open DFSyntax
module CL = Caml.List

(* debug string functions *)

exception Error of string

let error s = raise (Error s)

(**** debug strings ****)
let mid_to_dbgstr (m : mid) : string =
  let names = Cid.names m in
  String.concat ~sep:"_" names
;;
let objcid_str = P4tPrint.str_of_private_oid ;;
let varcid_str = P4tPrint.str_of_varid ;;


let oper_to_dbgstr i =
  match i with
  | Const c -> Integer.value_string c
  | Meta m -> varcid_str m
  | MetaSlice(l, h, m) -> sprintf "%s[%i:%i]" (varcid_str m) l h
  | RegVar _ -> "<mem_cell>"
  | NoOper -> "<None>"
;;

let opers_to_dbgstr oper_list =
  Caml.String.concat ", " (Caml.List.map oper_to_dbgstr oper_list)
;;

let rec dbgstr_of_cid_list (args : mid list) =
  match args with
  | [] -> ""
  | [a] -> mid_to_dbgstr a
  | a :: args -> mid_to_dbgstr a ^ ", " ^ dbgstr_of_cid_list args
;;

let dbgstr_of_cidpairs cidpairs =
  Caml.String.concat
    ", "
    (CL.map
       (fun (s, d) -> sprintf "(%s, %s)" (varcid_str s) (varcid_str d))
       cidpairs)
;;

let str_of_cids cids =
  let names = Core.List.map cids ~f:varcid_str in
  Core.String.concat ~sep:", " names
;;

let dbgstr_of_cond c =
  match c with
  | Exact z -> string_of_int (Integer.to_int z)
  | Any -> "_"
;;

let dbgstr_of_pat_conditions pat =
  let pat_strs = Caml.List.map (fun (_, cond) -> dbgstr_of_cond cond) pat in
  "(" ^ Caml.String.concat ", " pat_strs ^ ")"
;;

let dbgstr_of_pat pat = 
  (* [x: 1; y: _; ] *)
  match pat with 
    | [] -> "<EMPTY PATTERN>"
    | _ -> 
      let dbgstr_of_field (m, c) = 
        sprintf "%s: %s" (varcid_str m) (dbgstr_of_cond c) 
      in 
      CL.map dbgstr_of_field pat |> String.concat ~sep:"; "
;;


let dbgstr_of_rule r =
  match r with
  | Match (_, pat, acn_id) ->
    (dbgstr_of_pat pat) ^ " --> " ^ objcid_str acn_id ^ "();"
  | OffPath pat -> dbgstr_of_pat pat ^ " : NOOP();"
;;

let cids_to_string cids =
  let names = Core.List.map cids ~f:Cid.to_string in
  Core.String.concat ~sep:", " names
;;

(* wrapper to debug functions that print to a formatter *)
let stringerize_1 pp_writer pp_writer_arg =
  pp_open_vbox str_formatter 4;
  pp_writer str_formatter pp_writer_arg;
  pp_close_box str_formatter ();
  flush_str_formatter ()
;;

let ids_in_cid_decls cid_decls = CL.split cid_decls |> fst |> str_of_cids

let str_to_id str =
  match BatString.split_on_char '~' str with
  | [name; num] -> name, int_of_string num
  | _ -> error "cannot convert into an id"
;;

let str_to_cid str =
  BatString.split_on_char '.' str |> CL.map str_to_id |> Cid.create_ids
;;

let cid_str_in_cid_decls cid_decls cidstr =
  Cid.exists cid_decls (str_to_cid cidstr)
;;

let str_of_decl decl =
  PrintUtils.open_block ();
  P4tPrint.PrintComputeObject.print_decls [decl];
  PrintUtils.close_block ()
;;

let str_of_decls decls =
  PrintUtils.open_block ();
  P4tPrint.PrintComputeObject.print_decls decls;
  PrintUtils.close_block ()
;;

let str_of_cid_decls cid_decls =
  PrintUtils.open_block ();
  P4tPrint.PrintComputeObject.print_decls (CL.split cid_decls |> snd);
  PrintUtils.close_block ()
;;

let rulescts_of_tables cid_decls = 
  let print_tbl_info (tbl) = 
    print_endline (sprintf "%s,%i" 
      (objcid_str (tid_of_tbl tbl))
      (CL.length (rules_of_table tbl))
    )
  in 
  print_endline ("**********");
  print_endline ("tbl, rules");
  (tbls_of_dmap cid_decls) |> CL.iter print_tbl_info;
  print_endline ("**********")
;;

let str_of_rules rules = 
  PrintUtils.open_block ();
  P4tPrint.PrintTable.print_tbl_rules str_formatter rules;
  PrintUtils.close_block ()
;;


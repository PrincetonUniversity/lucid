(* inline and remove all consts 
TODO: this doesn't yet support consts that have expressions 
containing other consts in them. *)

open Syntax
open Cid
open Batteries
module CL = Caml.List

let eliminate_consts ds = 
  (* get assoc list of consts:exprs *)
  let map_f dec = match dec.d with 
    | DConst(id, _, exp) -> Some (Cid.id id, exp)
    | _ -> None
  in 
  let consts = CL.filter_map map_f ds in 
  (* replace every const var exp with its value exp *)
  let v = object(_)
    inherit [_] s_map as super 
      method !visit_EVar _ cid = 
        match (Cid.lookup_opt consts cid) with 
          | None -> EVar(cid)
          | Some exp -> exp.e
    end
  in 
  let ds = v#visit_decls () ds in 
  (* delete the const declarations *)
  let filter_f dec = match dec.d with 
    | DConst(_, _, _) -> false | _ -> true
  in 
  let ds = CL.filter filter_f ds in 
  ds 
;;
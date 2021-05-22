(* pre-compute every non-immediate argument of a 
function or hash expression *) 
open Syntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List

(* 5/19 -- don't precompute arguments to event combinators. This is 
temporary to support event combinators inlined with a generate statement. *)
let exception_cids = [TofinoEvent.event_delay_cid; TofinoEvent.event_sslocate_cid; TofinoEvent.event_smlocate_cid]
;;

let precompute_args ds = 
  let v = object (_)
    inherit [_] s_map as super 

    val mutable pre_stmts : statement list = []
    method pre_stmts = pre_stmts
    method !visit_statement ctx statement = 
      let new_stmt = super#visit_statement ctx statement in 
      (* integrate any precompute statements generated 
      while transforming the tree. *)
      let all_stmts = pre_stmts @ [new_stmt] in 
      pre_stmts <- [];
      fold_stmts all_stmts

    method !visit_exp _ exp =     
      let map_f arg = 
        match (is_immediate arg) with 
          | true -> arg 
          | false -> (
            trans_info ("precomputing argument expression: "^(Printing.exp_to_string arg));
            let pre_stmt, new_arg = precompute arg in 
            pre_stmts <- pre_stmts @ [pre_stmt];
            new_arg 
          )
      in 
      match exp.e with 
        | EHash(size, args) -> (
          let new_args = CL.map map_f args in 
          {exp with e=EHash(size, new_args)}
        )
        | ECall(fcn_id, args) -> (
          match (CL.mem fcn_id exception_cids) with 
            | true -> exp (* skip event combinators *)
            | false -> 
              trans_info ("Pulling arguments out for: "^(Cid.to_string fcn_id));
              (* pull out the argument if it is not an immediate *)
              let new_args = CL.map map_f args in 
              {exp with e=ECall(fcn_id, new_args)}
        )
        | _ -> exp
      end
    in 
  v#visit_decls () ds 
;;
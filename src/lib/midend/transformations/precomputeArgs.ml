(* pre-compute every non-immediate argument of a
function or hash expression *)
(* 5/22 TODO: refactor this. Much of the functionality might be obsolete / incorrect. *)
open CoreSyntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List

(* 5/19 -- don't precompute arguments to event combinators. This is
temporary to support event combinators inlined with a generate statement. *)
let exception_cids =
  [ Cid.create ["Event";"delay"] ]
;;

let precompute_args ds =
  let v =
    object
      inherit [_] s_map as super
      val mutable pre_stmts : statement list = []
      method pre_stmts = pre_stmts

      method! visit_statement ctx statement =
        let new_stmt = super#visit_statement ctx statement in
        (* integrate any precompute statements generated
      while transforming the tree. *)
        let all_stmts = pre_stmts @ [new_stmt] in
        pre_stmts <- [];
        fold_stmts all_stmts

      method! visit_exp _ exp =

        (* precompute a single argument in a statement. 
          add the statement to the pre_statements and 
        return the new argument. *) 
        let precompute_arg arg =
          match is_immediate arg with
          | true -> arg
          | false ->
            let pre_stmt, new_arg = precompute arg in
            pre_stmts <- pre_stmts @ [pre_stmt];
            new_arg
        in
        match exp.e with
        | EHash (size, args) ->
          let new_args = CL.map precompute_arg args in
          { exp with e = EHash (size, new_args) }
        (* 5/22 -- The main point of this is to eliminate calls to builtins.. I think. *)
        | ECall (fcn_id, args) -> 
          (match CL.mem fcn_id exception_cids with
          | true -> exp (* skip event combinators *)
          | false ->
            (* pull out the argument if it is not an immediate *)
            let new_args = CL.map precompute_arg args in
            { exp with e = ECall (fcn_id, new_args) })
        | _ -> exp
    end
  in
  v#visit_decls () ds
;;

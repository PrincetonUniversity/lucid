(* pre-compute every non-immediate argument of a
function or hash expression *)
(* 5/22/22 TODO: refactor this. Much of the functionality might be obsolete / incorrect. *)
open CoreSyntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List

(* 5/19/22 -- don't precompute arguments to event combinators. This is
temporary to support event combinators inlined with a generate statement. *)
let exception_cids = [Cid.create ["Event"; "delay"]]

let array_method_cids = 
  let open InterpState in
  List.map 
    (fun (def: InterpState.State.global_fun ) -> def.cid) 
    (Arrays.defs@PairArrays.defs)
;;

let is_hash exp = 
  match exp.e with
  | EHash _ -> true
  | _ -> false 
;;

let precompute_args inline_array_addrs ds =
  let v =
    object
      inherit [_] s_map as super
      val mutable pre_stmts : statement list = []

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
        | ECall (fcn_id, args) 
            when (inline_array_addrs & (List.exists 
              (fun fcid -> Cid.equal_names fcn_id fcid)
              array_method_cids)) -> (
            (* array methods are special -- we allow hash expressions 
            in the index argument, which is always the 2nd arg (at pos 1) *)
            match args with
            | arr::addr::rest ->
              let arr' = precompute_arg arr in
              (* first, make sure the address exp itself is simplified *)
              (* now, the address exp can be either an immediate or a hash *)
              let addr' = super#visit_exp () addr in
              let addr' = if (is_immediate addr' or is_hash addr') 
                then addr' 
                else precompute_arg addr' 
              in
              (* print_endline ("addr': " ^ (CorePrinting.exp_to_string addr')); *)
              let rest' = CL.map precompute_arg rest in
              {exp with e = ECall (fcn_id, arr'::addr'::rest')}
            | _ -> error "[precompute_args] array method call with < 3 args"
        )        
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

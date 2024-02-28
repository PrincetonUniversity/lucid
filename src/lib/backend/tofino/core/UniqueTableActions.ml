(* Make per-table copies of each action *)

(* 
  Input form: 
    - no restrictions
  Output form:
    - each action is used by only 1 table

  1. build an actions context ctx and remove actions from program
  2. for each table t:
    for each action a in t's declaration:
      1. create a new action t_a with the body of a
      2. replace a with t_a in: 
        1. the declaration of t
        2. table_installs to t
 *)
open TofinoCore
open CoreSyntax

module IdMap = Collections.IdMap

type replace_ctx = {
  (* actionid -> definition *)
  actions : decl IdMap.t;
  (* tblid -> actionid -> newactionid *)
  local_renames : (id IdMap.t) IdMap.t;
}
let empty_ctx = { 
  actions = IdMap.empty;
  local_renames = IdMap.empty;
}
let add_action id decl ctx =
  {ctx with actions=IdMap.add id decl ctx.actions}
;;
let add_tableaction_renames tid aids new_aids ctx =
  let local_renames = List.fold_left2
    (fun map aid aid' -> IdMap.add aid aid' map)
    IdMap.empty
    aids
    new_aids
  in
  {ctx with local_renames=IdMap.add tid local_renames ctx.local_renames;}
;;
let get_renames tid ctx =
  IdMap.find tid ctx.local_renames
;;
let new_aid tid aid = (Id.concat_names "_" tid aid) ;;


(* rename actions in an expression *)
let rename_actionexp renames exp =
  let evar_replacer =
    object (_)
      inherit [_] s_map as super
      method! visit_EVar renames cid =
        match (IdMap.find_opt (Cid.to_id cid) renames) with
        | None -> EVar(cid)
        | Some(id') -> EVar(Cid.id id')
      end
  in
  evar_replacer#visit_exp renames exp
;;

(* create a table-local copy of an action *)
let copy_action ctx aid aid' =
  let acn_decl = IdMap.find aid ctx.actions in
  match acn_decl.d with
    | DActionConstr(acn_def) -> 
      {acn_decl with d=
        DActionConstr({acn_def with aid=aid';});}
    | _ -> error "[copy_action] found a non-action in the action map"
;;

(* rename actions in a statement *)
(* LEFT OFF HERE: eliminating table_install from IR *)
let rename_actionvars renames decl = 
  let evar_replacer =
    object (_)
      inherit [_] s_map as super

      (* replace the name of an action variable with 
         the name of the table-specific action variable.
         Use the table that appears with the action in 
         the call to the Table builtin method.
         This should only apply to Table.install 
         and its variants -- Table.create is processed 
         separately. *)

      method! visit_ECall renames cid exps b = 
        let tbl_exps = List.filter_map (fun exp -> if (Tables.is_tbl_ty exp.ety.raw_ty) then Some(exp) else None) exps in
        match tbl_exps with 
          | [] -> ECall(cid, exps, b)
          | [tbl_exp] -> (
            let local_renames = get_renames (id_of_exp tbl_exp) renames in
            (* for each argument expression, if it is an EVar(id) where id is in local_renames, then replace the id   *)
              let exps' = List.map 
              (fun exp -> 
                match exp.e with 
                | EVar(id) -> 
                  let id' = match (IdMap.find_opt (Cid.to_id id) local_renames) with
                    | None -> id
                    | Some(id') -> Cid.id id'                
                  in
                  {exp with e=EVar(id')}
                | _ -> exp
              )
              exps
            in
            ECall(cid, exps', b)
          )
          | _ -> error "[UniqueTableActions.rename_actionvars] reached a function call with multiple table type arguments"

        (* match tbl_ty with 
          | None -> ECall(cid, exps, b)
          | Some _ -> 
            (* need to figure out the table's name and which parameter is an action variable *)
            let (tbl, key, action_call) = match exps with 
              | [tbl; key; action_call] -> (tbl, key, action_call)
              | _ -> error "[rename_actionvars] expected 3 args to table_install"
            in
            let local_renames = get_renames (id_of_exp tbl) renames in
            let action_call = match action_call.e with 
              | ECall(aid, args, b) -> 
                let id = Cid.to_id aid in
                let id' = IdMap.find id local_renames in
                {action_call with e=ECall(Cid.id id', args, b)}
              | _ -> error "[rename_actionvars] expected action call in table_install"
            in
            ECall(cid, [tbl; key; action_call], b) *)
      (* method! visit_STableInstall renames tbl entries =
        let local_renames = get_renames (id_of_exp tbl) renames in
        let entries' = List.map 
          (fun entry -> {entry with
            eaction = IdMap.find (entry.eaction) local_renames;})
          entries
        in 
        STableInstall(tbl, entries') *)
      end
  in
  evar_replacer#visit_decl renames decl
;;

let update_decl ctx decl : replace_ctx * decls =
  match decl.d with
  (* actions -- add action to context and remove from prog *)
  | DActionConstr({aid=aid; _}) -> 
    add_action aid decl ctx, []
  (* table constructors -- create bindings, update context, update local actions *)
  | DGlobal(tid, tty, econstr) ->
    let tdef = Tables.dglobal_params_to_tbl_def tid econstr in  
    let aids = List.map id_of_exp tdef.tactions in
    let new_aids = List.map (new_aid tid) aids in
    let ctx = add_tableaction_renames tid aids new_aids ctx in
    let tdefault' = 
      ( IdMap.find (Cid.to_id (fst tdef.tdefault)) (get_renames tid ctx) |> Cid.id
      , snd tdef.tdefault)
    in
    let tactions' = 
      List.map (rename_actionexp (get_renames tid ctx)) tdef.tactions
    in
    (* new table definition *)
    let tdef' = {tdef with 
      tactions = tactions';
      tdefault = tdefault';}
    in
    (* new action definitions -- copies of original actions, 
       with local names.*)
    let actions = List.map2 (copy_action ctx) aids new_aids in
    (* let d' = DGlobal(tid, tty, {e=ETableCreate(tdef'); ety=tty; espan=decl.espan}) in *)
    let econstr' = Tables.tbl_def_to_econstr tdef' in
    let exp_constr = {econstr with e = econstr'.e}in
    let d' = DGlobal(tid, tty, exp_constr) in
    ctx, actions@[{decl with d=d'}]
  (* everything else -- rename action vars wherever they appear (table_install, mainly) *)
  | _ -> 
    ctx, [rename_actionvars ctx decl]
;;

let process ds = 
  List.fold_left 
    (fun (ctx, new_ds) d -> 
      let ctx', d' = update_decl ctx d in
      ctx', new_ds@d')
    (empty_ctx, [])
    ds
  |> snd
;;
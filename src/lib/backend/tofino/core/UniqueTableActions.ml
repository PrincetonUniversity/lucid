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
let rename_actionvars renames decl = 
  let evar_replacer =
    object (_)
      inherit [_] s_map as super
      method! visit_STableInstall renames tbl entries =
        let local_renames = get_renames (id_of_exp tbl) renames in
        let entries' = List.map 
          (fun entry -> {entry with
            eaction = IdMap.find (entry.eaction) local_renames;})
          entries
        in 
        STableInstall(tbl, entries')
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
  | DGlobal(tid, tty, {e=ETableCreate(tdef); ety=ety; espan=espan;}) -> 
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
    ctx, actions@[{decl with d=DGlobal(tid, tty, {e=ETableCreate(tdef'); ety; espan})}]
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
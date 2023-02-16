open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Remove tuples from the program by turning their entries into individual
   variables. Assumes records and vectors (the other recursive types)
   have already been removed, so a type contains a tuple if and only if
   it is itself a tuple. *)

let extract_etuple e =
  match e.e with
  | ETuple es -> es
  | _ ->
    Console.error
    @@ "Internal error (extract_etuple): "
    ^ Printing.exp_to_string e
;;

(* Given an id and a list of types, create a fresh id for each type *)
let rename_elements id tys =
  List.mapi
    (fun i rty -> Id.fresh (Id.name id ^ "_" ^ string_of_int i), ty rty)
    tys
;;

let sequence_statements ss =
  let seq = List.fold_left sseq snoop ss in
  seq.s
;;

(* Maps tuple variable names to the list of variables represening
   their components. *)
type env = (id * ty) list IdMap.t

let flatten_params env params =
  let env = ref env in
  let rec aux params =
    List.flatten
    @@ List.map
         (fun (id, ty) ->
           match ty.raw_ty with
           | TTuple tys ->
             let new_params = rename_elements id tys in
             env := IdMap.add id new_params !env;
             aux new_params
           | _ -> [id, ty])
         params
  in
  let new_params = aux params in
  !env, new_params
;;


(* completely flatten all the tuples in a type, 
   so that a TTuple may only appear in the 
   outermost raw type. *)
let flatten_ty ty = 
  let flattener = 
    object (self)
      inherit [_] s_map as super 
      method! visit_raw_ty ctx raw_ty =
        match raw_ty with
        | TTuple(inner_tys) ->
          (* flatten inner types *)
          let flat_inner_tys = List.map (self#visit_raw_ty ctx) inner_tys in
          (* concat together *)
          let flat_inner_tys = List.map
            (fun raw_ty -> 
              match raw_ty with
              | TTuple(inner_tys) -> inner_tys
              | raw_ty -> [raw_ty])
            flat_inner_tys
            |> List.flatten
          in
          (* return flat tuple *) 
          TTuple(flat_inner_tys)
        | raw_ty ->
          (* transform inner *)
          super#visit_raw_ty ctx raw_ty    
      end
    in
    flattener#visit_ty () ty
;;

(* flatten a list of types to remove all TTuples *) 
let flatten_tys tys = 
  List.fold_left
    (fun tys ty -> 
    match (flatten_ty ty).raw_ty with
    | TTuple(raw_tys) -> tys@(List.map Syntax.ty raw_tys)
    | _ -> tys@[ty])
  []
  tys
;;

let replacer =
  object (self)
    inherit [_] s_map as super

    (* Table extensions -- types *)
    method! visit_TTable _ tbl_ty = 
      (* flatten param and return types *)    
      let tbl_ty' = { tbl_ty with 
        tparam_tys = flatten_tys tbl_ty.tparam_tys;
        tret_tys = flatten_tys tbl_ty.tret_tys; }
      in 
      TTable(tbl_ty')

    method! visit_TAction _ acn_ty =
      let acn_ty' = {
        aconst_param_tys = flatten_tys acn_ty.aconst_param_tys;
        aparam_tys = flatten_tys acn_ty.aparam_tys;
        aret_tys = flatten_tys acn_ty.aret_tys; } 
      in 
      TAction(acn_ty')
    
    (* Table extensions -- statements *)
    method! visit_STableMatch env tblmatch =
      (* recurse on inner components *)
      let tblmatch = self#visit_tbl_match env tblmatch in
      match tblmatch.out_tys with
      (* the match table creates new variables, which 
         we must flatten *)
      | Some (out_tys) -> (
        let var_defs = List.combine tblmatch.outs out_tys in
        let env', new_var_defs = flatten_params !env var_defs in
        let outs', out_tys' = List.split new_var_defs in
        (* update the environment with the new ids *)
        env := env';
        (* return apply table statement with updates *)
        STableMatch({tblmatch with outs=outs'; out_tys=Some(out_tys');})
      )
      | None -> 
        (* the match table writes existing variables, we must 
           find their flattened id *)
        let rec lookup_flat_ids id : id list =
          match (IdMap.find_opt id !env) with 
          | None -> 
             [id] (* not a tuple *)
          | Some ids_tys -> 
            List.map lookup_flat_ids (List.split ids_tys |> fst)
            |> List.flatten
        in 
        let outs' = List.map lookup_flat_ids tblmatch.outs |> List.flatten in        
        STableMatch({tblmatch with outs=outs';})
    method! visit_ETableMatch _ tblmatch =
      Console.error_position
        tblmatch.tbl.espan
        "Table apply expressions should be converted to statements before tuple elim."

    (* Split into a bunch of variable definitions, one for each
       tuple element. *)
    method! visit_SLocal env id ty exp =
      match (Option.get exp.ety).raw_ty with
      | TTuple tys ->
        let entries = self#visit_exp env exp |> extract_etuple in
        let new_ids = rename_elements id tys in
        let new_defs =
          List.map2
            (fun (id, ty) e ->
              (* Recursively split up each entry *)
              let recursive_defs = self#visit_SLocal env id ty e in
              statement recursive_defs)
            new_ids
            entries
        in
        env := IdMap.add id new_ids !env;
        sequence_statements new_defs
      | _ -> super#visit_SLocal env id ty exp

    (* Like SLocal, but we look up the new variable names *)
    method! visit_SAssign env id exp =
      match (Option.get exp.ety).raw_ty with
      | TTuple _ ->
        let entries = self#visit_exp env exp |> extract_etuple in
        let new_ids = IdMap.find id !env in
        let new_defs =
          List.map2
            (fun (id, _) e ->
              let recursive_defs = self#visit_SAssign env id e in
              statement recursive_defs)
            new_ids
            entries
        in
        sequence_statements new_defs
      | _ -> super#visit_SAssign env id exp

    (* Split up variable expressions into tuple expressions, where
       each component is a variable from the environment *)
    method! visit_EVar env cid =
      match cid with
      | Compound _ -> EVar cid
      | Id id ->
        (match IdMap.find_opt id !env with
        | None -> EVar cid
        | Some lst ->
          ETuple
            (List.map
               (fun (id, ty) ->
                 { (var_sp (Id id) Span.default) with ety = Some ty })
               lst))

    method! visit_EOp env op args =
      match op, args with
      | TGet (_, idx), [e] ->
        let es = self#visit_exp env e |> extract_etuple in
        let e = List.nth es idx |> self#visit_exp env in
        e.e
      | _ -> super#visit_EOp env op args

    (** Now we have to visit all expressions which might contain tuples,
        so we can eliminate them in the appropriate way *)

    (* This case is unlikely but possible *)
    method! visit_SUnit env exp =
      match (Option.get exp.ety).raw_ty with
      | TTuple _ ->
        let entries = self#visit_exp env exp |> extract_etuple in
        let new_defs =
          List.map
            (fun e ->
              let recursive_defs = self#visit_SUnit env e in
              statement recursive_defs)
            entries
        in
        sequence_statements new_defs
      | _ -> super#visit_SUnit env exp

    (* Given a (possibly nested) tuple-type expression, turn it into
       a single unnested tuple expression with all elements in order *)
    method flatten env exp =
      match (Option.get exp.ety).raw_ty with
      | TTuple _ ->
        let es = self#visit_exp env exp |> extract_etuple in
        List.map (self#flatten env) es |> List.concat
      | _ -> [self#visit_exp env exp]

    (* We replace tuple-type parameters to functions and events with
       one parameter for each tuple entry. So we need to adjust the
       arguments at the call site as well *)
    method! visit_ECall env cid args =
      let args = List.map (self#flatten env) args |> List.concat in
      ECall (cid, args)

    (* Same as ECall *)
    method! visit_EHash env sz args =
      let args = List.map (self#flatten env) args |> List.concat in
      EHash (sz, args)
  end
;;

let flatten env e = replacer#flatten (ref env) e
let replace_exp env exp = replacer#visit_exp (ref env) exp
let replace_statement env s = replacer#visit_statement (ref env) s
let replace_ty ty = replacer#visit_ty (ref IdMap.empty) ty

let rec replace_decl (env : env) d =
  match d.d with
  | DConst (id, { raw_ty = TTuple tys }, exp) ->
    let es = replace_exp env exp |> extract_etuple in
    let new_ids = rename_elements id tys in
    let env = IdMap.add id new_ids env in
    let new_ds =
      List.map2
        (fun (id, ty) exp -> { d with d = DConst (id, ty, exp) })
        new_ids
        es
    in
    replace_decls env new_ds
  | DGlobal (id, ty, exp) -> (
      match ty.raw_ty with
      (* Same as DConst, except we might generate either a DConst or
         a DGlobal for each component *)
      | TTuple tys -> 
        let es = replace_exp env exp |> extract_etuple in
        let new_ids = rename_elements id tys in
        let env = IdMap.add id new_ids env in
        let new_ds =
          List.map2
            (fun (id, ty) exp ->
              if is_global ty
              then { d with d = DGlobal (id, ty, exp) }
              else { d with d = DConst (id, ty, exp) })
            new_ids
            es
        in
        replace_decls env new_ds
      (* The tuple types inside of a table types must be flattened *)
      | TTable(_) ->
        env, [{ d with d = DGlobal(id, replace_ty ty, replace_exp env exp)}]
      | _ -> env, [d])
  | DEvent (id, sort, _, params) ->
    let _, new_params = flatten_params env params in
    env, [{ d with d = DEvent (id, sort, [], new_params) }]
  | DHandler (id, sort, (params, body)) ->
    let body_env, new_params = flatten_params env params in
    let body = replace_statement body_env body in
    env, [{ d with d = DHandler (id, sort, (new_params, body)) }]
  | DSize _ | DMemop _ | DExtern _ | DSymbolic _ | DConst _ ->
    env, [d]
  | DAction(id, tys, const_params, (params, action_body)) -> 
    let tys' = flatten_tys tys in 
    let body_env, const_params' = flatten_params env const_params in
    let body_env, params' = flatten_params body_env params in
    (* Flatten all tuples in action body *)
    let action_body' = List.map (flatten body_env) action_body |> List.flatten in 
    env, [{d with d=DAction(id, tys', const_params', (params', action_body'))}]
  | DFun _ | DConstr _ | DModule _ | DUserTy _ | DModuleAlias _ ->
    Console.error_position
      d.dspan
      "Modules, records and functions should be eliminated before tuple \
       elimination."

and replace_decls env ds =
  let env, ds =
    List.fold_left
      (fun (env, ds) d ->
        let env', ds' = replace_decl env d in
        env', ds' :: ds)
      (env, [])
      ds
  in
  env, List.flatten (List.rev ds)
;;

(* Sanity checker to make sure no tuples remain in the program *)
let checker =
  object
    inherit [_] s_iter

    method! visit_exp _ e =
      match e.e with
      | ETuple _ ->
        Console.error_position e.espan
        @@ "Internal error: failed to eliminate tuple "
        ^ Printing.exp_to_string e
      | _ -> ()
  end
;;

let print_prog ds =
  let str = Printing.decls_to_string ds in
  Console.report str 
;;

let eliminate_prog ds =
  let _, ds = replace_decls IdMap.empty ds in
  checker#visit_decls () ds;
  ds
;;

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

(* this is what we need for tables that create new vars *)
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

let replacer =
  object (self)
    inherit [_] s_map as super

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
  | DGlobal (id, { raw_ty = TTuple tys }, exp) ->
    (* Same as DConst, except we might generate either a DConst or
       a DGlobal for each component *)
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
  | DEvent (id, sort, _, params) ->
    let _, new_params = flatten_params env params in
    env, [{ d with d = DEvent (id, sort, [], new_params) }]
  | DHandler (id, (params, body)) ->
    let body_env, new_params = flatten_params env params in
    let body = replace_statement body_env body in
    env, [{ d with d = DHandler (id, (new_params, body)) }]
  | DSize _ | DMemop _ | DExtern _ | DSymbolic _ | DConst _ | DGlobal _ ->
    env, [d]
  | DAction(id, ty, const_params, (params, action_body)) -> 
    let body_env, const_params' = flatten_params env const_params in
    let body_env, params' = flatten_params body_env params in
    (* Flatten all tuples in action body *)
    let action_body' = List.map (flatten body_env) action_body |> List.flatten in 
    env, [{d with d=DAction(id, ty, const_params', (params', action_body'))}]
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


(* Do all the tuple elimination for tables and actions *)
let table_tuple_eliminator env ds =
  let eliminator =
    object (self)
      inherit [_] s_map as super

      method! visit_exp do_flatten exp =
        if (do_flatten)
        then (
          match exp.e with
          | ETuple(exps) -> 
            let flat_inner_exps = List.fold_left
              (fun flat_exps exp -> 
                let flat_exp = self#visit_exp do_flatten exp in
                match flat_exp.e with
                  ETuple(exps) -> flat_exps@exps
                  | _ -> flat_exps@[flat_exp])
              []
              exps
            in
            {exp with e=ETuple(flat_inner_exps)}
          | _ -> super#visit_exp do_flatten exp
        )
        else (exp)

      method! visit_raw_ty do_flatten raw_ty =
        if (do_flatten)
        then (
          match raw_ty with
          | TTuple(inner_tys) ->
            (* flatten inner types *)
            let flat_inner_tys = List.map (self#visit_raw_ty do_flatten) inner_tys in
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
            super#visit_raw_ty do_flatten raw_ty
        )
        else (raw_ty)

      (* flatten a list of raw types. Example: 
          [ Tup(Int, Tup(Bool, Bool)), Tup(Int, Bool)]
          --> 
          [ Int, Bool, Bool, Int, Bool ] *)
      method flatten_tys tys =
        (* a list of raw types where all tuples are flattened *)
        let flat_tys = List.map (self#visit_ty true) tys in
        List.map 
          (fun ty -> 
            match ty.raw_ty with
            | TTuple(inner_rawtys) -> List.map (fun raw_ty -> ty_sp raw_ty ty.tspan) inner_rawtys
            | _ -> [ty_sp ty.raw_ty ty.tspan])
          flat_tys
        |> List.concat
      (* method flatten_tys tys = *)

      (* flatten a list of expressions, which may contain tuples *)
      method flatten_exps exps = 
        let flat_exps = List.map (self#visit_exp true) exps in
        List.map
          (fun exp -> match exp.e with
            | ETuple(exps) -> exps
            | _ -> [exp])
          flat_exps
        |> List.concat 

      (* types *)
      method! visit_T_Table _ tbl_ty = 
        (* flatten param and return types *)    
        let tparam_tys = self#flatten_tys tbl_ty.tparam_tys in                
        let tret_tys = self#flatten_tys tbl_ty.tret_tys in        
        T_Table({tbl_ty with tparam_tys; tret_tys;})

      method! visit_TAction _ acn_ty = 
        let aconst_param_tys = self#flatten_tys acn_ty.aconst_param_tys in                
        let aparam_tys = self#flatten_tys acn_ty.aparam_tys in        
        let aret_tys = self#flatten_tys acn_ty.aret_tys in        
        TAction({aconst_param_tys; aparam_tys; aret_tys})
      
      (* expressions (should be eliminated by now) *)
      method! visit_ETableApply _ _ = 
        error "[TupleElimination.table_tuple_eliminator] table expressions (ETableApply) should
        have been eliminated by now.";

      (* flatten a list of ids that have not been bound *)
      (* method! flatten_unbound_ids ids tys = *)



      (* method! visit_ApplyTable dummy tbl_match = *)
        (* everything will be taken care of automatically, except the ids *)





      end
  in

  let new_decls = eliminator#visit_decls false ds in
  env, new_decls
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
  print_endline ("---- after main tuple elimination ----");
  print_endline "decls: ";
  let str = Printing.decls_to_string ds in
  Console.report str 
;;

let eliminate_prog ds =
  let env, ds = replace_decls IdMap.empty ds in
  print_prog ds;
  print_endline ("starting tuple elimination in table constructs. ");
  let _, ds = table_tuple_eliminator env ds in
  exit 1;
  checker#visit_decls () ds;
  ds
  (* let _ = env in  *)
  (* checker#visit_decls () ds; *)
  (* ds *)
;;

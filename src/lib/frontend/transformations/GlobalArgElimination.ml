open Batteries
open Syntax
open SyntaxUtils
open Collections

(* I want to use a map here (hence the name), but I couldn't find a total order
   which behaved the way I wanted (e.g. such that two types were equal if the
   equiv function below returned true *)
module GMap = struct
  type t = (id * ty) list

  let empty : t = []
  let add x t = x :: t

  let equiv ty1 ty2 =
    let ty1 = (normalizer ())#visit_ty () ty1 in
    let ty2 = (normalizer ())#visit_ty () ty2 in
    equiv_ty ~ignore_effects:true ~qvars_wild:true ty1 ty2
  ;;

  let find_matching ty t = List.filter (fun (_, ty2) -> equiv ty ty2) t
end

(* Useful little list function *)
let rec partition_map (f : 'a -> 'b option) (lst : 'a list) : 'b list * 'a list =
  match lst with
  | [] -> [], []
  | hd :: tl ->
    let bs, ays = partition_map f tl in
    (match f hd with
    | None -> bs, hd :: ays
    | Some b -> b :: bs, ays)
;;

(* Maps to tell us the effect of each global id, and the ids & concrete types
   of all the globals with a particular general type *)
type global_info = int IdMap.t * GMap.t

let collect_globals ds : global_info =
  let gs =
    List.filter_map
      (function
        | { d = DGlobal (id, ty, _) } -> Some (id, ty)
        | _ -> None)
      ds
  in
  let start_id = Id.create "start" in
  List.fold_lefti
    (fun (idmap, gmap) i (id, gty) ->
      IdMap.add id i idmap, GMap.add (id, gty) gmap)
    (IdMap.singleton start_id 0, GMap.empty)
    gs
;;

(* Maps variables in a constraint to global variable ids *)
type inst = (id * ty) IdMap.t

let possible_instantiations ((effect_map, gmap) : global_info) spec gparams =
  let get_effect (inst : inst) id =
    let id =
      match id with
      | Id id -> id
      | Compound _ ->
        failwith "Event Duplication must be run after module elimination"
    in
    let mapped_id =
      match IdMap.find_opt id inst with
      | Some (mid, _) -> mid
      | None -> id
    in
    IdMap.find mapped_id effect_map
  in
  let satisfies_spec inst =
    List.for_all
      (function
        | CSpec lst ->
          fst
          @@ List.fold_left
               (fun (b, prev) curr ->
                 if not b
                 then false, curr
                 else (
                   let left, right =
                     get_effect inst (fst prev), get_effect inst (fst curr)
                   in
                   match snd prev with
                   | SpecLess -> left < right, curr
                   | SpecLeq -> left <= right, curr))
               (true, List.hd lst)
               (List.tl lst)
        | CEnd _ -> true)
      spec
  in
  let rec all_insts lst =
    match lst with
    | [] -> [IdMap.empty]
    | (arg_id, possible_gids) :: tl ->
      let tl_insts = all_insts tl in
      List.concat
      @@ List.map
           (fun inst ->
             List.map (fun poss -> IdMap.add arg_id poss inst) possible_gids)
           tl_insts
  in
  (* Each index contains every possible instantiation for that index argument *)
  let possible_gids =
    List.map
      (fun (arg_id, arg_ty) -> arg_id, GMap.find_matching arg_ty gmap)
      gparams
  in
  let possible_insts = all_insts possible_gids in
  List.filter satisfies_spec possible_insts
;;

type event_mapping =
  { inst : inst (* An instantiation of the event *)
  ; new_id : id (* The event id corresponding to this instantiation *)
  ; params : params
        (* The original parameters of the event, with any polymorphism eliminated *)
  }

(* Given the list of existing globals and an event declaration, return several
   copies of the declaration, one for each possible combination of globals
   that could be passed in. Also return the instantiation that led to each
   copy *)
let duplicate_event_decl global_info (eid, constr_specs, params)
    : event_mapping list
  =
  let gparams = List.filter (is_global % snd) params in
  let possibles = possible_instantiations global_info constr_specs gparams in
  let new_id inst =
    (* Don't rename if there were no global args *)
    if List.length gparams = 0
    then eid
    else
      Id.fresh
      @@ Id.name eid
      ^ List.fold_left
          (fun acc (id, _) -> acc ^ "_" ^ Id.name (fst @@ IdMap.find id inst))
          ""
          gparams
  in
  List.map
    (fun inst ->
      (* Unify each inst to remove any QVars from the type *)
      let maps = TyperInstGen.fresh_maps () in
      let params = TyperInstGen.instantiator#visit_params maps params in
      let gparams = List.filter (is_global % snd) params in
      List.iter
        (fun (id, ty) ->
          TyperUnify.unify_ty ty.tspan ty (snd @@ IdMap.find id inst))
        gparams;
      let params = TyperInstGen.generalizer#visit_params () params in
      { inst; new_id = new_id inst; params })
    possibles
;;

(* Maps events to the list of their replacement events *)
let duplicate_all_events global_info ds : event_mapping list IdMap.t =
  List.fold_left
    (fun acc d ->
      match d.d with
      | DEvent (eid, _, spec, params) ->
        IdMap.add eid (duplicate_event_decl global_info (eid, spec, params)) acc
      | _ -> acc)
    IdMap.empty
    ds
;;

(* Go through the body and substitute in inst's value for each global-typed
   parameter *)
let add_param_defs (inst : inst) handler_params event_params body =
  let subst_map =
    List.fold_left2
      (fun acc (arg_id, ty) (orig_id, _) ->
        (* arg_id is the name used in the handler's definition; orig_id is
           the name used in the event declaration *)
        if is_global ty
        then IdMap.add arg_id (EVar (Id (fst @@ IdMap.find orig_id inst))) acc
        else acc)
      IdMap.empty
      handler_params
      event_params
  in
  FunctionInlining.subst#visit_statement subst_map body
;;

let filter_params params =
  List.filter (fun (_, ty) -> not (is_global ty)) params
;;

(* Return a new handler corresponding to the given instantiation, with a new id,
   new arguments, and all globals in the inst substituted into the body *)
let update_handler span handler_body { inst; new_id; params } =
  (* Instantiate both parameters so we can unify them; this ensures we update
     any type annotations in the body *)
  let handler_params, body =
    TyperInstGen.instantiator#visit_body
      (TyperInstGen.fresh_maps ())
      handler_body
  in
  let params =
    TyperInstGen.instantiator#visit_params (TyperInstGen.fresh_maps ()) params
  in
  let new_params =
    (* Use id from the handler param, but type from the instantiated event *)
    List.map2
      (fun (id, hty) (_, ety) ->
        TyperUnify.unify_ty hty.tspan hty ety;
        id, ety)
      handler_params
      params
    |> filter_params
  in
  let body' = add_param_defs inst handler_params params body in
  let new_handler_body =
    TyperInstGen.generalizer#visit_body () (new_params, body')
  in
  decl_sp (DHandler (new_id, new_handler_body)) span
;;

let replace_decls (emap : event_mapping list IdMap.t) ds =
  let replace_decl d =
    match d.d with
    | DEvent (eid, sort, _, _) ->
      begin
        match IdMap.find_opt eid emap with
        | Some lst ->
          List.map
            (fun { new_id; params } ->
              decl_sp (DEvent (new_id, sort, [], filter_params params)) d.dspan)
            lst
        | None -> failwith "Impossible. I hope."
      end
    | DHandler (eid, body) ->
      begin
        match IdMap.find_opt eid emap with
        | Some lst -> List.map (update_handler d.dspan body) lst
        | None -> failwith "Impossible. I hope."
      end
    | _ -> [d]
  in
  List.concat @@ List.map replace_decl ds
;;

(* Replace instances of the old event name with the new one. This is currently
   very basic: it only does the replacement if the arguments are literal global
   ids. We can make it smarter later if we feel the need *)
let replace_uses (event_mappings : event_mapping list IdMap.t) ds =
  let v =
    object (self)
      inherit [_] s_map as super

      method! visit_ECall dummy cid args =
        match cid with
        (* At least for now, events never have Compound ids *)
        | Compound _ -> ECall (cid, List.map (self#visit_exp dummy) args)
        | Id id ->
          (match IdMap.find_opt id event_mappings with
          (* Check to see if we've replaced this event *)
          | None -> ECall (cid, List.map (self#visit_exp dummy) args)
          | Some mappings ->
            let global_arg_values, other_args =
              partition_map
                (fun e ->
                  if is_global (Option.get e.ety)
                  then begin
                    match e.e with
                    | EVar cid -> Some (Cid.to_id cid)
                    | _ ->
                      Console.error
                      @@ "This expression doesn't look like a global id: "
                      ^ Printing.exp_to_string e
                  end
                  else None)
                args
            in
            (* It's rather hacky to be doing this via string manipulation; a
               better alternative (which I'm too lazy to implement right now)
               would be to construct the inst we get from this call, and search
               through the list for an equivalent one. But names should be
               deterministic, so I expect this implementation won't cause problems
               for a long time, if ever *)
            let expected_name =
              List.fold_left
                (fun acc id -> acc ^ "_" ^ Id.name id)
                (Id.name id)
                global_arg_values
            in
            let new_id =
              List.find_map
                (fun { new_id } ->
                  if Id.name new_id = expected_name then Some new_id else None)
                mappings
            in
            ECall (Id new_id, List.map (self#visit_exp dummy) other_args))
    end
  in
  v#visit_decls () ds
;;

let mapping_to_string m =
  idmap_to_string
    (fun lst ->
      Printing.list_to_string
        (fun { inst; new_id; params } ->
          Printf.sprintf
            "inst: %s, %s(%s)\n"
            (idmap_to_string (Id.to_string % fst) inst)
            (Id.to_string new_id)
            (Printing.comma_sep
               (fun (id, ty) ->
                 Printing.ty_to_string ty ^ " " ^ Id.to_string id)
               params))
        lst)
    m
;;

(* Optimization: Go through one last time and remove the declaration/handler
   for any events which aren't actually generated, to minimize bloat. Only do
   this for events which once had a global argument, though, so the user can
   still generate other events via the interpreter spec file. *)
let deduplicate (event_mapping : event_mapping list IdMap.t) ds =
  (* Strategy: Every event value must be created at some point through an ECall,
     so just collect the name of every function that's called and remove any
     event/handler definitions which don't match. *)
  let didn't_duplicate =
    IdMap.fold
      (fun _ renamings acc ->
        (* The entries with singleton lists correspond to all the events we
            _didn't_ make multiple copies of. *)
        match renamings with
        | [{ new_id }] -> CidSet.add (Id new_id) acc
        | _ -> acc)
      event_mapping
      CidSet.empty
  in
  let don't_remove = ref didn't_duplicate in
  let v =
    object (self)
      inherit [_] s_iter as super

      method! visit_ECall dummy cid args =
        List.iter (self#visit_exp dummy) args;
        don't_remove := CidSet.add cid !don't_remove
    end
  in
  v#visit_decls () ds;
  List.filter
    (fun d ->
      match d.d with
      | DEvent (id, _, _, _) | DHandler (id, _) ->
        CidSet.mem (Id id) !don't_remove
      | _ -> true)
    ds
;;

(* Move all handler definitions to the bottom of the program to make sure
   they appear after any relevant globals have been defined *)
let sink_handlers ds =
  let handlers, others =
    List.partition
      (fun d ->
        match d.d with
        | DHandler _ -> true
        | _ -> false)
      ds
  in
  others @ handlers
;;

let eliminate_prog ds =
  let global_info = collect_globals ds in
  let event_mappings = duplicate_all_events global_info ds in
  (* print_endline @@ mapping_to_string event_mappings; *)
  let ds = replace_decls event_mappings ds in
  let ds = replace_uses event_mappings ds in
  let ds = deduplicate event_mappings ds in
  let ds = sink_handlers ds in
  ds
;;

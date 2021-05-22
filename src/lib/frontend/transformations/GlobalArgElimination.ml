open Batteries
open Syntax
open Collections

module GMap = Map.Make (struct
  type t = global_ty

  let compatible size1 size2 =
    match STQVar.strip_links size1, STQVar.strip_links size2 with
    | IVar (QVar _), _ | _, IVar (QVar _) -> true
    | s1, s2 -> s1 = s2
  ;;

  let compare (id1, sizes1) (id2, sizes2) =
    if Cid.equals id1 id2
       && List.length sizes1 = List.length sizes2
       && List.for_all2 compatible sizes1 sizes2
    then 0
    else Pervasives.compare (id1, sizes1) (id2, sizes2)
  ;;
end)

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

(* Maps to tell us the effect of each global id, and the ids of all the globals
   with a specific type *)
type global_info = int IdMap.t * id list GMap.t

let collect_globals ds : global_info =
  let gs =
    List.filter_map
      (function
        | { d = DGlobal (id, gty, _, _) } -> Some (id, gty)
        | _ -> None)
      ds
  in
  List.fold_lefti
    (fun (idmap, gmap) i (id, gty) ->
      IdMap.add id i idmap, GMap.modify_def [] gty (fun lst -> id :: lst) gmap)
    (IdMap.empty, GMap.empty)
    gs
;;

(* Maps variables in a constraint to global variable ids *)
type inst = id IdMap.t

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
      | Some mid -> mid
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
      (fun (arg_id, arg_ty) -> arg_id, GMap.find_default [] arg_ty gmap)
      gparams
  in
  let possible_insts = all_insts possible_gids in
  List.filter satisfies_spec possible_insts
;;

type event_mapping = inst * id * params

(* Given the list of existing globals and an event declaration, return several
   copies of the declaration, one for each possible combination of globals
   that could be passed in. Also return the instantiation that led to each
   copy *)
let duplicate_event_decl global_info (eid, constr_specs, params)
    : event_mapping list
  =
  let gparams =
    List.filter_map
      (function
        | id, { raw_ty = TGlobal (gty, _) } -> Some (id, gty)
        | _ -> None)
      params
  in
  let possibles = possible_instantiations global_info constr_specs gparams in
  let new_id inst =
    (* Don't rename if there were no global args *)
    if List.length gparams = 0
    then eid
    else
      Id.fresh
      @@ Id.name eid
      ^ List.fold_left
          (fun acc (id, _) -> acc ^ "_" ^ Id.name (IdMap.find id inst))
          ""
          gparams
  in
  (* Don't need constraints anymore! *)
  List.map (fun inst -> inst, new_id inst, params) possibles
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

let replace_decls emap ds =
  let add_param_defs inst params orig_params body =
    let subst_map =
      List.fold_left2
        (fun acc (arg_id, ty) (orig_id, _) ->
          (* arg_id is the name used in the handler's definition; orig_id is
             the name used in the event declaration *)
          match ty.raw_ty with
          | TGlobal _ ->
            IdMap.add arg_id (EVar (Id (IdMap.find orig_id inst))) acc
          | _ -> acc)
        IdMap.empty
        params
        orig_params
    in
    FunctionInlining.subst subst_map body
  in
  let filter_params params =
    List.filter
      (fun (_, ty) ->
        match ty.raw_ty with
        | TGlobal _ -> false
        | _ -> true)
      params
  in
  List.concat
  @@ List.map
       (fun d ->
         match d.d with
         | DEvent (eid, sort, _, _) ->
           begin
             match IdMap.find_opt eid emap with
             | Some lst ->
               List.map
                 (fun (_, eid', params') ->
                   decl_sp
                     (DEvent (eid', sort, [], filter_params params'))
                     d.dspan)
                 lst
             | None -> failwith "Impossible. I hope."
           end
         | DHandler (eid, (params, body)) ->
           begin
             match IdMap.find_opt eid emap with
             | Some lst ->
               List.map
                 (fun (inst, eid', orig_params) ->
                   let body' = add_param_defs inst params orig_params body in
                   decl_sp
                     (DHandler (eid', (filter_params params, body')))
                     d.dspan)
                 lst
             | None -> failwith "Impossible. I hope."
           end
         | _ -> [d])
       ds
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
                  match Option.get e.ety with
                  | TGlobal _ ->
                    begin
                      match e.e with
                      | EVar cid -> Some (Cid.to_id cid)
                      | _ ->
                        Console.error
                        @@ "This expression doesn't look like a global id: "
                        ^ Printing.exp_to_string e
                    end
                  | _ -> None)
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
                (fun (_, id, _) ->
                  if Id.name id = expected_name then Some id else None)
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
        (fun (inst, eid, params) ->
          "inst: "
          ^ idmap_to_string Id.to_string inst
          ^ ", "
          ^ Id.to_string eid
          ^ "("
          ^ Printing.comma_sep
              (fun (id, ty) -> Printing.ty_to_string ty ^ " " ^ Id.to_string id)
              params
          ^ ")\n")
        lst)
    m
;;

(* Optimization: Go through one last time and remove the declaration/handler
   for any events which aren't actually generated, to minimize bloat. Only do
   this for events which once had a global argument, though, so the user can
   still generate other events via the interpreter spec file. *)
let deduplicate event_mapping ds =
  (* Strategy: Every event value must be created at some point through an ECall,
     so just collect the name of every function that's called and remove any
     event/handler definitions which don't match. *)
  let didn't_duplicate =
    IdMap.fold
      (fun _ renamings acc ->
        (* The entries with singleton lists correspond to all the events we
            _didn't_ make multiple copies of. *)
        match renamings with
        | [(_, new_id, _)] -> CidSet.add (Id new_id) acc
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
  let ds = replace_decls event_mappings ds in
  let ds = replace_uses event_mappings ds in
  let ds = deduplicate event_mappings ds in
  sink_handlers ds
;;

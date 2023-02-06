open CoreSyntax
open CorePrinting
module IdMap = Map.Make(struct type t = CoreSyntax.id let compare = (fun (s1,i1) (s2,i2) -> String.compare s1 s2) end)

type ev_var_info = {
  ev_id_int : id; 
  ev_params_map : ((id*ty) list) IdMap.t; 
  ev_initial_val : int
}

type event_info = {
  i : int;
  id : id;
  params : (id * ty) list
}

type env = {
  var_infos : ev_var_info IdMap.t;
  event_infos : event_info IdMap.t
}



let max_int map = 
  IdMap.fold (fun _ ei cur -> if ei.i > cur then ei.i else cur) map 0
;;

let make_ev_id_int id = 
  Id.create ((id_to_string id)^"_event_int")
;;

let make_ev_param_id id eid origid = 
  Id.create((id_to_string id)^(id_to_string eid)^(id_to_string origid)^"event_param_id")
;;

let make_params_map var_id ev_infos = 
  let add_event_vars ev_info = List.map (fun (id, ty) -> ((make_ev_param_id var_id ev_info.id id), ty)) ev_info.params in
  IdMap.map add_event_vars ev_infos
;;

let make_default_val ty = 
  match ty.raw_ty with
  | TInt size -> exp (EVal(vint 0 size)) ty
  | _ -> Console.error "No default for non-ints"
;;

let ev_id_int_var_ty = ty (TInt (8));;

let make_num_size i size = exp (EVal (vinteger (Integer.create i size))) (ty (TInt size));;
let make_ev_id_var ev_var_info = 
  (statement (SLocal (ev_var_info.ev_id_int, ev_id_int_var_ty, (make_num_size ev_var_info.ev_initial_val 8))))

let make_local_defs ev_var_info = 
  let make_param_defs params = List.map (fun (id, ty) -> (statement (SLocal (id, ty, (make_default_val ty))))) params in
  let param_var_defs = IdMap.fold (fun eid params acc -> List.append (make_param_defs params) acc) ev_var_info.ev_params_map [] in
  (make_ev_id_var ev_var_info):: param_var_defs

let make_default_exp_map ev_infos = 
  IdMap.map (fun ev_info -> List.map (fun (id, ty) -> (make_default_val ty)) ev_info.params) ev_infos

let get_map var_infos event_infos match_exp = 
  match match_exp.e with 
  | EVar (cid) -> (match IdMap.find_opt (Cid.to_id cid) var_infos with
                    | Some ev_var_info -> Some (IdMap.map (fun l -> (List.map (fun (id, ty) -> (exp (EVar (Cid.id id)) ty)) l)) ev_var_info.ev_params_map)
                    | None -> None)
  | ECall (cid, exps) -> (match IdMap.find_opt (Cid.to_id cid) event_infos with 
                            | Some ev_info -> Some (IdMap.add (Cid.to_id cid) exps (make_default_exp_map event_infos))
                            | None -> None)
  | _ -> None
;;
let replace_exp var_infos event_infos exp = 
  match exp.e with 
  | EVar (cid) -> (match IdMap.find_opt (Cid.to_id cid) var_infos with
                    | Some ev_var_info -> aexp (EVar (Cid.id (ev_var_info.ev_id_int))) ev_id_int_var_ty exp.espan
                    | None -> exp)
  | ECall (cid, params) -> (match IdMap.find_opt (Cid.to_id cid) event_infos with 
                            | Some ei -> (make_num_size ei.i 8)
                            | None -> exp)
  | _ -> exp
;;
let get_param_ids params =
  List.map (fun (id, ty) -> id) params
;;
let replace_pat event_infos pat = 
  match pat with
  | PEvent (cid, params) -> PNum (Z.of_int ((IdMap.find (Cid.to_id cid) event_infos).i))
  | _ -> pat
;;

let sequence_statements ss =
  let seq = List.fold_left sseq snoop ss in
  seq.s
;;

let combine_exp_pat_info loexpmap_opt pats = 
  let comb_lists acc loexp pat_id_list = List.fold_left2 (fun acc exp id -> IdMap.add id exp acc) acc loexp pat_id_list in
  let combine acc expmap_opt pat = 
    (match expmap_opt, pat with
    | Some expmap, PEvent (cid, params) -> let param_id_list = get_param_ids params in
                                            comb_lists acc (IdMap.find (Cid.to_id cid) expmap) param_id_list
    | _ -> acc) in
  List.fold_left2 combine IdMap.empty loexpmap_opt pats

let var_replacer = 
  object (self)
    inherit [_] s_map as super
    method! visit_EVar env cid = (match (IdMap.find_opt (Cid.to_id cid) env) with
                                  | Some exp -> exp.e
                                  | None -> EVar (cid))

  end 
let replacer = 
  object (self) 
    inherit [_] s_map as super
    method! visit_DEvent env id sort params = 
      let ei = (!env).event_infos in 
      env := {(!env) with event_infos = (IdMap.add id {i = (max_int ei) + 1; id = id; params=params} ei)}; 
      DEvent((id, sort, params))

    method! visit_SLocal env id ty exp = 
      match ty.raw_ty with 
      | TEvent -> let initial_val = 
                    match exp.e with
                    | ECall (cid, params) -> (match IdMap.find_opt (Cid.to_id cid) (!env).event_infos with 
                                              | Some ev_info -> ev_info.i
                                              | None -> Console.error_position exp.espan "Not an event call" )
                    | EVar (cid) -> Console.error_position exp.espan "can't do event aliasing for now... why?"
                    | _ -> Console.error_position exp.espan "This should be a type-error"
                  in
                  let ev_var_info = {ev_id_int=(make_ev_id_int id); 
                                      ev_params_map = (make_params_map id (!env).event_infos);
                                      ev_initial_val = initial_val} in
        
                  env := {(!env) with var_infos = IdMap.add id ev_var_info (!env).var_infos}; 
                  sequence_statements (List.append (make_local_defs ev_var_info) [(statement (SLocal (id, ty, exp)))])
      | _ -> SLocal (id, ty, exp)

    method! visit_SMatch env exps branches = 
      let match_exp_maps = List.map (get_map (!env).var_infos (!env).event_infos) exps in
      let new_exps = List.map (replace_exp (!env).var_infos (!env).event_infos) exps in
      let branch_replacer (ps, s) = 
        let id_exp_map = combine_exp_pat_info match_exp_maps ps in
        let new_ps = List.map (replace_pat (!env).event_infos) ps in
        let new_s = super#visit_statement env (var_replacer#visit_statement id_exp_map s) in
        (new_ps, new_s) in
      (SMatch (new_exps, (List.map branch_replacer branches)))
  end


let process_prog ds = 
  let env_infos = (ref {var_infos = IdMap.empty; event_infos = IdMap.empty}) in
  let ds = replacer#visit_decls env_infos ds in 
  ds
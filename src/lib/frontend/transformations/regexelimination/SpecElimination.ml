open Syntax
open PlainRegex
open DFASynthesis
open RegexElimination


type insert_info = {
  vr_id : Id.t;
  events : Id.t list; 
  data : decl option;
  idx : decl option;
  effect : statement
}

let fun_idx_expr = make_evar (Id.create "sre_idx_fun_name");;

let make_sr_macro id effect idx_expr =
  let idx = (match idx_expr with | Some ex -> fun_idx_expr | None -> (make_num 0)) in
  statement (SIf ((exp (ETransitionRegex (id, idx, None))), effect, (statement SNoop)))

  
let insert_if_necessary h_id body infos = 
  let insertions = List.filter_map (fun info -> if (List.mem h_id info.events) then Some (make_sr_macro info.vr_id info.effect info.idx) else None) infos in
  DHandler (h_id, ((fst body), statement (sequence_statements (List.rev ((snd body) :: insertions)))))

let replace_spec_regex env id size sr = 
  match sr.s_regex with
  | SRDetect (Some data, Some idx, vr, effect) -> 
    let vr_decl = decl (DVarRegex (id, size, (alphabet (AUnspecified)), vr)) in 
    [vr_decl; data; idx]
  | SRDetect (None, Some idx, vr, effect) -> 
    let vr_decl = decl (DVarRegex (id, size, (alphabet (AUnspecified)), vr)) in 
    [vr_decl;idx]
  | SRDetect (Some data, None, vr, effect) -> 
    let vr_decl = decl (DVarRegex (id, size, (alphabet (AUnspecified)), vr)) in 
    [vr_decl;data]
  | SRDetect (None, None, vr, effect) -> [(decl (DVarRegex (id, size, (alphabet (AUnspecified)), vr)))]

let replacer = 
  object (self) 
    inherit [_] s_map as super
    method! visit_DSpecRegex env id size sr = 
      match sr.s_regex with 
      | SRDetect(data, idx, vr, effect) -> env := {vr_id = id; events = (get_events vr); data = data; idx = idx; effect = effect} :: (!env); DSpecRegex (id, size, sr)
  
    method! visit_DHandler env id body = insert_if_necessary id body (!env)
  end


let process_prog ds = 
  let env_infos = (ref []) in
  let ds = replacer#visit_decls env_infos ds in
  let replace_spec d = 
    match d.d with 
    | DSpecRegex (id, size, spec_regex) -> replace_spec_regex !env_infos id size spec_regex
    | _ -> [d] in
  List.flatten (List.map replace_spec ds)
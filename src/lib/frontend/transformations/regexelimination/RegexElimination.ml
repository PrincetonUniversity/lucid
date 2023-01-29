open Batteries
open Syntax
open SyntaxUtils
open Arrays
open Z3
open Collections
open PlainRegex
open DFASynthesis



(*-------------------Parsing RE stuff ----------------------------*)
type binder = 
  {
    binding_event : id;
    assignments : (id * id) list
  }
and pred_info = 
  {
    pred_event : id;
    pred : exp
  };;

let binders re = 
  let rec binders_acc re acc = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (_, _) -> acc
    | VRBinding (event_id, assignments, sub) -> {binding_event=event_id; assignments=assignments} :: binders_acc sub acc
    | VRConcat (sub1, sub2) | VRUnambigConcat (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
    | VRClosure sub -> binders_acc sub acc
    | VROr (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
    | VRAnd (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
  in binders_acc re [];;

let preds re = 
  let rec preds_acc re acc = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (event_id, pred) -> 
      (match pred.e with 
      (*If it's a value, it has to be a bool*)
      | EVal (v) -> acc
      | _ -> let p_rec = {pred_event=event_id; pred=pred} in 
                if (List.exists (fun pr -> pr.pred.e = p_rec.pred.e) acc) then acc else p_rec :: acc)
    | VRBinding (event_id, assignments, sub) -> preds_acc sub acc
    | VRConcat (sub1, sub2) | VRUnambigConcat (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
    | VRClosure sub -> preds_acc sub acc
    | VROr (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
    | VRAnd (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
  in preds_acc re [];;

let rec get_events re = 
  let merge events other = 
    List.fold_left (fun acc ev_name -> if List.mem ev_name acc then acc else ev_name :: acc) events other in
  match re.v_regex with 
  | VREmptySet -> []
  | VREmptyStr -> []
  | VRLetter (event_id, pred) -> [event_id]
  | VRBinding (event_id, assignments, sub) -> merge [event_id] (get_events sub)
  | VRUnambigConcat (sub1, sub2) | VRConcat (sub1, sub2) | VROr (sub1, sub2) | VRAnd (sub1, sub2) -> merge (get_events sub1) (get_events sub2)
  | VRClosure sub -> get_events sub
;;


type alphabet_letter_def = {letter_id : id ; preds : exp list}
;;
(*Make a list of alphabet letter defs from the relevant info about an RE*)
let alphabet_def re_preds re_events=
  let rec update_ans_list list_of_alphabet_def pi = 
    match list_of_alphabet_def with 
    [] -> [{letter_id=pi.pred_event; preds=[pi.pred]}]
    | {letter_id=letter_id; preds=preds} :: tail -> if letter_id = pi.pred_event then {letter_id=letter_id; preds=pi.pred ::preds} :: tail else {letter_id=letter_id; preds=preds} :: (update_ans_list tail pi)
  in 
    let acc = List.map (fun ev -> {letter_id=ev; preds=[]}) re_events in
      List.fold_left update_ans_list acc re_preds
;;
(*From a list of alphabet defs, return all of the possible PRE symbols.*)
let rec get_letters ev_id preds = 
  let add_to_bools bool lopres = 
    List.map (fun pres -> {ev_id=pres.ev_id; bools=(bool :: pres.bools)}) lopres in
  match preds with
  | [] -> [{ev_id=(fst ev_id); bools=[]}]
  | p :: tail -> let rest_of_letters = (get_letters ev_id tail) in 
    List.append (add_to_bools true rest_of_letters) (add_to_bools false rest_of_letters);;
let rec get_all_letters alphabet =
  match alphabet with
  | [] -> []
  | {letter_id=letter_id; preds=preds} :: tail -> List.append (get_letters letter_id preds) (get_all_letters tail)
;;

(*----------translate from RE to PRE--------------*)
(*Make a list of list of booleans to represent the letters to be "Or" together *)
let rec big_union_helper letter_pred list_of_pred =
  let tack_bool_front b lolob = List.map (function lob -> b::lob) lolob in
    match list_of_pred with 
    [] -> [[]]
    | pred :: tail -> let new_tail = big_union_helper letter_pred tail in
      match letter_pred with
      Some p when p = pred -> (tack_bool_front true new_tail)
      | _ -> (List.append (tack_bool_front true new_tail) (tack_bool_front false new_tail))
;;

let rec big_union_make_or ev_name lolob = 
  match lolob with 
  [] -> PREmptySet
  | lob :: tail -> pre_or (PRSymbol({ev_id = ev_name; bools = lob})) (big_union_make_or ev_name tail)
;;

let rec big_union event_id pred alphabet = 
  match alphabet with
  [] -> PRSymbol({ev_id=(fst event_id); bools=[]})
  | {letter_id=letter_id; preds=preds} :: tail -> if letter_id = event_id 
    then (big_union_make_or (fst event_id) (big_union_helper pred preds))
    else big_union event_id pred tail
;;
(*Turn an RE into a PRE, given the alphabet*)
let rec translate re re_alphabet = 
    match re with 
    | VREmptySet -> PREmptySet
    | VREmptyStr -> PREmptyString
    | VRLetter (event_id, pred) -> 
      (match pred.e with 
      | EVal (v) -> (match v.v with 
        | VBool b when b-> big_union event_id None re_alphabet
        | _ -> PREmptySet)
      | _ -> big_union event_id (Some pred) re_alphabet)
    | VRBinding (event_id, assignments, sub) -> pre_concat (big_union event_id None re_alphabet) (translate sub.v_regex re_alphabet)
    | VRUnambigConcat (sub1, sub2) | VRConcat (sub1, sub2) -> pre_concat (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRClosure sub -> pre_closure (translate sub.v_regex re_alphabet)
    | VROr (sub1, sub2) -> pre_or  (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRAnd (sub1, sub2) -> pre_and (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
;;



(*---------------------Z3 synthesis----------------------*)



(*---------------Making the statements---------------*)

let rec get_index_of elem loe start comp = 
  match loe with
  | [] -> -1
  | e2 :: tail -> if comp elem e2 then start else (get_index_of elem tail (start+1) comp)
;;

let letter_to_int pres event_order total_preds = 
  let rec letter_to_int_acc bools acc shifts= 
    (match bools with 
    | [] -> acc lsl shifts
    | b :: tail -> let next = if b then (acc lsl 1) + 1 else (acc lsl 1) in letter_to_int_acc tail next (shifts-1)) in
  letter_to_int_acc pres.bools (get_index_of pres.ev_id event_order 0 (fun s event_id -> s = (fst event_id))) total_preds
;;


let sequence_statements ss =
  let seq = List.fold_left sseq snoop ss in
  seq.s
;;

let dgloabl_arr_type int_size = ty (TName (Arrays.t_id, [(IConst int_size)], true));;

let make_id_val id = (Id.create (String.cat (fst id) "val"));;
let make_idx_val id = Id.create (String.cat (fst id) "idx");;


let make_counter_id id = 
  Id.create ("counter"^(fst id))
;;

let make_res_id id = 
  Id.create ("res"^(fst id))
;;

let make_ans_id id = 
  Id.create ("ans"^(fst id))
;;

let make_exp_from_statements id lostatements =
  EStmt (statement (sequence_statements lostatements), (make_evar (make_ans_id id)))

let make_global_def arr_size int_size id = 
  decl (DGlobal (id, (dgloabl_arr_type int_size), exp (ECall (Arrays.array_create_id, [(make_num arr_size)]))))
;;

let get_type_of_var id be event_params_map =
  let event_params = IdMap.find be event_params_map in
    snd (List.find (fun param -> (fst param) = id) event_params)
;;



type reInfo = 
{
  binders : binder list;
  preds : pred_info list;
  synthesis_response : synthesis_response;
  event_order : id list;
  alphabet : plain_re_symbol list
};;


type env = {
  re_info_map : reInfo IdMap.t; 
  current_handler : (Id.t * params) option;
  params_map : params IdMap.t;
};;

let rec check_unmabig_concat_vr vr alphabet = 
  match vr.v_regex with 
  | VRUnambigConcat (suba, subb) -> PlainRegex.check_unambiguous_concat (translate suba.v_regex alphabet) (translate subb.v_regex alphabet) (get_all_letters alphabet)
  | VRConcat (suba, subb) -> (check_unmabig_concat_vr suba alphabet) || (check_unmabig_concat_vr subb alphabet)
  | _ -> true


let re_info id alphabet vr = 
  let re_binders = binders vr in
  let re_preds = preds vr in
  let events = (match alphabet.alph with 
                | AExplicit (ids) -> ids
                | AUnspecified -> (get_events vr)
                | AName _ -> Console.error_position alphabet.alphabet_span "Alphabets should have been eliminated") in 
  let re_alphabet = alphabet_def re_preds events in 
  let pre = translate vr.v_regex re_alphabet in
  let dfa = (plain_re_to_dfa pre (get_all_letters re_alphabet)) in
  let synthesized = synthesize id dfa in
    Printf.printf "%d binders.\n%d events\n%d size alph def\n" (List.length re_binders) (List.length events) (List.length re_alphabet);
    if not (check_unmabig_concat_vr vr re_alphabet) then Console.error_position vr.v_regex_span @@ Printf.sprintf "That concatenation may not be unambiguous";
    List.iter (fun letter -> (Printf.printf "%s: %d\n" (print_letter letter) (letter_to_int letter events (List.length re_preds)))) (get_all_letters re_alphabet);
    print_dfa dfa;
    print_string (plain_re_to_string pre);
    {binders=re_binders; preds = re_preds; synthesis_response=synthesized;event_order=events; alphabet=(get_all_letters re_alphabet)}
;;

let make_binding_set be params_map idx_expr assignment =
  (statement (SLocal ((make_id_val (fst assignment)), 
  (get_type_of_var (snd assignment) be params_map),
  (exp (ECall (Arrays.array_setm_cid, [ (make_evar (fst assignment));idx_expr;(make_evar (Id.create "checkThenSet"));(make_evar (snd assignment))]))))))
;;

let make_binding_get be params_map idx_expr assignment = 
  (statement (SLocal ((make_id_val (fst assignment)), 
  (get_type_of_var (snd assignment) be params_map),
  (exp (ECall (Arrays.array_get_cid, [ (make_evar (fst assignment)); idx_expr]))))))
;;

let make_lshift id = (exp (EOp (LShift, [(exp (EVar (Cid.id id)));(make_num 1)])));;

let make_assign id exp = statement (SAssign (id, exp));;

let pred_update_statement id pred = 
  let counter = (make_counter_id id) in 
    let lshift = make_lshift counter in 
      statement (SIf (pred, (make_assign counter (exp (EOp (Plus, [lshift; (make_num 1)])))), (make_assign counter lshift)))
;;

let rec make_binding_defs binders handler_eid params_map idx_expr = 
  let def_maker be = (match handler_eid with 
  | None -> make_binding_get be params_map idx_expr
  | Some (eid, params) -> if be = eid then (make_binding_set be params_map idx_expr) else (make_binding_get be params_map idx_expr)) in
    match List.rev binders with 
    | [] -> []
    | {binding_event = be; assignments = assignments} :: tail -> 
      let defs = List.map (def_maker be) assignments in 
      List.append defs (make_binding_defs tail handler_eid params_map idx_expr)
;;

let rec make_pred_defs reid preds handler_eid =
  match handler_eid, preds with 
  | None, _ -> []
  | _, [] -> []
  | Some (eid, _), {pred_event=pred_event; pred=pred} :: tail -> 
    if pred_event = eid then (pred_update_statement reid pred) :: (make_pred_defs reid tail handler_eid) else (make_pred_defs reid tail handler_eid)
;;

let init_val eid event_order preds = 
  let shifts = List.length (List.filter (fun pred -> pred.pred_event <> eid) preds) in
  (get_index_of eid event_order 0 (fun x y -> x=y)) lsl shifts

let counter_def id re_info handler_eid preds = 
  match handler_eid with
  | None -> error "not in event handler"
  | Some (eid, _) -> statement (SLocal ((make_counter_id id), (ty (TInt (IConst DFASynthesis.bv_size))), (make_num_size (init_val eid re_info.event_order preds) DFASynthesis.bv_size)))
;;

let res_def id = 
  statement (SLocal ((make_res_id id), (ty (TInt (IConst DFASynthesis.bv_size))), (make_num_size 0 DFASynthesis.bv_size)));;

let ans_def id = 
  statement (SLocal ((make_ans_id id), (ty (TBool)), (exp (EVal (value (VBool true))))));;

let make_row_pair id letter synthesis_response event_order preds = 
  let memop_id = (List.nth synthesis_response.memops (LetterMap.find letter synthesis_response.whichop)).id in
    let f = LetterMap.find letter synthesis_response.f in 
      let g = LetterMap.find letter synthesis_response.g in 
        let pattern = [(PNum (Z.of_int (letter_to_int letter event_order (List.length preds))))] in
          let stmt = statement (SAssign ((make_res_id id), (exp (ECall (Arrays.array_update_complex_cid, [(make_evar id); (make_evar (make_idx_val id)); (make_evar memop_id); (make_num_size f DFASynthesis.bv_size); (make_num_size g DFASynthesis.bv_size); (make_num_size 0 DFASynthesis.bv_size)]))))) in
            (pattern,stmt)
;;

let make_match_def id alphabet synthesized event_order preds =
  let rows = List.map (fun letter -> (make_row_pair id letter synthesized event_order preds)) alphabet in
    [statement (SMatch ([(make_evar (make_counter_id id))], rows))]
;;

let make_return_def id synthesis_response = 
  let rows = List.map (fun accept_state -> ([(PNum (Z.of_int accept_state))], (statement (SAssign ((make_ans_id id), (exp (EVal (value (VBool true))))))))) synthesis_response.accepting in
    let last = ([(PWild)], (statement (SAssign ((make_ans_id id), (exp (EVal (value (VBool false)))))))) in
      [statement (SMatch ([(make_evar (make_res_id id))], List.rev (last :: rows)))]
;;

let make_static_defs id idx_expr = 
  let time_def = (statement (SLocal ((Id.create "time"), (ty (TInt (IConst 32))), (exp (ECall ((Cid.create ["Sys"; "time"]), [])))))) in
  let idx_def = (statement (SLocal ((make_idx_val id), (ty (TInt (IConst 32))), idx_expr))) in
  [time_def; idx_def]

let make_transition_statements env id idx_expr = 
  let re_info = IdMap.find id env.re_info_map in
  let static_defs = make_static_defs id idx_expr in
  let binding_defs = List.append static_defs (make_binding_defs re_info.binders env.current_handler env.params_map (make_evar (make_idx_val id))) in
  let pred_update_defs = List.append binding_defs ((counter_def id re_info env.current_handler re_info.preds) :: (make_pred_defs id re_info.preds env.current_handler)) in
  let match_def = List.append pred_update_defs ((res_def id) :: (make_match_def id re_info.alphabet re_info.synthesis_response re_info.event_order re_info.preds)) in
  let return_def = List.append match_def (make_return_def id re_info.synthesis_response) in
  return_def
;;

let make_trans_match env id idx_expr event_expr = 
  let make_branch (eid, params) = 
    let p = [PEvent (Cid.create_ids [eid], params)] in
    let s = statement (sequence_statements (make_transition_statements {env with current_handler = Some (eid, params)} id idx_expr)) in
    (p,s) in
  let re_info = IdMap.find id env.re_info_map in
  let events_list = List.map (fun eid -> (eid, IdMap.find eid env.params_map)) re_info.event_order in
  statement (SMatch ([event_expr], ((List.map (fun ev_info -> make_branch ev_info) events_list))))

let replacer = 
  object (self)
    inherit [_] s_map as super

    method! visit_DEvent env id event_sort constr_spec_list params = env := {(!env) with params_map=(IdMap.add id params (!env).params_map)}; DEvent (id, event_sort, constr_spec_list, params)
    
    method! visit_DHandler env id body = env := {(!env) with current_handler= (Some (id, (fst body)))}; DHandler (id, ((self#visit_body env) body))

    method! visit_DVarRegex env id size alph vr = env := {(!env) with re_info_map=(IdMap.add id (re_info id alph vr) (!env).re_info_map)}; DVarRegex (id, size, alph, vr)
    
    method! visit_ETransitionRegex env id idx ev_expr = 
      match ev_expr with 
      | None -> make_exp_from_statements id ((ans_def id) :: (make_transition_statements !env id idx))
      | Some e -> make_exp_from_statements id ((ans_def id) :: [(make_trans_match !env id idx e)])

  end
  ;;



let replace_var_regex env id size vr = 
  let re_info = IdMap.find id env.re_info_map in
  List.append 
    (List.rev_map (make_global_def size 32) (List.map (fun (id1, id2) -> id1) (List.flatten (List.map (fun b -> b.assignments) (binders vr)))))
    ((make_global_def size DFASynthesis.bv_size id) :: (List.map (fun memop_response -> decl (DMemop (memop_response.id, memop_response.params, memop_response.memop_body))) re_info.synthesis_response.memops))
;;


let process_prog ds = 
  let env = (ref {re_info_map=IdMap.empty; current_handler=None; params_map=IdMap.empty}) in 
  let ds = replacer#visit_decls env ds in
    let replace d = 
      match d.d with 
      | DVarRegex (id, size, alph, var_regex) -> replace_var_regex !env id (Z.to_int size) var_regex
      | _ -> [d] in
    List.flatten (List.map replace ds)
      


let make_statements_def id = 
  ECall ((Cid.create_ids [Id.create "Array"; Id.create "set"]), [exp (EVar (Cid.create_ids [id])); exp (EVal(vinteger (Integer.of_string "0"))); exp (EVal(vinteger (Integer.of_string "17")))])
;;


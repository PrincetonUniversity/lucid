open Batteries
open Syntax
open SyntaxUtils
open Arrays
open Z3
open Collections
open PlainRegex
open DFASynthesis
open Printing



(*-------------------Parsing RE stuff ----------------------------*)
type binder = 
  {
    binding_event : id;
    assignments : (ty * id * exp) list; 
    under_closure : bool
  }
and pred_info = 
  {
    pred_event : id;
    pred : exp
  };;

let binders re = 
  let rec binders_acc re acc in_closure = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (_, _) -> acc
    | VRBinding (event_id, assignments, pred, sub) -> {binding_event=event_id; assignments=assignments; under_closure = in_closure} :: binders_acc sub acc in_closure 
    | VRConcat (sub1, sub2) | VRUnambigConcat (sub1, sub2) | VROr (sub1, sub2) | VRAnd (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc in_closure) in_closure
    | VRClosure sub | VRNegation sub -> binders_acc sub acc true
  in binders_acc re [] false;;

let preds re = 
  let check_pred event_id pred acc = 
    (let make_rec pred = 
      (let p_rec = {pred_event=event_id; pred=pred} in 
                if (List.exists (fun pr ->(exp_to_string pr.pred) = (exp_to_string p_rec.pred)) acc) then acc else p_rec :: acc) in
    match pred.e with 
      (*If it's a value, it has to be a bool*)
      | EVal (_) -> acc
      | EOp (Not, [sub]) -> make_rec sub
      | _ -> make_rec pred) in 
  let rec preds_acc re acc = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (event_id, pred) -> check_pred event_id pred acc
    | VRBinding (event_id, assignments, pred, sub) -> preds_acc sub (check_pred event_id pred acc)
    | VRConcat (sub1, sub2) | VRUnambigConcat (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
    | VRClosure sub | VRNegation sub-> preds_acc sub acc
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
  | VRBinding (event_id, _, _, sub) -> merge [event_id] (get_events sub)
  | VRUnambigConcat (sub1, sub2) | VRConcat (sub1, sub2) | VROr (sub1, sub2) | VRAnd (sub1, sub2) -> merge (get_events sub1) (get_events sub2)
  | VRClosure sub | VRNegation sub -> get_events sub
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
let rec big_union_helper letter_pred list_of_pred flip =
  let tack_bool_front b lolob = List.map (function lob -> (if flip then (not b) else b)::lob) lolob in
    match list_of_pred with 
    [] -> [[]]
    | pred :: tail -> let new_tail = big_union_helper letter_pred tail flip in
      match letter_pred with
      Some p when (exp_to_string p) = (exp_to_string pred) -> (tack_bool_front true new_tail)
      | _ -> (List.append (tack_bool_front true new_tail) (tack_bool_front false new_tail))
;;

let rec big_union_make_or ev_name lolob = 
  match lolob with 
  [] -> PREmptySet
  | lob :: tail -> pre_or (PRSymbol({ev_id = ev_name; bools = lob})) (big_union_make_or ev_name tail)
;;

let rec big_union event_id pred alphabet flip = 
  match alphabet with
  [] -> PRSymbol({ev_id=(fst event_id); bools=[]})
  | {letter_id=letter_id; preds=preds} :: tail -> if letter_id = event_id 
    then (big_union_make_or (fst event_id) (big_union_helper pred preds flip))
    else big_union event_id pred tail flip
;;
(*Turn an RE into a PRE, given the alphabet*)
let rec translate re re_alphabet = 
    let preamble_big_union event_id pred = (match pred.e with 
    | EVal (v) -> (match v.v with 
      | VBool b when b-> big_union event_id None re_alphabet false
      | _ -> PREmptySet)
    | EOp (Not, [sub]) -> (big_union event_id (Some sub) re_alphabet true)
    | _ -> big_union event_id (Some pred) re_alphabet false) in
    match re with 
    | VREmptySet -> PREmptySet
    | VREmptyStr -> PREmptyString
    | VRLetter (event_id, pred) -> preamble_big_union event_id pred
    | VRBinding (event_id, assignments, pred, sub) -> pre_concat (preamble_big_union event_id pred) (translate sub.v_regex re_alphabet)
    | VRUnambigConcat (sub1, sub2) | VRConcat (sub1, sub2) -> pre_concat (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRClosure sub -> pre_closure (translate sub.v_regex re_alphabet)
    | VROr (sub1, sub2) -> pre_or  (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRAnd (sub1, sub2) -> pre_and (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRNegation (sub) -> pre_negation (translate sub.v_regex re_alphabet)
;;



(*---------------------Z3 synthesis----------------------*)



(*---------------Making the statements---------------*)

let inferred_size s = TInt (IVar (QVar (Id.create s)))

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

let dgloabl_arr_type size = ty (TName (Arrays.t_id, [size], true));;

let make_id_val id = (Id.create (String.cat (fst id) "val"));;
let make_idx_val id = Id.create (String.cat (fst id) "idx");;


let f_id id = Id.create ("f_synthesized_meta_var"^(fst id));;
let g_id id = Id.create ("g_synthesized_meta_var"^(fst id));;
let mem_id id = Id.create ("memop_synthesized_meta_var"^(fst id));;
let make_counter_id id = 
  Id.create ("counter"^(fst id))
;;

let make_res_id id = 
  Id.create ("res"^(fst id))
;;

let make_ans_id id = 
  Id.create ("ans"^(fst id))
;;

let make_asgn_id reid id = 
  Id.create ("assigned_var_"^(fst reid)^(fst id));;
let make_exp_from_statements id lostatements =
  EStmt (statement (sequence_statements lostatements), (make_evar (make_ans_id id)))

let make_global_def arr_size size id = 
  decl (DGlobal (id, (dgloabl_arr_type size), exp (ECall (Arrays.array_create_id, [(make_num arr_size)]))))
;;

let make_global_def_asgn arr_size reid (ty, id, _) = 
  match ty.raw_ty with 
  | TInt (size) -> make_global_def arr_size size (make_asgn_id reid id)

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
  current_handler : (Id.t *handler_sort * params) option;
  params_map : params IdMap.t;
  added_cts : bool;
  added_reset : bool;
  needs_reset : bool
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
  let all_letters = (get_all_letters re_alphabet) in
  let pre = translate vr.v_regex re_alphabet in
  let dfa = (plain_re_to_dfa pre all_letters) in
  Printf.printf "Alphabet size %d\n" (List.length dfa.alphabet);
  let synthesized = synthesize id dfa in
    if not (check_unmabig_concat_vr vr re_alphabet) then Console.error_position vr.v_regex_span @@ Printf.sprintf "That concatenation may not be unambiguous";
    List.iter (fun letter -> (Printf.printf "%s: %d\n" (print_letter letter) (letter_to_int letter events (List.length re_preds)))) all_letters;
    print_dfa dfa;
    print_string (plain_re_to_string pre);
    {binders=re_binders; preds = re_preds; synthesis_response=synthesized;event_order=events; alphabet=all_letters}
;;
let check_then_set_name = Id.create("re12351sdaCheckThenSet");;
let make_binding_set reid be params_map idx_expr in_closure (ty, id, eval) =
  let e = if in_closure then (exp (ECall (Arrays.array_set_cid, [(make_evar (make_asgn_id reid id)); idx_expr;eval]))) else 
    (exp (ECall (Arrays.array_setm_cid, [ (make_evar (make_asgn_id reid id));idx_expr;(make_evar check_then_set_name);eval]))) in
  (statement (SLocal (id, 
  ty,
  e)))
;;
let make_binding_get reid be params_map idx_expr (ty, id, _) = 
  (statement (SLocal (id, 
  ty,
  (exp (ECall (Arrays.array_get_cid, [ (make_evar (make_asgn_id reid id)); idx_expr]))))))
;;

let make_lshift id = (exp (EOp (LShift, [(exp (EVar (Cid.id id)));(make_num 1)])));;

let make_assign id exp = statement (SAssign (id, exp));;

let pred_update_statement id pred = 
  let counter = (make_counter_id id) in 
    let lshift = make_lshift counter in 
      statement (SIf (pred, (make_assign counter (exp (EOp (Plus, [lshift; (make_num_size 1 DFASynthesis.bv_size)])))), (make_assign counter lshift)))
;;
(*
let single_pred_update id eid pred preds event_order = 
  let counter = (make_counter_id id) in
  let init_val = (init_val eid re_info.event_order preds) in
  statement (SIf (pred, ))*)

let rec make_binding_defs reid binders handler_eid params_map idx_expr = 
  let def_maker be in_closure = (match handler_eid with 
  | None -> make_binding_get reid be params_map idx_expr
  | Some (eid, _, _) -> if be = eid then (make_binding_set reid be params_map idx_expr in_closure) else (make_binding_get reid be params_map idx_expr)) in
    match List.rev binders with 
    | [] -> []
    | {binding_event = be; assignments = assignments; under_closure = in_closure} :: tail -> 
      let defs = List.map (def_maker be in_closure) assignments in 
      List.append defs (make_binding_defs reid tail handler_eid params_map idx_expr)
;;

let rec make_pred_defs reid preds handler_eid =
  match handler_eid, preds with 
  | None, _ -> []
  | _, [] -> []
  | Some (eid, _, _), {pred_event=pred_event; pred=pred} :: tail -> 
    if pred_event = eid then (pred_update_statement reid pred) :: (make_pred_defs reid tail handler_eid) else (make_pred_defs reid tail handler_eid)
;;

let init_val eid event_order preds = 
  let shifts = List.length (List.filter (fun pred -> pred.pred_event <> eid) preds) in
  (get_index_of eid event_order 0 (fun x y -> x=y)) lsl shifts

let counter_def id re_info handler_eid preds = 
  match handler_eid with
  | None -> error "not in event handler"
  | Some (eid,_, _) -> statement (SLocal ((make_counter_id id), (ty (TInt (IConst DFASynthesis.bv_size))), (make_num_size (init_val eid re_info.event_order preds) DFASynthesis.bv_size)))
;;

let make_local_synth_var_def varid = 
  statement (SLocal (varid, (ty (TInt (IConst DFASynthesis.bv_size))), (make_num_size 0 DFASynthesis.bv_size)));;

let res_def id = 
  statement (SLocal ((make_res_id id), (ty (TInt (IConst DFASynthesis.bv_size))), (make_num_size 0 DFASynthesis.bv_size)));;

let ans_def id = 
  statement (SLocal ((make_ans_id id), (ty (TBool)), (exp (EVal (value (VBool true))))));;

let make_row_pair id letter synthesis_response event_order preds = 
  let f = LetterMap.find letter synthesis_response.f in 
    let g = LetterMap.find letter synthesis_response.g in 
      let pattern = [(PNum (Z.of_int (letter_to_int letter event_order (List.length preds))))] in
        let fasgn = (make_assign (f_id id) (make_num_size f DFASynthesis.bv_size)) in
        let gasgn = (make_assign (g_id id) (make_num_size g DFASynthesis.bv_size)) in
        let masgn = (make_assign (mem_id id) (make_num_size (LetterMap.find letter synthesis_response.whichop) DFASynthesis.bv_size)) in
        (pattern,statement (sequence_statements [fasgn; gasgn; masgn]))
;;

let make_match_def id alphabet synthesized event_order preds =
  let rows = List.map (fun letter -> (make_row_pair id letter synthesized event_order preds)) alphabet in
    [statement (SMatch ([(make_evar (make_counter_id id))], rows))]
;;

let make_pred_var_id id pred preds = 
  let idx = (get_index_of pred preds 0 (fun p1 p2 -> p1 = p2)) in
  Id.create ("pred"^(string_of_int idx)^(fst id))
;;


(*
   letter_event, (s11 - s12), (s21 - s22), ... -> *)
let make_row_stmt_inline_preds id letter synthesis_response = 
  let fasgn = (make_assign (f_id id) (make_num_size (LetterMap.find letter synthesis_response.f) DFASynthesis.bv_size)) in
  let gasgn = (make_assign (g_id id) (make_num_size (LetterMap.find letter synthesis_response.g) DFASynthesis.bv_size)) in
  let masgn = (make_assign (mem_id id) (make_num_size (LetterMap.find letter synthesis_response.whichop) DFASynthesis.bv_size)) in
  statement (sequence_statements [fasgn; gasgn; masgn])
;;

let make_row_pat_inline_preds letter pred_tuples = 
  let rec make_row_pat_inline_preds_acc bools pred_tuples acc = 
    (match bools, pred_tuples with
    | [], [] -> acc
    | (b :: btail), ((t_pat, f_pat, _) :: ttail) -> (if b then t_pat else f_pat) :: (make_row_pat_inline_preds_acc btail ttail acc)
    | _ -> (Console.error_position Span.default (Printf.sprintf "The list of bools and preds should be the same size! Letter: %s, Preds: %d" (print_letter letter) (List.length pred_tuples))) ) in
  match letter.bools, pred_tuples with
  | [], [] -> (Console.error_position Span.default "Shouldn't be making this match statement at all!")
  | _ -> make_row_pat_inline_preds_acc letter.bools pred_tuples []


let extract_sub_exp pred = 
  let sub_exp args = exp (EOp (Sub, args)) in
  match pred.pred.e with
  | EOp(Eq, args)-> (sub_exp args)
  | EOp(Neq, args) -> (sub_exp args)
  | EOp(Less, args) -> (sub_exp args)
  | EOp (More, args) -> (sub_exp args)
  | _ -> error "Can only do == and != preds right now!"

let extract_t_f_cid id all_preds pred  = 
  let pzero = PNum (Z.of_int 0) in
  let pvid = (make_pred_var_id id pred all_preds) in
  let ppos = PBit ([0; -1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1]) in
  let pneg = PBit ([1; -1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1]) in
  match pred.pred.e with
  | EOp(Eq, args)-> (pzero, PWild, pvid)
  | EOp(Neq, args) -> (PWild, pzero, pvid)
  | EOp(Less, args) -> (pneg, PWild, pvid)
  | EOp (More, args) -> (ppos, PWild, pvid)
  | _ -> error "Can only do == and != preds right now!"

(*Should put all the wildcards at the bottom so they don't step on above rows.*)
let sort_rows_by_wildcards rows = 
  let rem_nonwilds l = List.filter (fun p -> (match p with | PWild -> true | _ -> false)) l in
  let comp (ps1, _) (ps2,_) = List.compare_lengths (rem_nonwilds ps1) (rem_nonwilds ps2) in
  List.sort comp rows

let make_match_def_inline_preds id handler alphabet synthesis_response event_order preds = 
  let eid = (match handler with 
  | None -> error "Not in a handler, but trying to inline preds for a specific event!"
  | Some (eid, _, _) -> eid) in
  let letters_this_event = List.filter (fun c -> c.ev_id = (fst eid)) alphabet in
  match letters_this_event with
  | [] -> Console.error "Something wrong-transition in handler with no letters!"
  | l1 :: [] -> [(make_row_stmt_inline_preds id l1 synthesis_response)]
  | _ -> (
    let preds_this_event = List.filter (fun p -> p.pred_event = eid) preds in
    Printf.printf "Handler: %s\n" (id_to_string eid);
    List.iter (fun p -> Printf.printf "Pred: %s" (exp_to_string p.pred)) preds_this_event;
    let pred_tuples = List.map (extract_t_f_cid id preds) preds_this_event in
    let exps = List.map (fun (_, _, pvid) -> make_evar pvid) pred_tuples in
    let filter_alph = List.filter (fun l -> (fst eid) = l.ev_id) alphabet in 
    let rows = List.map (fun letter -> ((make_row_pat_inline_preds letter pred_tuples), (make_row_stmt_inline_preds id letter synthesis_response))) filter_alph in
    [statement (SMatch (exps, (sort_rows_by_wildcards rows)))])
;;

let make_return_def id synthesis_response = 
  let rows = List.map (fun accept_state -> ([(PNum (Z.of_int accept_state))], (statement (SAssign ((make_ans_id id), (exp (EVal (value (VBool true))))))))) synthesis_response.accepting in
    let last = ([(PWild)], (statement (SAssign ((make_ans_id id), (exp (EVal (value (VBool false)))))))) in
    let print = sprintf "state machine transitioned to %d" [(make_evar (make_res_id id))] in
      [print; statement (SMatch ([(make_evar (make_res_id id))], List.rev (last :: rows)))]
;;

let make_static_defs id idx_expr = 
  let idx_def = (statement (SLocal ((make_idx_val id), (ty (inferred_size "idxvalsize")), idx_expr))) in
  [idx_def;(make_local_synth_var_def (f_id id)); (make_local_synth_var_def (g_id id)); (make_local_synth_var_def (mem_id id))]


let array_update_def id synthesis_response = 
  let make_branch i = 
    let memop_id = (List.nth synthesis_response.memops i).id in
    let st = statement (SAssign ((make_res_id id), (exp (ECall (Arrays.array_update_complex_cid, [(make_evar id); (make_evar (make_idx_val id)); (make_evar memop_id); (make_evar (f_id id)); (make_evar (g_id id)); (make_num_size 0 DFASynthesis.bv_size)]))))) in
    let pat = PNum (Z.of_int i) in 
    ([pat], st) in
  statement (SMatch ([(make_evar (mem_id id))], (List.map make_branch (List.filter (fun id -> LetterMap.exists (fun _ i -> i == id) synthesis_response.whichop) DFASynthesis.regact_ids))))

let make_transition_statements env id idx_expr = 
  let re_info = IdMap.find id env.re_info_map in
  let static_defs = make_static_defs id idx_expr in
  let binding_defs = List.append static_defs (make_binding_defs id re_info.binders env.current_handler env.params_map (make_evar (make_idx_val id))) in
  let pred_defs = (make_pred_defs id re_info.preds env.current_handler) in
  let pred_update_defs = List.append binding_defs ((counter_def id re_info env.current_handler re_info.preds) :: pred_defs) in
  let match_def = List.append pred_update_defs ((res_def id) :: (make_match_def id re_info.alphabet re_info.synthesis_response re_info.event_order re_info.preds)) in
  let match_update_def = List.append match_def ((res_def id) :: [(array_update_def id re_info.synthesis_response)]) in
  let return_def = List.append match_update_def (make_return_def id re_info.synthesis_response) in
  return_def
;;

let make_pred_ids id preds = 
  List.mapi (fun i p -> (statement (SLocal ((make_pred_var_id id p preds), (ty (inferred_size ("predsizeval"^(string_of_int i)))), (extract_sub_exp p))))) preds

let make_transition_statements_inline_preds env id idx_expr = 
  let eid = (match env.current_handler with 
  | None -> error "Not in a handler, but trying to inline preds for a specific event!"
  | Some (eid, _, _) -> eid) in
  let re_info = IdMap.find id env.re_info_map in
  let static_defs = make_static_defs id idx_expr in
  let binding_defs = List.append static_defs (make_binding_defs id re_info.binders env.current_handler env.params_map (make_evar (make_idx_val id))) in
  let pred_ids_defs = List.append binding_defs (make_pred_ids id (List.filter (fun p -> p.pred_event = eid) re_info.preds)) in
  let match_counter_replacement = List.append pred_ids_defs (make_match_def_inline_preds id env.current_handler re_info.alphabet re_info.synthesis_response re_info.event_order re_info.preds) in
  let match_update_def = List.append match_counter_replacement ((res_def id) :: [(array_update_def id re_info.synthesis_response)]) in
  let return_def = List.append match_update_def (make_return_def id re_info.synthesis_response) in
  return_def

let make_counter_branch id re_info eid params= 
  let binding_defs = (make_binding_defs id re_info.binders (Some (eid, None, params)) "dummy" (make_evar (make_idx_val id))) in
  let counter_set = (make_assign (make_counter_id id) (make_num_size (init_val eid re_info.event_order re_info.preds) DFASynthesis.bv_size)) :: binding_defs in
  let pred_update_defs = List.append counter_set (make_pred_defs id re_info.preds (Some (eid,None, params))) in
  let p = [PEvent (Cid.create_ids [eid], params)] in
  let s = statement (sequence_statements pred_update_defs) in
  (p,s) 
;;


let make_trans_match env id idx_expr event_expr = 
  let re_info = IdMap.find id env.re_info_map in
  let static_defs = make_static_defs id idx_expr in
  let counter_def = List.append static_defs [(make_local_synth_var_def (make_counter_id id))] in 
  let match_counter_calc = 
    let counter_update_match = statement (SMatch ([event_expr], (List.map (fun eid -> make_counter_branch id re_info eid (IdMap.find eid env.params_map)) re_info.event_order))) in
    List.append counter_def [counter_update_match] in 
  let match_character_def = List.append match_counter_calc ((res_def id) :: (make_match_def id re_info.alphabet re_info.synthesis_response re_info.event_order re_info.preds)) in
  let match_update_def = List.append match_character_def ((res_def id) :: [(array_update_def id re_info.synthesis_response)]) in
  let return_def = List.append match_update_def (make_return_def id re_info.synthesis_response) in
  return_def

let check_then_set_memop = 
  let id = check_then_set_name in 
  let memval = Id.create "memval" in 
  let newval = Id.create "newval" in
  let params = [(memval, (ty (inferred_size "memvalsize"))); (newval, (ty (inferred_size "newvalsize")))] in
  let body = MBIf((exp (EOp (Eq, [(make_evar memval);(make_num 0)]))), (make_evar newval), (make_evar memval)) in
  (decl (DMemop (id, params, body)))

(*RESETTING CODE*)
let make_reset_event_id id = 
  Id.create (String.cat (fst id) "reset")

let reset_regex_event_decl id = 
  decl (DEvent ((make_reset_event_id id), EBackground, [], [((make_idx_val id), ty (TInt (IConst 32)))]))

let reset_regex_event_handler reid binders = 
  let reset_exp size id = statement (SUnit (exp (ECall (array_set_cid, [(make_evar id);(make_evar (make_idx_val reid)); (make_num_size 0 size)])))) in
  let resets = List.map (fun (_, id, _) -> reset_exp 32 (make_asgn_id reid id)) (List.flatten (List.map (fun b -> b.assignments) binders)) in
  decl (DHandler ((make_reset_event_id reid), HData, ([((make_idx_val reid), ty (TInt (IConst 32)))], statement (sequence_statements (List.append resets [(reset_exp DFASynthesis.bv_size reid)])))))

let reset_regex_stmt id idx_expr = 
  (*let eset = exp (ECall (array_set_cid, [exp (EVar (Cid.create_ids [id])); idx_expr; (make_num_size 0 DFASynthesis.bv_size)])) in
  SUnit(eset)*)
  SGen ((GSingle (None)), exp (ECall (Cid.create_ids [(make_reset_event_id id)], [idx_expr])))

let replacer = 
  object (self)
    inherit [_] s_map as super
    method! visit_DHandler env id sort body = env := {(!env) with current_handler= (Some (id, sort, (fst body)))}; DHandler (id, sort, ((self#visit_body env) body))

    method! visit_ETransitionRegex env id idx ev_expr = 
      match ev_expr with 
      | None -> make_exp_from_statements id ((ans_def id) :: (make_transition_statements_inline_preds !env id idx))
      | Some e -> make_exp_from_statements id ((ans_def id) :: (make_trans_match !env id idx e))

    method! visit_SResetRegex env id idx_expr = 
      reset_regex_stmt id idx_expr
  end
  ;;

let collector = 
  object (self)
    inherit [_] s_map as super
    
    method! visit_DEvent env id event_sort constr_spec_list params = env := {(!env) with params_map=(IdMap.add id params (!env).params_map)}; DEvent (id, event_sort, constr_spec_list, params)
    
    method! visit_DHandler env id sort body = env := {(!env) with current_handler= (Some (id, sort, (fst body)))}; DHandler (id, sort, ((self#visit_body env) body))

    method! visit_DVarRegex env id size alph vr = env := {(!env) with re_info_map=(IdMap.add id (re_info id alph vr) (!env).re_info_map)}; DVarRegex (id, size, alph, vr)

    method! visit_SResetRegex env id idx_expr = (env := {(!env) with needs_reset = true});  SResetRegex (id, idx_expr)
end;;



let replace_var_regex env id size vr = 
  let re_info = IdMap.find id (!env).re_info_map in
  let used_memops = 
    let used regact_int = LetterMap.exists (fun _ i -> i == regact_int) re_info.synthesis_response.whichop in
    (List.filter_map (fun memop_response -> if (used memop_response.regact_int)
      then Some (decl (DMemop (memop_response.id, memop_response.params, memop_response.memop_body))) else None) re_info.synthesis_response.memops) in
  let def = List.append
    (List.map (make_global_def_asgn size id ) (List.flatten (List.map (fun b -> b.assignments) (binders vr))))
    ((make_global_def size (IConst DFASynthesis.bv_size) id) :: used_memops) in
  let tail = if ((!env).added_cts) then def else (env := {(!env) with added_cts = true}; List.append [(check_then_set_memop)] def) in
  if ((!env).added_reset) or (not ((!env).needs_reset)) then tail else (env := {(!env) with added_reset = true};(List.append [(reset_regex_event_decl id); (reset_regex_event_handler id re_info.binders)] tail))
  

;;


let process_prog ds = 
  let env = (ref {re_info_map=IdMap.empty; current_handler=None; params_map=IdMap.empty; added_cts = false; added_reset= false; needs_reset = false}) in 
  let ds = collector#visit_decls env ds in
  let ds = replacer#visit_decls env ds in
    let replace d = 
      match d.d with 
      | DVarRegex (id, size, alph, var_regex) -> replace_var_regex env id (Z.to_int size) var_regex
      | _ -> [d] in
    List.flatten (List.map replace ds)
      


let make_statements_def id = 
  ECall ((Cid.create_ids [Id.create "Array"; Id.create "set"]), [exp (EVar (Cid.create_ids [id])); exp (EVal(vinteger (Integer.of_string "0"))); exp (EVal(vinteger (Integer.of_string "17")))])
;;


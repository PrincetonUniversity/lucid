open Batteries
open Syntax
open SyntaxUtils
open Collections
open PlainRegex

type binder = 
  {
    binding_event : id;
    var_names : id list
  }
and pred_info = 
  {
    pred_event : id;
    pred : exp
  }

and alphabet_letter_def = {ev_id : id ; preds : exp list}
;;

let binders re = 
  let rec binders_acc re acc = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (_, _, _) -> acc
    | VRBinding (event_id, var_names, sub) -> {binding_event=event_id; var_names=var_names} :: binders_acc sub acc
    | VRConcat (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
    | VRClosure sub -> binders_acc sub acc
    | VROr (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
    | VRAnd (sub1, sub2) -> binders_acc sub1 (binders_acc sub2 acc)
  in binders_acc re [];;

let preds re = 
  let rec preds_acc re acc = 
    match re.v_regex with  
    | VREmptySet -> acc
    | VREmptyStr -> acc
    | VRLetter (event_id, var_names, pred) -> {pred_event=event_id; pred=pred} :: acc
    | VRBinding (event_id, var_names, sub) -> preds_acc sub acc
    | VRConcat (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
    | VRClosure sub -> preds_acc sub acc
    | VROr (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
    | VRAnd (sub1, sub2) -> preds_acc sub1 (preds_acc sub2 acc)
  in preds_acc re [];;

let alphabet_def re re_binders re_preds=
  let rec update_ans_list list_of_pair pi = 
    match list_of_pair with 
    [] -> [{ev_id=pi.pred_event; preds=[pi.pred]}]
    | {ev_id=ev_id; preds=preds} :: tail -> if ev_id = pi.pred_event then {ev_id=ev_id; preds=pi.pred ::preds} :: tail else update_ans_list tail pi
  in 
    List.fold_left update_ans_list [] re_preds
;;

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
  | lob :: tail -> PROr(PRSymbol({ev_id = ev_name; bools = lob}), big_union_make_or ev_name tail)
;;
    
let rec big_union event_id var_names pred alphabet = 
  match alphabet with
  [] -> PRSymbol({ev_id=(fst event_id); bools=[]})
  | {ev_id=ev_id; preds=preds} :: tail -> if ev_id = event_id 
    then (big_union_make_or (fst event_id) (big_union_helper pred preds))
    else big_union event_id var_names pred tail
;;

let rec translate re re_alphabet = 
    match re with 
    | VREmptySet -> PREmptySet
    | VREmptyStr -> PREmptyString
    | VRLetter (event_id, var_names, pred) -> big_union event_id var_names (Some pred) re_alphabet
    | VRBinding (event_id, var_names, sub) -> PRConcat(big_union event_id var_names None re_alphabet, translate sub.v_regex re_alphabet)
    | VRConcat (sub1, sub2) -> PRConcat((translate sub1.v_regex re_alphabet),(translate sub2.v_regex re_alphabet))
    | VRClosure sub -> PRClosure(translate sub.v_regex re_alphabet)
    | VROr (sub1, sub2) -> PROr((translate sub1.v_regex re_alphabet),(translate sub2.v_regex re_alphabet))
    | VRAnd (sub1, sub2) -> PRAnd((translate sub1.v_regex re_alphabet),(translate sub2.v_regex re_alphabet))
;;

let sequence_statements ss =
  let seq = List.fold_left sseq snoop ss in
  seq.s
;;

let make_binding_def id = 
  DGlobal (id, TInt(8), EVal(vinteger 0))
;;
(*Each of the below adds a declaration (or many) to the acc*)
let add_binding_defs binders acc = 
  List.fold_left (fun binder acc -> List.append (List.map (fun id -> make_binding_def id) binder.var_names) acc) [] binders
;;


let make_defs binders preds synthesis_response = 
  let binding_defs = (var_defs re_binders) in
    let state_def = (state_def id) in


;;

let replace_var_regex id vr = 
  (*List.iter (function b -> (Printf.printf "%s " (fst b.binding_event)); List.iter (function vn -> Printf.printf "%s " (fst vn)) b.var_names) (binders vr);*)
  let re_binders = binders vr in
    let re_preds = preds vr in
      let re_alphabet = alphabet_def vr re_binders re_preds in 
        let pre = translate vr.v_regex re_alphabet in
          let synthesized = do_synthesis (plain_re_to_dfa pre) in
            let statement = make_defs re_binders re_preds synthesized

        
        print_string (plain_re_to_string pre);


          print_string (List.fold_left String.cat "" (List.map (fun b -> (if b then "1" else "0")) [true; false;true; false]));
  DVarRegex(id, vr)

  (*  let re_binders = binders re in
    let re_preds = preds re in
      let re_alphabet = alphabet_def re re_binders re_preds in*)
;;

let replacer = 
  object (self)
    inherit [_] s_map as super

    method! visit_DVarRegex env id var_regex = replace_var_regex id var_regex
  end
  ;;


let process_prog ds = 
  let ds = replacer#visit_decls () ds in
    List.filter 
      (function 
      | {d = DVarRegex _} -> false
      | _ -> true) ds;;
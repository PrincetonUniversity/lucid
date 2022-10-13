open Batteries
open Syntax
open SyntaxUtils
open Collections
open Arrays

type plain_re_symbol = 
  {
    ev_id : string ;
    bools : bool list
  }

and plain_re = 
  | PREmptyString
  | PREmptySet
  | PRSymbol of plain_re_symbol
  | PROr of plain_re * plain_re
  | PRAnd of plain_re * plain_re
  | PRConcat of plain_re * plain_re
  | PRClosure of plain_re

let rec bool_list_less_than bl1 bl2 = 
  match bl1, bl2 with
  | [], [] -> false
  | head1 :: tail1, head2 :: tail2 ->  if head1 = head2 then bool_list_less_than tail1 tail2 else head1
  | _, _ -> raise (Invalid_argument "Cannot compare lists of different length");;

(*assumes both are in canonical form.
  0 < "" < all symbols < concat < and < or < closure
  concat, and, or are lexicographically ordered using this same relation
  This doesn't mean anything about the sets of the two things, but rather is used to swap the arguments for and/or left-right so they
  can be checked for structural equality.*)
let rec pre_less_than pre1 pre2 = 
  match (pre1, pre2) with 
  | _, PREmptySet -> false
  | PREmptySet, _ -> true
  | _, PREmptyString -> false
  | PREmptyString, _ -> true
  | PRSymbol(prs1), PRSymbol(prs2) -> if prs1.ev_id = prs2.ev_id then bool_list_less_than prs1.bools prs2.bools else (String.compare prs1.ev_id prs2.ev_id) < 0
  | _, PRSymbol(_) -> false
  | PRSymbol(_), _ -> true
  | PRConcat(pre11, pre12), PRConcat(pre21, pre22) 
  | PRAnd(pre11, pre12), PRAnd(pre21, pre22) 
  | PROr(pre11, pre12), PROr(pre21, pre22) -> if pre11 = pre21 then pre_less_than pre12 pre22 else pre_less_than pre11 pre21 
  | _, PRConcat(_, _) -> false
  | PRConcat(_, _), _ -> true
  | _, PRAnd(_, _) -> false
  | PRAnd(_, _), _ -> true
  | _, PROr(_, _) -> false
  | PROr(_, _), _ -> true
  | PRClosure(pre11), PRClosure(pre21) -> pre_less_than pre11 pre21
;;

(*Makes an or but applies all possible similarity rules to it*)
let rec pre_or pre1 pre2 = 
  match (pre1, pre2) with
  | _,_ when pre1 = pre2 -> pre1
  | PREmptySet, _ -> pre2
  | _, PREmptySet -> pre1
  | PROr(pre11, pre12), _ ->  pre_or pre11 (pre_or pre12 pre2)
  | _, _ -> if pre_less_than pre2 pre1 then PROr(pre2, pre1) else PROr(pre1, pre2)
;;

let rec pre_and pre1 pre2 = 
  match (pre1, pre2) with
  | _,_ when pre1 = pre2 -> pre1
  | PREmptySet, _ | _, PREmptySet-> PREmptySet
  | PRAnd(pre11, pre12), _ ->  pre_and pre11 (pre_and pre12 pre2)
  | _, _ -> if pre_less_than pre2 pre1 then PRAnd(pre2, pre1) else PRAnd(pre1, pre2)
;;

let rec pre_concat pre1 pre2 = 
  match (pre1, pre2) with 
  | PREmptySet, _ | _, PREmptySet-> PREmptySet
  | PREmptyString, pre11 | pre11, PREmptyString -> pre11
  | PRConcat(pre11, pre12), _ -> pre_concat pre11 (pre_concat pre12 pre2)
  | _, _ -> PRConcat(pre1, pre2)
;;

let pre_closure pre = 
  match pre with 
  | PREmptySet | PREmptyString | PRClosure (_)-> pre
  | _ -> PRClosure (pre)

let rec pre_symbol s lob = PRSymbol {ev_id=s;bools=lob};;

(*I think this is horribly inefficient. Please do not use regularly; rather just maintain them as canonical using the constructors above.*)
let rec make_canonical pre = 
  match pre with 
  | PROr (pre1, pre2) -> pre_or (make_canonical pre1) (make_canonical pre2)
  | PRAnd (pre1, pre2) -> pre_and (make_canonical pre1) (make_canonical pre2)
  | PRConcat (pre1, pre2) -> pre_concat (make_canonical pre1) (make_canonical pre2)
  | PRClosure (pre1) -> pre_closure (make_canonical pre1)
  | _ -> pre
;;

let rec nullable pre = 
  match pre with 
  | PREmptyString -> PREmptyString
  | PREmptySet | PRSymbol (_) -> PREmptySet
  | PROr (pre1, pre2) -> pre_or (nullable pre1) (nullable pre2)
  | PRAnd (pre1, pre2) | PRConcat (pre1, pre2) -> pre_and (nullable pre1) (nullable pre2)
  | PRClosure (pre) -> PREmptyString
;;

let rec pre_deriv pre prs = 
  match pre with 
  | PREmptyString | PREmptySet -> PREmptySet
  | PRSymbol (prs1) when prs <> prs1 -> PREmptySet
  | PRSymbol (_) -> PREmptyString
  | PROr (pre1, pre2) -> pre_or (pre_deriv pre1 prs) (pre_deriv pre2 prs)
  | PRAnd (pre1, pre2) -> pre_and (pre_deriv pre1 prs) (pre_deriv pre2 prs)
  | PRConcat (pre1, pre2) -> pre_or (pre_concat (pre_deriv pre1 prs) pre2) (pre_concat (nullable pre1) (pre_deriv pre2 prs))
  | PRClosure (pre1) -> pre_or (pre_deriv pre1 prs) pre
;;

let rec plain_re_to_string pre = 
    match pre with 
    | PREmptyString -> "\"\""
    | PREmptySet -> "0"
    | PRSymbol (pres) -> String.cat pres.ev_id  (List.fold_left String.cat "" (List.map (fun b -> (if b then "1" else "0")) pres.bools))
    | PROr (pre1, pre2) -> Printf.sprintf "(%s + %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRAnd (pre1, pre2) -> Printf.sprintf "(%s & %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRConcat (pre1, pre2) -> Printf.sprintf "(%s . %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRClosure (pre) -> Printf.sprintf "(%s)*" (plain_re_to_string pre)
;;

module States = Set.Make(struct type t = plain_re let compare = compare end);;
module Transition = Map.Make(struct type t = plain_re * plain_re_symbol let compare = compare end);;
type dfa = 
   {
    states : States.t;
    alphabet : plain_re_symbol list;
    initial : plain_re;
    transition : plain_re Transition.t;
    accepting : States.t
  };;



let rec explore state acc alphabet =
  let next state acc letter = 
    let statederiv = (pre_deriv state letter) in
      let states, trans = acc in
        let trans = Transition.add (state, letter) statederiv trans in 
          if States.mem state states then 
            (states, trans) 
          else 
            let states = States.add statederiv states in
              (explore state (states, trans) alphabet) in
  List.fold_left (next state) acc alphabet
;;

let plain_re_to_dfa pre alphabet = 
  let init = pre in
    let (states, trans) = explore init ((States.singleton init), Transition.empty) alphabet in
      let accepting = States.filter (fun q -> (nullable q) = PREmptyString) states in
        {
          states=states;
          alphabet=alphabet;
          initial=init;
          transition=trans;
          accepting=accepting
        }
;;

module LetterMap = Map.Make(struct type t = plain_re_symbol let compare = compare end)
type synthesis_response = 
{
  accepting : int list;
  f : int LetterMap.t;
  g : int LetterMap.t;
  whichop : int LetterMap.t;
  memops : decl list;
  init : int
}

let do_synthesis dfa = 
  true;;

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
    | VRLetter (event_id, var_names, pred) -> 
      (match pred.e with 
      (*If it's a value, it has to be a bool*)
      | EVal (v) -> acc
      | _ -> let p_rec = {pred_event=event_id; pred=pred} in 
                if (List.exists (fun pr -> pr.pred.e = p_rec.pred.e) acc) then acc else p_rec :: acc)
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
  | lob :: tail -> pre_or (PRSymbol({ev_id = ev_name; bools = lob})) (big_union_make_or ev_name tail)
;;

let rec big_union event_id var_names pred alphabet = 
  match alphabet with
  [] -> PRSymbol({ev_id=(fst event_id); bools=[]})
  | {ev_id=ev_id; preds=preds} :: tail -> if ev_id = event_id 
    then (big_union_make_or (fst event_id) (big_union_helper pred preds))
    else big_union event_id var_names pred tail
;;

(*
let rec get_letters ev_id preds = 
  let add-to-bools bool lopres = 
    match lopres with 
    | [] -> 
  match preds with
  | [] -> {ev_id=ev_id; bools=[]}
  | p :: tail -> List.append (add-to-bools true tail) (add-to-bools false tail)
;;

let get_all_letters alphabet =
  match alphabet with
  | [] -> []
  | {ev_id=ev_id; preds=preds} :: tail -> List.append (all_letters ev_id preds) (get_all_letters tail)
*)

let rec translate re re_alphabet = 
    match re with 
    | VREmptySet -> PREmptySet
    | VREmptyStr -> PREmptyString
    | VRLetter (event_id, var_names, pred) -> 
      (match pred.e with 
      | EVal (v) -> (match v.v with 
        | VBool b when b-> big_union event_id var_names None re_alphabet
        | _ -> PREmptySet)
      | _ -> big_union event_id var_names (Some pred) re_alphabet)
    | VRBinding (event_id, var_names, sub) -> pre_concat (big_union event_id var_names None re_alphabet) (translate sub.v_regex re_alphabet)
    | VRConcat (sub1, sub2) -> pre_concat (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRClosure sub -> pre_closure (translate sub.v_regex re_alphabet)
    | VROr (sub1, sub2) -> pre_or  (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
    | VRAnd (sub1, sub2) -> pre_and (translate sub1.v_regex re_alphabet) (translate sub2.v_regex re_alphabet)
;;

let sequence_statements ss =
  let seq = List.fold_left sseq snoop ss in
  seq.s
;;

let make_global_def id = 
  DGlobal (id, ty (TName (Arrays.t_id, [(IConst 32)], true)), exp (ECall (Cid.create_ids [Id.create "Array"; Id.create "create"], [exp (EVal(vinteger (Integer.of_string "1")))])))
;;

(*Each of the below adds a declaration (or many) to the acc*)
let add_binding_defs binders acc = 
  List.fold_left (fun acc binder -> List.append (List.map (fun id -> make_global_def id) binder.var_names) acc) [] binders
;;

let make_statements_def id = 
  ECall ((Cid.create_ids [Id.create "Array"; Id.create "set"]), [exp (EVar (Cid.create_ids [id])); exp (EVal(vinteger (Integer.of_string "0"))); exp (EVal(vinteger (Integer.of_string "17")))])
;;

let make_defs id binders preds synthesis_response = 
  let binding_defs = add_binding_defs binders [] in
    let state_def = (make_global_def id) :: binding_defs in
      List.append (synthesis_response.memops) state_def
;;



let replace_var_regex id vr = 
  (*List.iter (function b -> (Printf.printf "%s " (fst b.binding_event)); List.iter (function vn -> Printf.printf "%s " (fst vn)) b.var_names) (binders vr);*)
  let re_binders = binders vr in
    let re_preds = preds vr in
      let re_alphabet = alphabet_def vr re_binders re_preds in 
        let pre = translate vr.v_regex re_alphabet in
          let synthesized = do_synthesis (plain_re_to_dfa pre) in
            (*let statement = make_defs re_binders re_preds synthesized in*)
              print_string (plain_re_to_string pre);
              (*(make_defs id re_binders re_preds synthesized)*)
              make_global_def id
;;

let replacer = 
  object (self)
    inherit [_] s_map as super

    method! visit_DVarRegex env id var_regex = replace_var_regex id var_regex
    
    method! visit_ETransitionRegex env id event = make_statements_def id
  end
  ;;


let process_prog ds = 
  let ds = replacer#visit_decls () ds in
    List.filter 
      (function 
      | {d = DVarRegex _} -> false
      | _ -> true) ds;;
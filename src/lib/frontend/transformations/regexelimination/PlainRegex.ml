open Batteries

(*----------------Plain RE stuff----------------------------*)
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
  | PRNegation of plain_re

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
  | PRNegation (pre11), PRNegation(pre21)
  | PRNegation (pre11), PRClosure (pre21)
  | PRClosure (pre11), PRNegation (pre21)
  | PRClosure(pre11), PRClosure(pre21) -> pre_less_than pre11 pre21
;;

(*Makes an or but applies all possible similarity rules to it*)
let pre_negation pre = 
  match pre with
  | PRNegation(pre1) -> pre1
  | _ -> PRNegation (pre)

let rec pre_or pre1 pre2 = 
  match (pre1, pre2) with
  | _,_ when pre1 = pre2 -> pre1
  | PRNegation (PREmptySet), _
  | _, PRNegation (PREmptySet) -> pre_negation (PREmptySet)
  | PREmptySet, _ -> pre2
  | _, PREmptySet -> pre1
  | PREmptyString, PRClosure (_) -> pre2
  | PRClosure (_), PREmptyString -> pre1
  | PROr(pre11, pre12), _ ->  pre_or pre11 (pre_or pre12 pre2)
  | _, _ -> if pre_less_than pre2 pre1 then PROr(pre2, pre1) else PROr(pre1, pre2)
;;

let rec pre_and pre1 pre2 = 
  match (pre1, pre2) with
  | _,_ when pre1 = pre2 -> pre1
  | PRNegation (PREmptySet), _ -> pre2
  | _, PRNegation (PREmptySet) -> pre1
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

let rec pre_closure pre = 
  match pre with 
  | PREmptySet | PREmptyString | PRClosure (_)-> pre
  | PROr(PREmptyString, pre1) | PROr(pre1, PREmptyString) -> pre_closure (pre1)
  | _ -> PRClosure (pre)

let pre_symbol s lob = PRSymbol {ev_id=s;bools=lob};;

(*I think this is horribly inefficient. Please do not use regularly; rather just maintain them as canonical using the constructors above.*)
let rec make_canonical pre = 
  match pre with 
  | PROr (pre1, pre2) -> pre_or (make_canonical pre1) (make_canonical pre2)
  | PRAnd (pre1, pre2) -> pre_and (make_canonical pre1) (make_canonical pre2)
  | PRConcat (pre1, pre2) -> pre_concat (make_canonical pre1) (make_canonical pre2)
  | PRClosure (pre1) -> pre_closure (make_canonical pre1)
  | PRNegation (pre1) -> pre_negation (make_canonical pre1)
  | _ -> pre
;;

let rec nullable pre = 
  match pre with 
  | PREmptyString -> PREmptyString
  | PREmptySet | PRSymbol (_) -> PREmptySet
  | PROr (pre1, pre2) -> pre_or (nullable pre1) (nullable pre2)
  | PRAnd (pre1, pre2) | PRConcat (pre1, pre2) -> pre_and (nullable pre1) (nullable pre2)
  | PRClosure (_) -> PREmptyString
  | PRNegation (pre1) -> (if (nullable pre1) = PREmptySet then PREmptyString else PREmptySet)
;;

let rec pre_deriv pre prs = 
  match pre with 
  | PREmptyString | PREmptySet -> PREmptySet
  | PRSymbol (prs1) when prs <> prs1 -> PREmptySet
  | PRSymbol (_) -> PREmptyString
  | PROr (pre1, pre2) -> pre_or (pre_deriv pre1 prs) (pre_deriv pre2 prs)
  | PRAnd (pre1, pre2) -> pre_and (pre_deriv pre1 prs) (pre_deriv pre2 prs)
  | PRConcat (pre1, pre2) -> pre_or (pre_concat (pre_deriv pre1 prs) pre2) (pre_concat (nullable pre1) (pre_deriv pre2 prs))
  | PRClosure (pre1) -> pre_concat (pre_deriv pre1 prs) pre
  | PRNegation (pre1) -> pre_negation (pre_deriv pre1 prs)
;;

let print_letter pres =
  String.cat pres.ev_id  (List.fold_left String.cat "" (List.map (fun b -> (if b then "1" else "0")) pres.bools))
;;

let rec plain_re_to_string pre = 
    match pre with 
    | PREmptyString -> "\"\""
    | PREmptySet -> "0"
    | PRSymbol (pres) -> print_letter pres
    | PROr (pre1, pre2) -> Printf.sprintf "(%s + %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRAnd (pre1, pre2) -> Printf.sprintf "(%s & %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRConcat (pre1, pre2) -> Printf.sprintf "(%s . %s)" (plain_re_to_string pre1) (plain_re_to_string pre2)
    | PRClosure (pre) -> Printf.sprintf "(%s)*" (plain_re_to_string pre)
    | PRNegation (pre) -> Printf.sprintf "!(%s)" (plain_re_to_string pre)
;;

(*-----------------Making a DFA---------------------*)
module States = Batteries.Set.Make(struct type t = plain_re let compare = compare end);;
module Transition = Map.Make(struct type t = plain_re * plain_re_symbol let compare = compare end);;
type dfa = 
   {
    states : States.t;
    alphabet : plain_re_symbol list;
    initial : plain_re;
    transition : plain_re Transition.t;
    accepting : States.t
  };;

let print_dfa dfa = 
  print_endline "States";
  States.iter (fun pre -> (print_endline (plain_re_to_string pre))) dfa.states;
  print_endline "Alphabet";
  List.iter (fun l -> print_endline (print_letter l)) dfa.alphabet
;;

let rec explore state acc alphabet =
  let next state acc letter = 
    let statederiv =   Printf.printf "Deriv of %s with %s is %s\n" (plain_re_to_string state) (print_letter letter) (plain_re_to_string (pre_deriv state letter));
      (pre_deriv state letter) in
      let states, trans = acc in
        let trans = Transition.add (state, letter) statederiv trans in 
          if States.mem statederiv states then 
            (states, trans) 
          else 
            let states = States.add statederiv states in
              (explore statederiv (states, trans) alphabet) in
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

let check_continuation letter dfa = 
  States.exists (fun accepting_state -> (pre_deriv accepting_state letter) <> PREmptySet) dfa.accepting

let check_unambiguous_concat pre1 pre2 alphabet = 
  let dfa1 = plain_re_to_dfa pre1 alphabet in
  let dfa2 = plain_re_to_dfa pre2 alphabet in
  let continuations_letters = List.filter (fun l -> (check_continuation l dfa1)) alphabet in
  let prefixes_letters = List.filter (fun l -> (pre_deriv dfa2.initial l) <> PREmptySet) alphabet in
  (List.find_opt (fun l -> List.mem l prefixes_letters) continuations_letters) == None

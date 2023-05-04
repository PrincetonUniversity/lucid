(*
Assumes we have run alpha-renaming, so that variable names inside
each parser are unique

When we compile to the Tofino, each argument to each event is assigned a particular
"slot" in memory. The parser writes the relevant data into that slot, and the handler
reads from it. To avoid wasting stages by moving data around in the pipeline,
whenever the parser reads from the packet it puts the data in the appropriate slot,
e.g. in the code below, e1 is read directly into the slot assigned to the first
argument of foo:
```
read e1 : eth;
generate foo(e1);
```

However, this can cause problems if we need to store multiple variables in the same
slot, as below:
```
read e1 : eth;
read e2 : eth;
match ... with
| 0 -> generate foo(e1);
| 1 -> generate foo(e2);
```

In this case, both e1 and e2 are "destined" to be the first argument to foo, and so
they must both be stored in the appropriate slot. This is a problem, since we can't
keep both of them alive at once: e2 will overwrite e1 (and in fact, the overwriting
will be incomplete, so the resulting value will be e1 <bitwise or> e2). We could fix
the problem by moving the reads inside the match, so that e1 and e2 don't appear along
the same control path.
```
match ... with
| 0 -> read e1 : eth; generate foo(e1);
| 1 -> skip eth; read e2 : eth; generate foo(e2);
```

The other thing that can go wrong is trying to use the same variable as different
arguments to the same event:
```
read e1 : eth;
match ... with
| 0 -> generate foo(e1, ...);
| 1 -> generate foo(..., e1);
```

Doing this means that the first and last arguments of foo must be stored in the
same slot, which is impossible: we need both to be alive at the same time.
These sorts of errors can also happen transitively:
```
match ... with
| 0 -> read e1 : eth;
       match ... with
      | 1 -> generate foo(e1);
      | 2 -> generate bar(e1, ...);
| 3 -> read e2: eth;
       match ... with
      | 4 -> generate foo(e2);
      | 5 -> generate bar(..., e2);
```

In the above example, the 0 branch tells us that the first argument to foo and
bar must be stored in the same slot (which is acceptable, since they are different
events and hence their arguments are never needed at the same time). Then the 3
branch tells us that the first argument to foo is the same as the second argument
to bar, which transitively means that the first and second arguments to bar are
the same; an error.

The goal of this file is to detect these sorts of "slot errors", by computing the
sets of things that must go in the same slot. We consider two types of "things that
go in slots": event parameters, and variables inside parsers (including the parameters
to the parser). These sets form equivalence classes; we then ensure that no class
contains multiple arguments to the same event, or multiple variables inside a parser
that appear along the same control path.

Our strategy is inspired by unification-based type inference; however, we keep
additional information so we can provide the user with good error messages. We
assign each "slotted thing" a fresh "slot variable" (analogous to an unbound
type variable, or TVar in our abstract syntax). When an event is generated or
a parser is called, we unify the slot variables of each argument with the ones
for the parameters, and check to make sure we haven't created an illegal
equivalence class.

Unlike type inference, we keep around additional information. Each slot variable
stores the equivalence class of things that have been unified with it, and we use
refs in the same way we use TVars in type inference to allow efficient unification.
This lets us report to the user which things got unified, in case of an error. We
also return a map of which objects are stored in which slots, for use in the backend.
*)
open Syntax
open SyntaxUtils
open Batteries
open Collections

(* Syntax helper function: we expect every PGen or PCall to to be
   a call to the event/parser whose arguments are variables. *)
let extract_call e =
  let extract_var e =
    match e.e with
    | EVar cid -> Cid.to_id cid
    | _ -> failwith "slotAnalysis: expected a variable in event/parser argument"
  in
  match e.e with
  | ECall (cid, es) -> cid, List.map extract_var es
  | _ -> failwith "slotAnalysis: expected a call in parser"
;;

(* There are two types of object that can go in a slot:
   1.) The nth parameter to event or parser foo, or
   2.) The local variable x in parser p
*)
type slotted_object =
  | Param of cid (* event/parser name *) * int (* parameter position *)
  | Var of cid (* parser name *) * id (* variable name *)

module SlotMap = Map.Make (struct
  type t = slotted_object

  let compare = Pervasives.compare
end)

module SlotSet = Set.Make (struct
  type t = slotted_object

  let compare = Pervasives.compare
end)

(* For the purposes of our analysis, a "slot" is an equivalence
   class of objects which we have determined must occupy the same
   slot. We include a ref constructor as well for efficient
   unification; this is analogous to the way we use TVars during
   type inference.

   During our analysis, we have an invariant that each slot value
   in the environment is a chain of refs, never a bare set.
*)
type slot =
  | Set of SlotSet.t
  | Ref of slot ref

(* Strip refs and return the underlying set *)
let rec extract_set slot =
  match slot with
  | Ref { contents = slot } -> extract_set slot
  | Set s -> s
;;

(* Update the last ref in the chain *)
(* TODO: We could also get rid of the other refs, since we only
   ever care about the last one. *)
let rec set_last_ref slot v =
  match slot with
  | Ref { contents = Ref _ as slot } -> set_last_ref slot v
  | Ref ({ contents = _ } as r) -> r := v
  | Set _ -> failwith "slotAnalysis: Should always have ref chains"
;;

let rec get_last_ref slot =
  match slot with
  | Ref { contents = Ref _ as slot } -> get_last_ref slot
  | _ -> slot
;;

(* This maps slotted objects to the slot they are stored in, i.e.
   the set of other slotted objects they've been unified with.
   Invariants:
   - Every entry is a Ref, never a Set
   - No Param should be mapped to a different Param with the same cid
   - No Var should be mapped to a different Var which is assigned/read
     on the same control path, or to a Param with the same cid
*)
type slot_map = slot SlotMap.t

(* During the analysis, we maintain an environment containing:
   - The current slot map
   - The set of variables which are currently alive, and their associated
     slotted_object (this is empty unless we're inside a parser)
   - A map from each event/parser's name to the names of its
     arguments (for use when printing error messages)
*)
type env =
  { slots : slot_map
  ; live_vars : slotted_object IdMap.t
  ; params : id list CidMap.t
  }

let empty_env =
  { slots = SlotMap.empty; live_vars = IdMap.empty; params = CidMap.empty }
;;

(* Printing functions for error messages and debugging *)
let slotted_object_to_string env obj =
  match obj with
  | Param (cid, n) ->
    let args = CidMap.find cid env.params in
    let id = List.nth args n in
    Printf.sprintf
      "Parameter `%s` to event/parser `%s`"
      (Printing.id_to_string id)
      (Printing.cid_to_string cid)
  | Var (cid, id) ->
    Printf.sprintf
      "Variable `%s` in parser `%s`"
      (Printing.id_to_string id)
      (Printing.cid_to_string cid)
;;

let slot_to_string env slot =
  let set = extract_set slot in
  Printf.sprintf "{\n%s}\n"
  @@ SlotSet.fold
       (fun obj acc -> acc ^ slotted_object_to_string env obj ^ ";\n")
       set
       ""
;;

(* For debugging *)
let env_to_string env =
  Printf.sprintf
    "{live_vars: {\n%s\n}\nslot_map: {\n%s\n}\n}"
    (IdMap.fold
       (fun id obj acc ->
         Printf.sprintf
           "%s%s -> %s;\n"
           acc
           (Printing.id_to_string id)
           (slotted_object_to_string env obj))
       env.live_vars
       "")
    (SlotMap.fold
       (fun obj slot acc ->
         Printf.sprintf
           "%s%s -> %s\n"
           acc
           (slotted_object_to_string env obj)
           (slot_to_string env slot))
       env.slots
       "")
;;

(* Make sure that our invariants are satisfied in an environment
   where slotted_object points to set, and report and error if not *)
let validate_unification env set obj =
  (* Since the Set module find_first_opt requires the predicate to be
     monotonic, we have to roll our own (inefficient) version. This returns the
     first element of set to satisfy the predicate p, or None if none exists *)
  let search_set p =
    SlotSet.fold
      (fun obj acc ->
        match acc with
        | Some _ -> acc
        | None -> if p obj then Some obj else None)
      set
      None
  in
  let bad_object =
    match obj with
    | Param (cid, n) ->
      (* Ensure we haven't unified two parameters of the same thing, or a parameter
         of a parser with a variable in that parser *)
      search_set (fun obj ->
        match obj with
        | Param (cid', n') -> Cid.equal cid cid' && n <> n'
        | Var (cid', _) -> Cid.equal cid cid')
    | Var (cid, id) ->
      (* Ensure we haven't unified two variables on the same control path of a
         parser, or a variable in a parser with one of its parameters *)
      search_set (fun obj ->
        match obj with
        | Param (cid', _) -> Cid.equal cid cid'
        | Var (cid', id') ->
          Cid.equal cid cid'
          && (not (Id.equal id id'))
          && IdMap.mem id' env.live_vars)
  in
  match bad_object with
  | None -> ()
  | Some bad_obj ->
    Console.error
    @@ Printf.sprintf
         "Slot Analysis determined that \n\
          %s \n\
          and \n\
          %s \n\
          must share the same slot.\n\
          Variables and parameters assigned to this slot:\n\
          %s."
         (slotted_object_to_string env obj)
         (slotted_object_to_string env bad_obj)
         (slot_to_string env (Set set))
;;

(* Given two objects, unify their slots. We do so by unioning the sets of objects
   they point to, and then redirecting the last ref of obj1 to point at obj2.
   This redirection means that any future updates to either obj1 or obj2 will
   affect both of them automatically. *)
let unify env obj1 obj2 =
  let slot1 = SlotMap.find obj1 env.slots in
  let slot2 = SlotMap.find obj2 env.slots in
  let unified = SlotSet.union (extract_set slot1) (extract_set slot2) in
  validate_unification env unified obj1;
  validate_unification env unified obj2;
  set_last_ref slot1 slot2;
  set_last_ref slot2 (Set unified);
  env
;;

let add_slotted_obj env obj =
  let slot = Ref (ref (Set (SlotSet.singleton obj))) in
  { env with slots = SlotMap.add obj slot env.slots }
;;

(* Create new slotted objects for each parameter of an event or parser, and
   store the argument ids in the environment so we can look them up when printing. *)
let create_param_slots env cid params in_parser =
  let env =
    List.fold_lefti
      (fun env n (id, _) ->
        let obj = Param (cid, n) in
        let env = add_slotted_obj env obj in
        if in_parser
        then { env with live_vars = IdMap.add id obj env.live_vars }
        else env)
      env
      params
  in
  { env with params = CidMap.add cid (List.map fst params) env.params }
;;

(* Create a new slotted object for a variable defined inside a parser, and add
   it to the live variable map *)
let create_var_slot env cid id =
  let obj = Var (cid, id) in
  let env = add_slotted_obj env obj in
  { env with live_vars = IdMap.add id obj env.live_vars }
;;

(* The actual analysis: walk through each parser, adding variables to the
   environment when they are defined (i.e. read), and unifying them with
   event/parser arguments when those are called *)
let rec analyze_parser_action env cid action =
  (* print_endline @@ "ANALYZING " ^ Printing.parser_action_to_string action;
  print_endline @@ env_to_string env; *)
  match action with
  | PRead (id, _) -> create_var_slot env cid id
  | PAssign _ -> (* FIXME: Not sure what to do here *) env
  | PSkip _ -> env

and analyze_parser_step env cid step =
  (* print_endline @@ "ANALYZING " ^ Printing.parser_step_to_string step;
  print_endline @@ env_to_string env; *)
  match step with
  | PMatch (_, branches) ->
    List.fold_left
      (fun env (_, branch) ->
        (* Keep all slot information we've learned, but forget about variables
           that were declared in the branch (they're no longer alive) *)
        let env' = analyze_parser_block env cid branch in
        { env with slots = env'.slots })
      env
      branches
  | PGen e | PCall e ->
    (* Extract the arguments, and unify each one with the slot of the
       corresponding parameter *)
    let f_cid, args = extract_call e in
    let env =
      List.fold_lefti
        (fun env n arg ->
          let param_obj = Param (f_cid, n) in
          let arg_obj = IdMap.find arg env.live_vars in
          unify env param_obj arg_obj)
        env
        args
    in
    env
  | PDrop -> env

and analyze_parser_block env cid (actions, (step, _)) =
  let env =
    List.fold_left
      (fun env (action, _) -> analyze_parser_action env cid action)
      env
      actions
  in
  let env = analyze_parser_step env cid step in
  env
;;

let analyze_parser env cid params parser_block =
  let env = create_param_slots env cid params true in
  let env = analyze_parser_block env cid parser_block in
  { env with live_vars = IdMap.empty }
;;

(* Once we've finished our analysis, if we didn't get any errors, we can turn
   our slot map into a more useful form for the backend, by assigning each slot
   (i.e. each equivalence class of objects) an integer identifier.
*)
let assign_int_slotids env =
  let count = ref (-1) in
  (* We could map over the slotmap, but that would require a lot of comparing sets.
     To try to be a little more efficient, instead we fold, and add each element
     of each set to the output map as we encounter them *)
  SlotMap.fold
    (fun _ slot acc ->
      let set = extract_set slot in
      (* If we've already gone over this set, don't do it again *)
      if SlotMap.mem (SlotSet.any set) acc
      then acc
      else (
        incr count;
        SlotSet.fold (fun obj acc -> SlotMap.add obj !count acc) set acc))
    env.slots
    SlotMap.empty
;;

let analyze_prog ds =
  let v =
    object (self)
      inherit [_] s_iter

      (* If we're in a module, add this prefix to all parser/event names *)
      val mutable prefix = []

      method! visit_DModule env id _ body =
        let old_prefix = prefix in
        prefix <- prefix @ [id];
        self#visit_decls env body;
        prefix <- old_prefix

      method! visit_DEvent env id _ _ params =
        let new_env =
          create_param_slots !env (Cid.create_ids (prefix @ [id])) params false
        in
        env := new_env

      method! visit_DParser env id params body =
        let new_env =
          analyze_parser !env (Cid.create_ids (prefix @ [id])) params body
        in
        env := new_env
    end
  in
  let env = ref empty_env in
  v#visit_decls env ds;
  assign_int_slotids !env
;;

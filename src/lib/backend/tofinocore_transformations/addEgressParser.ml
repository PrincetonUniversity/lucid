(* this pass generates a parser for the egress component of a TofinoCore program... *)

open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
open AddIngressParser
[@@@ocaml.warning "-21-27-26"]
(*  

What does the final egress parser look like? 
we are parsing the ingress's output: 
a union
  (with tag)
  of sets
    (with each set having flag fields)
    of events
*)

let parser pid pparams pblock = 
  TDParser({pid; pparams; pblock; poutput=None;})
;;

(*  *)
;;
let idxof = 
  let rec _idxof idx id events = 
    match events with 
    | [] -> error "[id_to_pos] event with this id not found"
    | event::events ->     
      if (Id.equal (id_of_event event) id)
        then idx
        else (_idxof (idx+1) id events)
  in
  _idxof 0  
;;

(* given a bitvec of length k with n 1's, 
   generate an associative list (n, idx)
   where idx is the index of the nth 1 in the bitvec    
  e.g.: [1; 0; 1; 1] -> [(0, 0); (1, 2); (2, 3); (3, 4)] *)
let get_n_to_idx_map bitvec : (int * int) list = 
  let assoc, _, _ = List.fold_left
    (fun (assoc, pos, n_ones) bit ->  
      if (bit = 1)
        then (assoc@[n_ones, pos], pos+1, n_ones+1)
        else (assoc, pos+1, n_ones)      
      )
    ([], 0, 0)
    bitvec
  in
  assoc
;;

let block_of_nth_member out_ctor_base (members : event list) n = 
  let event = List.nth members n in
  let params = match event with 
    | EventSingle({evparams=evparams;}) -> evparams
    | _ -> error "[not a single event]"
  in
  (* read the event parameters *)
  let read_cmds = List.map read_id params in
  (* the generate expression uses the event constructor for _egress_, 
     i.e., not where members came from. *)
  let gen_cmd = pgen (
    call 
    (Cid.concat out_ctor_base (Cid.id (id_of_event event))) 
    (List.map (fun (id, ty) -> var (Cid.id id) ty) params)
    (ty TEvent)  
  )
  in
  block read_cmds gen_cmd
;;

(* given a list of members of an eventset and the 
   subsets of those members that are active, 
   generate the parser branches to extract the 
   parameters and generate the event for each case. 
   event_ctor_cid is the fully qualified cid 
   of the event to generate. *)
let branches_of_subsets out_ctor_base members subsets = 
  (* return an integer list of length |events| that
     contains a 1 for each event in events that is 
     in subset. Use "idxof events id" to get the 
    index of "id" in the event list *)
  let subset_to_bitvec (subset : id list) : int list =
    (* make a bitvector of length events with all 0's. *)
    (* for each event in subset, set the corresponding bit to 1 *)
    List.fold_left 
      (fun bitvec id -> 
        let idx = idxof id members in
        List.mapi (fun i bit -> if (i = idx) then 1 else bit) bitvec
      )
      (List.map (fun _ -> 0) members)
      subset
  in
  (* each bitvec indicates which members are active *)
  let subset_bitvecs = List.map subset_to_bitvec subsets in
  let branches = List.fold_left 
    (fun branches bitvec -> 
      let n_to_idx = get_n_to_idx_map bitvec in
      let new_branches = List.map 
        (fun (copy_num, event_idx) -> 
          pbranch
            (copy_num::bitvec)
            (block_of_nth_member out_ctor_base members event_idx))
        n_to_idx
      in
      branches@new_branches)
    []
    subset_bitvecs
  in
  branches
;;

(* make a block to parse a serialized eventset, 
   extract the nth_var event in the set, 
   and generate the event evout.members[idxof(nth_var)]...
   oof this is complicated. *)
let eventset_block evset (nth_var : (cid * ty)) (out_ctor_base : cid) = 
  match evset with 
  | EventSet({flags;members; subsets;}) -> (
    (* match on replica_id::flags *)
    let match_exps = List.map 
      (fun (cid, ty) -> var cid ty)
      ([nth_var]@(List.map (fun (id, ty) -> Cid.id id, ty) flags))
    in
    (* for each subset in subsets, make per-replica_id rules.
        for each rule, make a block that read the parameters and generates the event *)  
    let branches = branches_of_subsets out_ctor_base members subsets in
    block
      (List.map read_id flags)
      (pmatch match_exps branches)  
  )
  | _ -> error "not an eventset"
;;

let tname id = ty (TName(id, [], false)) ;;

let id = Id.create
(* make a parser that reads a serialized version of 
   from_ingress, prefixed with an intrinsic header 
   that encodes the id of the event to be extracted, 
   and generates the to_egress event. *)
let make_egr_parser 
  (from_ingress : event) 
  (to_egress : event) = 
    (* first, produce 
      egr_start(egr_intr_md_t egr_intr_md) {
        read egr_intr_md : egr_intr_md_t;
        egr_main(egr_intr_md.rid);
      } 
    *)
  print_endline ("members of from_ingress: ");
  (match from_ingress with 
    | EventUnion({members;}) -> 
      TofinoCorePrinting.line_sep (TofinoCorePrinting.event_to_string) (members) |> print_endline
    | _ -> error "nope"
  );
  let egr_intrinsic = List.hd egress_parser_intrinsics in
  let egress_intr_param = egr_intrinsic.pid, tname (Cid.id egr_intrinsic.pty.tyid) in 

  let tagid, tagty = etag from_ingress in

  let read_intr_cmd = read (fst egress_intr_param |> Cid.id) (snd egress_intr_param) in
  let read_event_tag = read (Cid.id tagid) tagty in
  let current_set_member_cidty = param_to_cidty egr_intrinsic "egress_rid" in 
  let egr_parser = parser
    (id "main")
    ([egress_intr_param])
    (block
      [
        read_intr_cmd;
        read_event_tag
      ]
      (pmatch [(var (Cid.id tagid) tagty)]
        (List.map (fun (tagval, event) ->  
          pbranch 
            [tagval] 
            (eventset_block 
              event 
              current_set_member_cidty 
              (id_of_event to_egress |> Cid.id)))  
          (etagged_members from_ingress) 
        )      
      )
    )
  in
  egr_parser
;;

(*** generate elimination -- this will apply to the ingress parser too ***)

(* given a list of events, find the one that matches cid *)
let rec cid_to_eventconstr prefix_ids events cid =
  match cid with 
  | Cid.Id(id) -> (
    match (List.find_opt (fun event -> Id.equal id (id_of_event event)) events) with
    | Some(event) -> prefix_ids, event
    | None -> error "[cid_to_event] outer component of cid did not match any event"
  )
  | Cid.Compound(id, cid) -> (
    (* the outer component must match one of the events *)
    let outer_event = match (List.find_opt (fun event -> Id.equal id (id_of_event event)) events) with
    | Some(event) -> event
    | None -> error "[cid_to_event] outer component of cid did not match any event"
    in
    cid_to_eventconstr (prefix_ids@[id]) (members_of_event outer_event) cid
  )      
;;

let eliminate_parser_generates to_egress_event parser = 
  (* eliminate generate statements in the parser by replacing them with 
     assignments to the parameters of the event. *)
  let v = object
    inherit [_] s_map as super
    method! visit_parser_block () (parser_actions_spans_list, (parser_step, sp))= 
      match parser_step with 
      | PGen(gen_exp) -> (
        (* this is a generate. So now we need to find the constructor for 
           the event, get its parameters, append assign statements to 
           set the parameters, and finally replace the PGen with an exit, 
           (perhaps something like PCall continue()) *)
        match gen_exp.e with 
        | ECall(evconstr_cid, eargs) -> 
          (* find the base event being generated *)
          let event_prefix, base_event = cid_to_eventconstr [] [to_egress_event] evconstr_cid in
          (* get the parameters, fully scoped *)
          let prefix_cid = Cid.create_ids event_prefix in
          let params = params_of_event base_event in
          let params = List.map 
            (fun (id, ty) -> 
              Cid.concat 
                prefix_cid 
                (Cid.create_ids [id_of_event base_event; id]),
              ty) 
            params 
          in

          (* optimized: make peek / skip actions to set all the params, 
             then replace the actions in the generate block with the new ones. *)

          let peek_skip_actions = List.fold_left
            (fun actions (cid,ty) -> 
              actions@[
                PPeek(cid, ty), Span.default;
                PSkip(ty), Span.default                
                ]
              )
            []
            params
          in
          let parser_actions_spans_list = peek_skip_actions in 
          (* make assign actions to set all the params *)
          (* let assign_actions = List.map2
            (fun cid earg -> PAssign(cid, earg), Span.default)
            params
            eargs
          in *)
          (* update the actions list *)
          (* let parser_actions_spans_list = parser_actions_spans_list@assign_actions in *)
          (* replace the PGen with an exit call*)
          let exit_call = call (Cid.create ["exit"]) [] (ty TEvent) in
          let parser_step = PCall(exit_call) in
          (super#visit_parser_block () (parser_actions_spans_list, (parser_step, sp)) : (parser_action * sp) list * (parser_step * sp))
        | _ -> error "parser generate should take an expression of variant ECall"
      )
      | _ ->  
      super#visit_parser_block () (parser_actions_spans_list, (parser_step, sp))
    end
  in
  v#visit_td () parser
;;

(*** speculative parsing -- this is now just for ingress ***)

(* 
  given an assign action where the right hand side is a variable created 
  by a previous read action, add a peek action before the read action 
  and delete the assign action.

  e.g.: 

  {
      read foo_a : int<<32>>;
      read foo_b : int<<32>>;
      egress_input.foo.foo_a = foo_a;
      egress_input.foo.foo_b = foo_b;
      exit();}
  --> 
  {
      peek egress_input.foo.foo_a : int<<32>>;
      read foo_a : int<<32>>;
      peek egress_input.foo.foo_b : int<<32>>;
      read foo_b : int<<32>>;
      exit();}
*)
let speculative_parsing parser = 
  error "todo"
(* this optimization will be important for the ingress parser, 
   but not for egress because we generate optimized code. *)

;;

let add_parser core_prog : prog =   
  print_endline ("---- next up.... PARSING! ----");
  (* we are generating a parser that read the ingress output 
     event and generates the egress input event *)
  let ingress_output_event = List.filter_map
    (fun comp -> 
      if (comp.comp_sort = HData)
        then Some((main_handler_of_component comp).hdl_output)
        else None  )
    core_prog
  |> List.hd
  in
  let egress_input_event = List.filter_map
  (fun comp -> 
    if (comp.comp_sort = HEgress)
      then Some((main_handler_of_component comp).hdl_input)
      else None  )
    core_prog
  |> List.hd
  in
  let egr_parser = make_egr_parser ingress_output_event egress_input_event in 
  print_endline ("---- egress parser ----");
  print_endline (TofinoCorePrinting.td_to_string egr_parser);
  print_endline ("-----------------------");
  let egr_parser = eliminate_parser_generates egress_input_event egr_parser in 
  print_endline ("---- egress parser after generate elimination ----");
  print_endline (TofinoCorePrinting.td_to_string egr_parser);
  print_endline ("-----------------------");
  core_prog
;;
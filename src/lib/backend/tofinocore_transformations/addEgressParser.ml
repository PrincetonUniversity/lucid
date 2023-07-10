(* this pass generates a parser for the egress component of a TofinoCore program... *)

open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
open AddIngressParser
[@@@ocaml.warning "-21-27-26"]

let parser pid pparams pblock pevent outparams = 
  TDParser({pid; pparams; pblock; pret_event=Some(pevent); pret_params = outparams;})
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
   out_ctor_base is the fully qualified cid 
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
  (* each bitvec indicates which members are active in the given eventset *)
  let subset_bitvecs = List.map subset_to_bitvec subsets in
  (* create parse branches for each bitvec / eventset form *)
  let branches = List.fold_left 
    (fun branches bitvec -> 
      (* a mapping from each replica id to... ???  *)
      let (rid_to_bitvec_idx : (int * int) list) = get_n_to_idx_map bitvec in
      let new_branches = List.map 
        (fun (copy_num, event_idx) -> 
          pbranch
            (copy_num::bitvec)
            (block_of_nth_member out_ctor_base members event_idx))
        rid_to_bitvec_idx
      in
      branches@new_branches)
    []
    subset_bitvecs
  in
  branches
;;

(* 

foo, port; bar, self


*)

let flag_val_of_event gen_seq event =
  match (List.assoc_opt (id_of_event event) gen_seq) with
  | None -> 0
  | Some(GPort(_)) -> 2
  | Some(GMulti(_)) -> 2
  | Some(GSingle(None)) -> 1
  | _ -> error "[flag_val_of_event] not sure what to do with a generate type GSingle(Some(...))"
;;


(* write a command to skip an event's parameters *)
let pskip_event_params event = 
  let event_size = List.fold_left 
    (fun sz (_, param_ty) -> 
      sz + size_of_tint param_ty)
    0
    (params_of_event event)
  in
  [PSkip(ty (TInt(event_size)))];
;;
(* write a command to read an event's parameters *)
let pread_event_params event = 
  let read_cmds = List.map read_id (params_of_event event) in
  read_cmds
;;
(* write a command to generate an event, assuming that 
   its parameters were read in the saem block by pread_event *)
let pgen_event out_ctor_base event = pgen (
  call 
  (Cid.concat out_ctor_base (Cid.id (id_of_event event))) 
  (List.map (fun (id, ty) -> var (Cid.id id) ty) (params_of_event event))
  (ty TEvent)  
)
;;


(* construct a parser block to read active event params, 
   skip other events, then generate the event. *)
let make_extract_event_block out_ctor_base members gen_seq active_event_id = 
  let pblock_egr actions step = 
    {
      pactions = List.map (fun a -> (a, Span.default)) actions;
      pstep = step, Span.default;  
    }
  in
  
  let active_event = List.find 
    (fun e -> Id.equal (id_of_event e) active_event_id)
    members
  in
  (* for each member: 
      if it is the active event, read its params
      else, if it is in gen_seq, skip its params
      else, ignore its params totally *)
  let actions = List.fold_left (fun actions member -> 
    if (Id.equal (id_of_event member) active_event_id)
      then (actions@(pread_event_params member))
      else if (List.mem_assoc (id_of_event member) gen_seq)
        then (actions@(pskip_event_params member))
        else actions
    )
    []
    members
  in
  pblock_egr actions (pgen_event out_ctor_base active_event)
    
  (* let param_actions = List.fold_left 
    (fun param_actions member -> 
      if (Id.equal (id_of_event member) active_event_id)
        then (param_actions@(pread_event_params member))
        else (param_actions@(pskip_event_params member)))
    []
    members
  in
  pblock_egr param_actions (pgen_event out_ctor_base active_event) *)
;;



type generate_seq = (id * gen_type) list
let string_of_generate_seq gs = 
  let string_of_gen_type = function
    | GPort(_) -> "port"
    | GMulti(_) -> "multi"
    | GSingle(None) -> "local"
    | GSingle(Some(_)) -> "self"
  in
  let string_of_gen_seq = function
    | (id, gen_type) -> 
      "generate_"^(string_of_gen_type gen_type)^"("^(fst id)^")"
  in
  String.concat ", " (List.map string_of_gen_seq gs)
;;
let branches_of_generated_events out_ctor_base (members : event list) (gen_seqs : generate_seq list) : parser_branch list = 
  (* each generate sequence turns into n rules -- one rule for each member *)
    (*
    here's roughly what we'd do in a friendly programming language 
    like python or c or assembly:
      flag_values = []
      for evid in members:
        gen_ty = gen_seq.get(evid, None)
        if (gen_ty == local):
          flag_values.append(1)
        elif (gen_ty == port):
          flag_values.append(2)
        else:
          flag_values.append(0)
      
      replica_id = 1
      replica_ids = []
      actions = []
      for event_id, gen_type in gen_seq:
        if (gen_type == port):
          replica_ids.append(0)
          actions.append([skip e for e in members if e != event_id else read e])
        else:
          replica_ids.append(replica_id)
          replica_id += 1
    *)
  (*construct the branches to parse a single generate sequence (each sequence has 1 branch for each replica) *)
  (* print_endline ("[branches_of_generated_events] out_ctor_base: "^(Cid.to_string out_ctor_base));
  print_endline ("[branches_of_generated_events] members: "^(String.concat ", " (List.map (fun m -> id_of_event m |> fst) members))); *)
  let branches_of_gen_seq (gen_seq : generate_seq) = 
    (* print_endline ("[branches_of_gen_seq] gen_seq: "^(string_of_generate_seq gen_seq)); *)
    (* there is 1 branch for each event in the sequence, because there's one branch for each replica. *)
    (* flag vals tells us which events are in the packet, so its the same for all rules *)
    let flag_vals = List.map (flag_val_of_event gen_seq) members in 
    (* print_endline ("[branches_of_gen_seq] initial flag_vals: "^(String.concat ", " (List.map string_of_int flag_vals))); *)
    (* now, we make a rule to extract each event in the sequence, 
        for the replica corresponding to this event.  *)
    let new_evnum = ref 0 in 
    (* make a single branch, for a single replica of the event event_id in a set of events "members" 
        where "gen_seq" tells you the other members in the current value of the set... *)
    let branch_of_gen (event_id, gen_ty) = 
      let block = make_extract_event_block out_ctor_base members gen_seq event_id in 
      let replica_val = match (gen_ty) with 
        | GSingle(None)-> (
          (* start at 1 and increment every
             time there's a recirc event (...conveniently named GSingle(None)...) *)
             new_evnum := (!new_evnum) + 1;
          !new_evnum
        )
        | GSingle(Some(_)) -> error "idk"
        | _ -> 0 (* this is a port or ports generate *)
      in
      let pattern = List.map 
        (fun p -> PNum(Z.of_int p))
        (flag_vals@[replica_val])
      in
      pattern, block
    in
    List.map branch_of_gen gen_seq
  in

  List.map branches_of_gen_seq gen_seqs |> List.flatten
;;


(* make a block to parse a serialized eventset, 
   extract the nth_var event in the set, 
   and generate the event evout.members[idxof(nth_var)]...
   oof this is complicated. *)
let eventset_block evset (rid_var : (cid * ty)) (out_ctor_base : cid) = 
  (* print_endline ("[eventset_block] evset id: "^(id_of_event evset |> fst)); *)
  match evset with 
  | EventSet({flags;members; generated_events;}) -> (
    let _, flag_fields, pad_opt = flags in 
    (* read the flags to locals, skip the padding *)
    let flag_actions = List.map read_id flag_fields in 
    let actions = match pad_opt with 
      | None -> flag_actions
      | Some(pad) -> flag_actions@[skip (snd pad)]
    in  
    (* match on replica_id::flags *)
    let match_exps = List.map 
      (fun (cid, ty) -> var cid ty)
      ((List.map (fun (id, ty) -> Cid.id id, ty) flag_fields)@[rid_var])
    in
    (* for each subset in subsets, make per-replica_id rules.
        for each rule, make a block that read the parameters and generates the event *)  
    (* let branches = branches_of_subsets out_ctor_base members subsets in *)
    let branches = branches_of_generated_events out_ctor_base members generated_events in
    block
      actions
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

      parser main() returns(event egress_input, egress_intrinsic_metadata_t egress_intrinsic_metadata) {
        read egress_intrinsic_metadata : egress_intrinsic_metadata_t;
        read tag : int<16>;
        match tag with 
        // ...
      }
    *)
  let egr_intr_id, egr_intr_ty = intrinsic_to_param egress_intrinsic_metadata_t in 

  (* note_ tagid does not matter. *)
  let local_tag_id = Id.create "ingress_union_tag" in
  let _, (_, tagty) = etag from_ingress in

  let read_intr_cmd = read (Cid.id egr_intr_id) egr_intr_ty in
  let read_event_tag = read (Cid.id local_tag_id) tagty in
  let egress_replica_id = field_of_intrinsic 
    egress_intrinsic_metadata_t 
    (Cid.id egr_intr_id)
    (Cid.create ["egress_rid"])
  in    
  let egr_parser = parser
    (id "main")
    ([])
    (block
      [
        read_intr_cmd;
        read_event_tag
      ]
      (pmatch [(var (Cid.id local_tag_id) tagty)]
        (List.map (fun (tagval, event) ->  
          pbranch 
            [tagval] 
            (eventset_block (* one block for each handler that may have run in ingress *)
              event 
              egress_replica_id 
              (id_of_event to_egress |> Cid.id)))  
          (etagged_members from_ingress) 
        )      
      )
    )
    to_egress
    (List.map intrinsic_to_param [egress_intrinsic_metadata_t;]) 
      (* egress_intrinsic_metadata_from_parser_t; egress_intrinsic_metadata_for_deparser_t]) *)
  in
  egr_parser
;;

let add_parser core_prog : prog =   
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
  let egr_parser = {td=egr_parser; tdspan = Span.default; tdpragma = None} in 
  let core_prog = List.map (fun comp -> if (fst comp.comp_id = "egress") then 
    {comp with comp_decls = egr_parser::comp.comp_decls} else comp) core_prog in
  (* print_endline ("----- done with egress parser -----"); *)
  core_prog
;;
(* this pass generates a parser for the egress component of a TofinoCore program... *)

open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
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


let block actions step : parser_block = 
  List.map (fun a -> a, Span.default) actions, (step, Span.default)
;;

let read cid ty = PRead(cid, ty)
let read_id (id, ty) = read (Cid.id id) ty
let peek cid ty = PPeek(cid, ty)
let skip ty = PSkip(ty)
let assign cid exp = PAssign(cid, exp)

let pbranch ints block : parser_branch  = (List.map (fun i -> PNum (Z.of_int i)) ints), block
let pmatch exps branches = PMatch(exps, branches)
let pgen exp = PGen(exp)
let pdrop = PDrop
let pcall exp = PCall(exp)

let parser id params block = 
  TDParser(id, params, block)
;;

let empty_block () :parser_block = 
  block [] pdrop
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
  print_endline ("---- egress parser ----");
  print_endline (TofinoCorePrinting.td_to_string egr_parser);
  print_endline ("-----------------------");
  egr_parser
  (* 
      <<left off here>> egress parser looks right-ish.
      next: 
        - think about how we encode flags. is it right? 
          the flags of an eventset have to go into a header. 
          for output, we have to put them in their own header...
            (in which case, the "eventset" is really a struct 
            of events with a flags event and then a set event...)
            we also have to do something like this to serialize an 
            event union...
        - add "enable" commands when serializing output
          from ingress / egress. 
        - add generate elimination and speculative parser passes
        - translate to P4 ir  
        - generate / translate user-written ingress parser
  *)
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
  let _ = make_egr_parser ingress_output_event egress_input_event in 
  exit 0;  
  core_prog

;;
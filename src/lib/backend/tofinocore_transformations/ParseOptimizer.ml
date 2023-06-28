(* parser optimization and final preparation for p4 ir *)
open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
[@@@ocaml.warning "-21-27-26"]


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


(* optimization pass: if a variable is read but never used, replace the read with a skip *)

(* do we use cid in the parser block before it is read again? *)
let used_in cid out_params pactions pstep  =   
  if (List.exists
    (fun (pid, _) -> Cid.equal (Cid.id pid) cid)
    out_params)
    then true
  else 
    let used = ref false in
    let v = object
    inherit [_] s_iter as super
    method! visit_PRead () cid' _ = 
      if (Cid.equal cid cid')
        then error "[used_in] while checking to see if a parser variable is ever used, it was found to be read. That means its read twice, which should be impossible."
    method! visit_EVar () cid' = 
      if (Cid.equal cid cid') then (used := true)
    end
    in
    List.iter (v#visit_parser_action ()) pactions;
    v#visit_parser_step () pstep;
    !used  
;;

(* replace reads that are never used with skips. *)
let elim_dead_reads component = 
  let v = object
  inherit [_] s_map as super
  method! visit_parser_block (out_params: params) parser_block = 
    let rec update_actions (pactions:(parser_action * sp) list) = 
      match pactions with 
      | [] -> []
      | (paction, sp)::pactions -> (
        (* we can delete paction if it reads something that is not 
           used in the other pactions or the step, or an out param. *)
        match paction with 
        | PRead(cid, ty) -> (
          let used_later = used_in cid out_params (List.split pactions |> fst) (fst parser_block.pstep) in
          if (used_later)
            then (paction, sp)::(update_actions pactions)
            else (PSkip(ty) ,sp)::(update_actions pactions)
        )
        | _ -> (paction, sp)::(update_actions pactions)
      )
    in
    let new_actions = update_actions parser_block.pactions in
    let new_step = (super#visit_parser_step out_params (fst parser_block.pstep)), snd parser_block.pstep in 
    {pactions=new_actions; pstep=new_step;}
  end
  in
  let p = List.find_map
    (fun decl -> match decl.td with 
      | TDParser(p) -> Some(p) | _ -> None)
    component.comp_decls
  in
  match p with 
  | Some(p) -> 
    v#visit_component p.pret_params component
  | None -> error "no parser in component"
  
;;

(* optimized generate elimination pass *)
let generates_using_cid output_event cid pactions pstep : cid list = 
  (* find all the generate statements that generate an event using a 
     constructor defined by out_event. Return a list of 
     _event parameters_ that get set to cid -- one parameter per 
     event constructor / generate. *)
  let ev_params = ref [] in
  let v = object
    inherit [_] s_iter as super
    method! visit_PGen () exp = 
      match exp.e with
      | ECall(evconstr_cid, eargs) -> (
          (* find the base event being generated *)
          let event_prefix, base_event = cid_to_eventconstr [] [output_event] evconstr_cid in
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
          (* return the parameter whose argument matches cid, if any. 
             If there is more than 1, it is an error that should have been 
          caught by the slot analysis (and compiler shouldn't have generated 
          slot-unsafe code) *)
          let param_matches = List.filter_map
            (fun (param_cid, earg) -> match earg.e with
              | EVar(cid') -> (
                if (Cid.equal cid cid')
                then (Some(param_cid))
                else (None)
              )
              | _ -> None)
            (List.combine params eargs)
          in
          match param_matches with
          | [] -> ()
          | [matching_param] -> ev_params := (!ev_params)@[matching_param]
          | _ -> error "a variable read in the parser was written to multiple parameters of a single event constructor -- this is not legal"
      )
      | _ -> ()
    end
  in
  List.iter (v#visit_parser_action ()) pactions;
  v#visit_parser_step () pstep;
  !ev_params |> List.split |> fst
;;

let generate_to_tagset out_event pstep : (parser_action * parser_step) option =
  match pstep with
  | PGen(gen_exp) -> (
    match gen_exp.e with 
    | ECall(evconstr_cid, eargs) -> (
      (* find the base event being generated *)
      let event_prefix, base_event = cid_to_eventconstr [] [out_event] evconstr_cid in
      let tagouterfield_id, (taginnerfield_id, tag_ty) = etag out_event in
      let tag_cid = Cid.create_ids (event_prefix@[tagouterfield_id; taginnerfield_id]) in
      (* let tag_cid = tag_id in  *)
      let tag_width = size_of_tint tag_ty in
      let tag_val = vint_exp (num_of_event base_event) tag_width in
      let assign_tag_action = PAssign(tag_cid, tag_val) in
      (* okay now where do we put the action ugh *)
    (* replace the PGen with an exit call*)
    let exit_call = call (Cid.create ["exit"]) [] (ty TEvent) in
    let parser_step = PCall(exit_call) in
    Some(assign_tag_action, parser_step))
   | _ -> None
  )
  | _ -> None
;;

(* Eliminate generates in an optimized way: 
  instead of reading packet data to a parser variable x, then copying the 
  variable to an event parameter y (which is what a generate does), 
  just add code that peeks to y (reads without advancing read pointer) 
  immediately before reading to x. 
example:
  read int a;
  ...
  generate(foo(a, ...)); 
  ==> 
  peek foo.param0;
  read a;
  ...
  generate(foo(a, ...));
  *)
let eliminate_generates_with_direct_reads component = 
  let v = object
  inherit [_] s_map as super
  method! visit_parser_block output_event parser_block = 
    let rec update_actions (pactions:(parser_action * sp) list) = 
      match pactions with
      | [] -> []
      | (paction, sp)::pactions -> (
        match paction with
        | PRead(cid, ty) -> (
          let params_set_to_cid = generates_using_cid 
            output_event 
            cid 
            (List.split pactions |> fst) 
            (fst parser_block.pstep)
          in
          (* add a peek statement for each param before the read action. *)
          let peek_actions = List.map (fun param_cid -> (PPeek(param_cid, ty), sp)) params_set_to_cid in 
          peek_actions@[paction, sp]@(update_actions pactions)
        )
        | _ -> (paction, sp)::(update_actions pactions)
      )
    in
    let new_actions = update_actions parser_block.pactions in
    let new_step = (super#visit_parser_step output_event (fst parser_block.pstep)), snd parser_block.pstep in 
    (* {pactions=new_actions; pstep=new_step;}    now we do a second transformation on the block -- eliminate the generate, if there is one. *)
    match generate_to_tagset output_event (fst new_step) with
      | None -> {pactions=new_actions; pstep=new_step;}
      | Some(tagset_action, exit_step) ->
          {pactions = new_actions@[tagset_action, Span.default];
          pstep = exit_step, Span.default;}     
  end
  in
  let output_event = (main_handler_of_component component).hdl_input in
  v#visit_component output_event component
;;

let print_parsers core_prog = 
  List.iter
    (fun component -> 
      Printf.printf "-- component %s --\n" (fst component.comp_id);
      Printf.printf 
        "%s" 
        (TofinoCorePrinting.parser_to_string (main_parser_of_component component) );
    )
    core_prog
;;

(* public functions *)
let parser_passes core_prog = 
  print_endline ("starting final parser passes");
  print_endline "---- parsers before optimizations ----";
  print_parsers core_prog;
  (* eliminate generates, adding peek reads to set event parameter variables *)
  let core_prog = List.map eliminate_generates_with_direct_reads core_prog in
  print_endline "---- parsers after eliminate_generates ----";
  print_parsers core_prog;
  (* replace reads that are never used in the parser with skips -- 
     after this, the only reads that remain should be reads to 
     variables used in parser matches. And those variables should 
     not be used as event generation arguments, because of the previous 
     generate elimination pass. *)
  let core_prog = List.map elim_dead_reads core_prog in
  print_endline "---- parsers after elim_dead_reads ----";
  print_parsers core_prog;
  core_prog
;;
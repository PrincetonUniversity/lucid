(* 
    This transformation is hoisting for event parameters that 
    are set to parse variables that are read directly from the header. 
    For each parse statement read x, look at the rest of the code. 
    Find each generate statement that passes x to an event and, if 
    x is not modified between its read point and its use, replace 
    "read x" with "peek event_arg; read x;". That is, read directly to 
    the event param, eliminating the copy from x -> event_arg 
    
    This transformation also handles local and assignment operations in 
    the parser the same way, so ultimately an output parameter set to 
    x will be written to at the last statement that updated x in its 
    control flow.
*)

(* example:
  read int a;
  ...
  generate(foo(a, ...)); 
  ==> 
  peek foo.param0;
  read a;
  ...
  generate(foo(a, ...));
  *)


open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
(* [@@@ocaml.warning "-21-27-26"] *)


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

(* get the fully scoped parameters of evconstr in output event *)
let scoped_params_of_evconstr output_event evconstr_cid = 
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
  params
;;


let del xs x = 
  (* remove x from xs *)
  List.filter (fun y -> not (x = y)) xs
;;

(* generates ultimately get replaced by a call to "exit" that 
   represents the end of the parser. *)
let exit_call = call (Cid.create ["exit"]) [] (ty TEvent) ;;
let exit_step = PCall(exit_call) 
;;


type pstep_sp = parser_step * sp
type paction_sps = (parser_action * sp) list
type hoist_cmds = (cid * (cid * ty)) list
type cids = cid list
let rec elim_parser_block output_event vars_read parser_block = 
  let hoist_cmds, (pactions, pstep) = elim_parser_inner output_event vars_read parser_block.pactions parser_block.pstep in
  hoist_cmds, {pactions; pstep}
and elim_parser_inner output_event (vars_read : cids) (pactions : paction_sps) (pstep : pstep_sp) 
    : hoist_cmds * (paction_sps * pstep_sp) = (* returns commands to hoist and the updated parser block *)
  (* hoist all possible generate parameters up to here *)
  (* let hoisted_params, pactions' = hoist_reads output_event parser_block.pactions parser_block.pstep in *)
  match pactions with 
  | [] -> (
    (* done processing the actions, now move on to the step *)
    match pstep with
    | PGen(exp), step_sp -> (
      (* a generate step. We want to replace this generate with assign statements as necessary
         for all the variables in the arguments that will not be hoisted. *)
      match exp.e with 
      | ECall(base_event_cid, eargs) -> (
        (* build an assoc list from args to parameters *)
        let params = scoped_params_of_evconstr output_event base_event_cid in 
        let hoist_cmds, gen_assigns = List.fold_left2
          (fun (hoist_cmds, gen_assigns) arg param -> 
            (* if arg is in vars_read *)
            match arg.e with 
            | EVar(argcid) when (List.mem argcid vars_read) ->
              (* a variable that is read and never updated on this control flow 
                 is the arg. That means the corresponding parameter can be hoisted
                 and no new command is necessary. *)
                 (argcid, param)::hoist_cmds, gen_assigns
            | _ -> 
              (* the parameter can't be hoisted, we need to add an assign. *)
              let assign = PAssign(fst param, arg), step_sp  in
              hoist_cmds, (assign::gen_assigns)
          )
          ([], [])
          eargs 
          params
        in
        (* return hoist commands and updated (actions, step) *)
        (* we need one more command! to set the generate tag. *)
        let event_prefix, base_event = cid_to_eventconstr [] [output_event] base_event_cid in
        let tagouterfield_id, (taginnerfield_id, tag_ty) = etag output_event in
        let tag_cid = Cid.create_ids (event_prefix@[tagouterfield_id; taginnerfield_id]) in
        let tag_width = size_of_tint tag_ty in
        let tag_val = vint_exp (num_of_event base_event) tag_width in
        let assign_tag_action = PAssign(tag_cid, tag_val) in
        (* print_endline ("generate step: "^(CorePrinting.parser_step_to_string (fst pstep))); *)
        (* if (List.length gen_assigns > 0) then 
        print_endline ("adding generate-specific assignment statements");
        List.iter
          (fun (pa, _) -> CorePrinting.parser_action_to_string pa |> print_endline)
          gen_assigns
        ; *)
        hoist_cmds, ((assign_tag_action, step_sp)::gen_assigns, (exit_step, step_sp))
      )
      | _ -> error "generate step does not call an event constructor"
    )
    | (PCall(_), _) -> error "calls should have been eliminated in parser steps"
    | PDrop, _ -> [], ([], pstep) (* no new hoist commands or actions *)
    | PMatch(exps, branches), match_sp -> 
      (* match: recurse and merge the hoist commands lists *)
      let hoist_cmds, branches' = List.fold_left
        (fun (hoist_cmds, branches') (pats, parser_block) -> 
          let hoist_cmds', parser_block' = elim_parser_block output_event vars_read parser_block in          
          hoist_cmds@hoist_cmds', (branches'@[(pats, parser_block')])
        )
        ([], [])
        branches
      in
      (* hoist commands from successors, no new assignments 
         (those are all in the successors) and updated successor branches *)
      hoist_cmds, ([], (PMatch(exps, branches'), match_sp))
  )
  (* if a variable is set by a read or peek, add it to 
     the list and recurse. When we return, we will add 
     hoisting statements for params set to the variable. *)
  | (paction, sp)::pactions -> (
    match paction with 
    | PRead(cid, _) 
    | PPeek(cid, _) -> (
      let vars_read = cid::vars_read in
      (* the result does not include this action. So we need to add it. 
         But also, we want to add all the hoist commands related to cid. *)
      let hoist_cmds, (subseq_actions, subseq_step) = elim_parser_inner output_event vars_read pactions pstep in
      (* print_endline ("on a read of variable "^(CorePrinting.cid_to_string cid)); *)
      let remaining_hoist_cmds, pactions = List.fold_left
        (fun (remaining_hoist_cmds, pactions) hoist_cmd -> 
          let (argcid, (paramcid, paramty)) = hoist_cmd in
          (* downstream processing has commanded us to set 
             paramcid with a peek at the point where this argcid is read. *)
          if (Cid.equal argcid cid) then 
            let peek_action = PPeek(paramcid, paramty) in
            remaining_hoist_cmds, pactions@[(peek_action, sp)]          
          else 
            (* this command has nothing to do with us, propagate it *)
            remaining_hoist_cmds@[hoist_cmd], pactions)
        ([], [])
        hoist_cmds
      in
      let pactions = MiscUtils.unique_list_of pactions in 
      remaining_hoist_cmds, ((pactions@[(paction, sp)]@subseq_actions), subseq_step)
    )
    (* locals and assigns are treated the same as reads, but it may be better to inline them? *)
    | PAssign(cid, exp)
    | PLocal(cid, _, exp) -> (
      let vars_read = cid::(del vars_read cid) in
      let hoist_cmds, (subseq_actions, subseq_step) = elim_parser_inner output_event vars_read pactions pstep in
      let remaining_hoist_cmds, hoisted_actions = List.fold_left
        (fun (remaining_hoist_cmds, pactions) hoist_cmd -> 
          let (argcid, (paramcid, _)) = hoist_cmd in
          (* downstream processing has commanded us to set 
             paramcid with a peek at the point where this argcid is read. *)
          if (Cid.equal argcid cid) then 
            let assign_action = PAssign(paramcid, exp) in
            remaining_hoist_cmds, pactions@[(assign_action, sp)]          
          else 
            (* this command has nothing to do with us, propagate it *)
            remaining_hoist_cmds@[hoist_cmd], pactions)
        ([], [])
        hoist_cmds
      in
      let hoisted_actions = MiscUtils.unique_list_of hoisted_actions in 
      remaining_hoist_cmds, ((hoisted_actions@[(paction, sp)]@subseq_actions), subseq_step)
    )
    | PSkip _ -> 
      let hoist_cmds, (subseq_actions, subseq_step) = 
        elim_parser_inner output_event vars_read (pactions) pstep
      in
      hoist_cmds, (((paction, sp)::subseq_actions), subseq_step)
  )
;;

let eliminate_generates_with_direct_reads component = 
  let v = object
    inherit [_] s_map as super
      (* just visit the main parser block *)
      method! visit_parser_block output_event parser_block = 
        let _, res = elim_parser_block output_event [] parser_block in
        res
    end
  in
  let output_event = (main_handler_of_component component).hdl_input in
  let output_event = get_event component output_event in 
  v#visit_component output_event component
;;

(* do we use cid in the parser block before it is read again? *)
let used_in cid out_params pactions pstep  =   
  if (List.exists
    (fun (pid, _) -> Cid.equal pid cid)
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

(* eliminate variables that are dead after hoisting. (never used in an expression)
   replace reads to those variables with skips, delete declarations and assignments. *)   
let elim_dead_reads component = 
  let v = object
  inherit [_] s_map as super
  method! visit_parser_block (out_params: cid_params) parser_block = 
    let rec update_actions (pactions:(parser_action * sp) list) = 
      match pactions with 
      | [] -> []
      | (paction, sp)::pactions -> (
        (* we can delete paction if it reads something that is not 
           used in the other pactions or the step, or an out param. *)
        match paction with 
        | PLocal(cid, _, _)
        | PAssign(cid, _) -> (
          (* for locals and assigns, we don't add a skip, we just delete the statement. *)
          let used_later = used_in cid out_params (List.split pactions |> fst) (fst parser_block.pstep) in
          if (used_later)
            then (paction, sp)::(update_actions pactions)
            else (update_actions pactions)
        )
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
    (* the builtin params are read in their entirety, not broken 
      into components, so we need the names of the structs not their fields. *)
    let builtin_params = (List.map (fun (i, t) -> Cid.id i, t) p.pret_params) in
    let out_params = match p.pret_event with 
    | None -> builtin_params
    | Some pret_event -> 
        let event_field_params = tyfields_of_event (get_event component pret_event) in
        builtin_params
        @event_field_params
    in
    v#visit_component out_params component
  | None -> component
  
;;

(* Okay. So. Apparently p4 or p4-tofino doesn't like 
   checksum operations over local parser variables. 
   So, we have to find all the local parser variables and 
   move them into the header. 
   This is super annoying for IR semantics because 
   the header represents the output of the handler function 
   that follows the parser. So really, the parser has no reason 
   to even be _aware_ of the header, besides this annoyance.
   The basic algorithm is: 
   1) find all the variables passed as arguments to checksums
   2) create a header for each variable, the header is a struct
      with a single field that holds the variable's name.
   3) replace all instances of the variable's cid with the 
      header field's cid.
   4) add the header declarations to the output event of the component's handler.


  The part that requires the most design is naming the header struct and 
  deciding how to represent it in the IR. 


   *)


(* convert a parameter (id, ty) to a single-field record. 
   return the name of the record and the type *)
let recwrap (id, ty) = 
  Id.fresh_name ((fst id)^"_rec"), CoreSyntax.ty (TRecord([id, ty.raw_ty]))
;;

(* get the (full cid, ty) of a recwrap'd parameter *)
let recwrap_extract (id, ty) = 
  match ty.raw_ty with 
  | TRecord([inner_id, inner_rawty]) -> 
    Cid.create_ids [id; inner_id],  (CoreSyntax.ty inner_rawty)
  | _ -> error "[recwrap_extract] invalid input. Expected rec with 1 field."

let move_checksum_args_into_header component = 
  let parser =  main_parser_of_component component in
  let outp_event = get_event component (parser.phdlret_event|> Option.get) in
  (* 1. get the checksum arguments *)
  let csum_args = ref [] in
  let v = object
    inherit [_] s_iter as super

    method! visit_EHash () _ args = 
      match args with 
      (* checksum call *)
      | {e=EVar(cid)}::args when (Cid.equal cid (Cid.id Builtins.checksum_id)) -> (
        csum_args := 
          (!csum_args)
          @(List.filter_map 
            (fun earg -> match earg.e with EVar(arg_cid) -> Some(Cid.to_id arg_cid, earg.ety) | _ -> None)
            args)
      )
      | _ -> ()
    end
  in  
  v#visit_parser () parser;
  let csum_args = !csum_args |> MiscUtils.unique_list_of in
  (* 2. construct a wrapping header record for each argument *)
  let wrapped_csum_args = List.map recwrap csum_args in
  (* 3. replace each use of the argument in the parser with the header record field. *)
  let replace_map = List.map2
    (fun (base_id, _) hdr_rec -> 
      let event_field_cid = recwrap_extract hdr_rec |> fst in
      (* have to prepend the name of the outer event *)
      let full_cid = Cid.compound (id_of_event outp_event) event_field_cid in
      Cid.id base_id, full_cid)  
    csum_args
    wrapped_csum_args
  in
  let replacer = object
    inherit [_] s_map as super
    method! visit_parser_action () pa = 
      match pa with
        | PPeek(cid, ty) when (List.mem_assoc cid replace_map) ->
          PPeek(List.assoc cid replace_map, ty)
        (* TRICKY: read event_param.hdr_var_param.hdr_var translates into:
           read event_param.hdr_var_param; invalidate(event_param.hdr_var_param);
            (we read the header, not the field within it, and we also need to 
              invalidate the field as soon as its read)
            However, since the parser syntax can't support a unit call, 
            we add the invalidate in the final translation... *)
        | PRead(cid, ty) when (List.mem_assoc cid replace_map) ->
          let full_cid = List.assoc cid replace_map in 
          let hdr_field_only_cid = 
            Cid.to_ids full_cid |> List.rev |> List.tl |> List.rev |> Cid.create_ids 
          in 
          PRead(hdr_field_only_cid, ty)
        | PAssign(cid, exp) when (List.mem_assoc cid replace_map) -> 
          PAssign(List.assoc cid replace_map, super#visit_exp () exp)
        | PLocal(cid, ty, exp) when (List.mem_assoc cid replace_map) -> 
          PLocal(List.assoc cid replace_map, ty, super#visit_exp () exp)
        | _ -> super#visit_parser_action () pa

    method! visit_EVar () cid = 
      if (List.mem_assoc cid replace_map) 
        then EVar(List.assoc cid replace_map)
        else EVar(cid)
    end
  in
  let parser' = replacer#visit_parser () parser in
  (* 4. add the header records to the output event's parameters *)
  

  let outp_event' = match outp_event with 
    | EventWithMetaParams{event; params} -> 
      EventWithMetaParams{event; params=params@wrapped_csum_args}
    | event -> EventWithMetaParams{event; params=wrapped_csum_args}
  in
  (* 5. update the parser and event declarations in the component. *)
  let component = set_event component outp_event' in
  let component = replace_main_parser_of_component parser' component in
  (* 6. that might just be it? *)
  component
;;


let print_parsers core_prog = 
  List.iter
    (fun component -> 
      Printf.printf "-- component %s --\n" (fst component.comp_id);      
      try
        Printf.printf 
          "%s" 
          (TofinoCorePrinting.parser_to_string (main_parser_of_component component) );
      with Failure f when f = "hd"->
        Printf.printf "no parser\n")
    core_prog
;;


let parser_passes core_prog = 
  (* print_endline ("starting final parser passes"); *)
  (* replace generates with peek reads and assign statements. For event arguments that are variables 
     which are read and not updated before the generate, hoist the setting of the event parameter by 
     adding a peek immediately after the initial read.*)
  let core_prog = List.map (skip_control eliminate_generates_with_direct_reads) core_prog in
  (* replace reads that are never used in the parser with skips -- 
     after this, the only reads that remain should be reads to 
     variables used in parser matches. And those variables should 
     not be used as event generation arguments, because of the previous 
     generate elimination pass. *)
  let core_prog = List.map (skip_control elim_dead_reads) core_prog in
  let core_prog = List.map (skip_control move_checksum_args_into_header) core_prog in 
  core_prog
;;
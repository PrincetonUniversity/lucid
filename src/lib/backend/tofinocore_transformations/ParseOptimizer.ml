(* parser optimization and final preparation for p4 ir *)
open CoreSyntax
open TofinoCoreNew
open AddIntrinsics
[@@@ocaml.warning "-21-27-26"]

(*** 1/3: generate elimination ***)

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

open Collections

let rec update_last fn xs =
  match xs with 
  | [] -> []
  | [x] -> [fn x]
  | x::xs -> x::(update_last fn xs)
;;

let freshnum = ref 1 ;;
let fresh_id id = 
  freshnum := !freshnum + 1;
  (fst id^"_"^string_of_int (!freshnum), !freshnum)
;;

let fresh_cid cid = 
  Cid.to_ids cid |> (update_last fresh_id) |> Cid.create_ids
;;  



type eliminate_ctx = (Cid.t CidMap.t) (* (new_name -> old_name) *)

let rec combine_ctxs (cidmaps: eliminate_ctx list) : eliminate_ctx = 
  match cidmaps with
  | [] -> error "[combine_ctxs] no maps"
  | [m] -> m
  | m::cidmaps -> (
    CidMap.merge
      (fun k a_opt b_opt -> 
        match a_opt, b_opt with 
          | None, None -> None
          | Some(a), Some(b) -> 
            error "[combine_ctxs] defined in both?"
          | Some(a), None -> Some(a)
          | None, Some(b) -> Some(b))
    m
    (combine_ctxs cidmaps)
  )
;;

let rec unique_params_parser_block (ctx : eliminate_ctx) parser_block : (eliminate_ctx * parser_block) = 
  (* first, traverse the step, collecting new undeclared vars in the context. *)
  let ctx', pstep' = unique_params_parser_step ctx (fst parser_block.pstep) in

  (* next, traverse the actions, declaring undeclared variables that are 
  renames of variables declared (read) in these actions. *)
  let ctx', pactions' = List.fold_left
    (fun (ctx, pactions) (paction, sp) -> 
      let ctx', new_pactions = unique_params_parser_action ctx paction in
      let new_pactions = List.map (fun p -> p, sp) new_pactions in 
      ctx', (pactions@new_pactions))
    (ctx', [])
    parser_block.pactions
  in
  ctx', {pactions = pactions'; pstep = (pstep', snd parser_block.pstep)}


and unique_params_parser_action ctx paction : eliminate_ctx * (parser_action list) =
  match paction with 
  | PRead(cid, ty) -> (
    let ctx', peek_acns = CidMap.fold 
      (fun new_cid old_cid (ctx, acns) -> 
        if (Cid.equal old_cid cid)
          then (
            let ctx' = CidMap.remove new_cid ctx in
            let acns' = acns@[peek new_cid ty] in
            ctx', acns'
          )
          else (ctx, acns))
      ctx
      (ctx, [])
    in
    ctx', peek_acns@[paction]
  )
  | _ -> ctx, [paction]

and unique_params_parser_step (ctx: eliminate_ctx) parser_step : (eliminate_ctx * parser_step) = 
  match parser_step with
  | PGen(exp) -> (
    match exp.e with 
    | ECall(ev_cid, ev_args) -> (     
      (* rename the args and update context *)
      let (ctx', ev_args') = List.fold_left
        (fun (ctx, ev_args) ev_arg -> 
          match ev_arg.e with 
          | EVar(old_cid) -> 
            let new_cid = fresh_cid old_cid in
            let ctx' = CidMap.add new_cid old_cid ctx in
            ctx', ev_args@[{ev_arg with e=EVar(new_cid)}]
          | _ -> ctx, ev_args@[ev_arg])
        (ctx, [])
        ev_args
      in
      ctx', PGen({exp with e = ECall(ev_cid, ev_args')})
    )
    (* generate is not an ecall? probably error *)
    | _ -> ctx, parser_step
  )
  (* pmatch - recurse on branches, combine the contexts after *)
  | PMatch(exps, branches) -> (
    let (ctxs, branches') = List.fold_left
      (fun (ctxs, branches) (pats, block:parser_branch) -> 
        let ctx', block' = unique_params_parser_block ctx block in
        ctxs@[ctx'], branches@[pats, block'])
      ([], [])
      branches
    in
    let ctx' = combine_ctxs ctxs in
    ctx', PMatch(exps, branches')
  )
  | PDrop -> ctx, parser_step
  | PCall(_) -> ctx, parser_step
;;


let unique_gen_args = object
  inherit [_] s_map as super
  method! visit_parser_block () parser_block = 
    snd (unique_params_parser_block (CidMap.empty) parser_block) 
  end
;;


(* optimization pass: if a variable is read but never used, replace the read with a skip *)

(* do we use cid in the parser block before it is read again? *)
let used_in cid pactions pstep  =   
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
let elim_dead_reads = 
  object
  inherit [_] s_map as super
  method! visit_parser_block () parser_block = 
    let rec update_actions (pactions:(parser_action * sp) list) = 
      match pactions with 
      | [] -> []
      | (paction, sp)::pactions -> (
        (* we can delete paction if it reads something that is not 
           used in the other pactions or the step *)
        match paction with 
        | PRead(cid, ty) -> (
          let used_later = used_in cid (List.split pactions |> fst) (fst parser_block.pstep) in
          if (used_later)
            then (paction, sp)::(update_actions pactions)
            else (PSkip(ty) ,sp)::(update_actions pactions)
        )
        | _ -> (paction, sp)::(update_actions pactions)
      )
    in
    let new_actions = update_actions parser_block.pactions in
    let new_step = (super#visit_parser_step () (fst parser_block.pstep)), snd parser_block.pstep in 
    {pactions=new_actions; pstep=new_step;}

  end
;;

let eliminate_generates component = 
  (* eliminate generate statements in the parser by replacing them with 
     assignments to the parameters of the event. *)
  (* the output event of the parser is the input event of the main handler *)
  let output_event = (main_handler_of_component component).hdl_input in
  let v = object
    inherit [_] s_map as super
    method! visit_parser_block () parser_block =
    let parser_actions_spans_list = parser_block.pactions in
    let (parser_step, sp) = parser_block.pstep in
      match parser_step with 
      | PGen(gen_exp) -> (
        (* this is a generate. So now we need to find the constructor for 
           the event, get its parameters, append assign statements to 
           set the parameters, and finally replace the PGen with an exit, 
           (perhaps something like PCall continue()) *)
        match gen_exp.e with 
        | ECall(evconstr_cid, eargs) -> 
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
          let tag_id, tag_ty = etag output_event in
          let tag_cid = Cid.create_ids (event_prefix@[tag_id]) in
          (* let tag_cid = tag_id in  *)
          let tag_width = size_of_tint tag_ty in
          let tag_val = vint_exp (num_of_event base_event) tag_width in



          (* finally, we need to set the tag of the event holding the base event. 
              for example, generate(ingress_input.bar) -> ingress_input.tag = bar.num;   
          *)
          (* optimized: make peek / skip actions to set all the params, 
             then replace the actions in the generate block with the new ones. *)
          (* let peek_skip_actions = List.fold_left
            (fun actions (cid,ty) -> 
              actions@[
                PPeek(cid, ty), Span.default;
                PSkip(ty), Span.default                
                ]
              )
            []
            params
          in
          let parser_actions_spans_list = peek_skip_actions in  *)
          (* make assign actions to set all the params and the tag *)
          let assign_actions = List.map2
            (fun (cid, _) earg -> PAssign(cid, earg), Span.default)
            ((tag_cid, tag_ty)::params)
            (tag_val::eargs)
          in
          (* update the actions list *)
          let parser_actions_spans_list = parser_actions_spans_list@assign_actions in
          (* replace the PGen with an exit call*)
          let exit_call = call (Cid.create ["exit"]) [] (ty TEvent) in
          let parser_step = PCall(exit_call) in
          let parser_block' = {pactions=parser_actions_spans_list; pstep=(parser_step, sp);} in 
          super#visit_parser_block () parser_block'
        | _ -> error "parser generate should take an expression of variant ECall"
      )
      | _ ->  
      super#visit_parser_block () parser_block
    end
  in
  v#visit_component () component
;;

(* 2/3: speculative parsing *)


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
(* this optimization will be important for the ingress parser, 
   but not for egress because we generate optimized code. *)

;;
(* optimize the ingress parser to read multiple copies of each 
   local variable that ends up being passed as an event parameter. 
   This decouples handler phvs. 
   
   Things to do in this pass:
   1. eliminate generates in the parser
   2. replace reads with peeks
   3. fill in other stuff in parser node
   *)

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
  (* 1. make arguments to generates unique *)
  let core_prog = unique_gen_args#visit_prog () core_prog in 
  print_endline "---- parsers with unique vars ----";
  (* 2. replace reads that are never used in the parser with skips *)
  let core_prog = elim_dead_reads#visit_prog () core_prog in 
  print_endline "---- parsers after elim_dead_reads ----";
  (* 3. finally, eliminate generates. *)
  let core_prog = List.map (fun comp -> eliminate_generates comp) core_prog in
  print_endline "---- parsers after eliminate_generates ----";
  print_parsers core_prog;
  exit 0;
  core_prog
;;
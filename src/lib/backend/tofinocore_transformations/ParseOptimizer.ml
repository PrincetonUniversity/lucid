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
          (* make assign actions to set all the params *)
          let assign_actions = List.map2
            (fun (cid, _) earg -> PAssign(cid, earg), Span.default)
            params
            eargs
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
let speculative_parsing parser = 
  error "todo"


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
  print_endline "before eliminate_generates";
  print_parsers core_prog;
  (* 1. eliminate generates *)
  let core_prog = List.map eliminate_generates core_prog in 
  print_endline "after eliminate_generates";
  print_parsers core_prog;
  core_prog
;;




(* depreciated *)
    let old_stuff core_prog = 
    let v = 
       object
          inherit [_] s_map as super
          method! visit_TDParser () parser =
            print_endline ("parser: ");
            print_endline (CorePrinting.parser_block_to_string parser.pblock);
            super#visit_TDParser () parser

          method! visit_parser_block () parser_block = 
             super#visit_parser_block () parser_block
 
       end
    in
    let ingress = List.find (fun cmp -> cmp.comp_id |> fst = "ingress") core_prog in
    let comp_decls = v#visit_tdecls () ingress.comp_decls in
    print_endline ("optimize_parser finished");
    exit 1;
    List.map (fun cmp -> 
      if (cmp.comp_id |> fst = "ingress")
        then ({cmp with comp_decls = comp_decls})
        else (cmp)
      )
      core_prog
 ;;
 
(* 

  The new generate elimination pass.

  The main task of this pass is to simultaneously construct the 
  output headers for generated event variables and vailidty bools. 
  To construct the egress, we will need to know:
    1) which generate paths can happen;
    2) which events are produced by each generate path
  That is derived from main_handler.event_output.generate_sequences....
  Should that be filled here? Probably...

  event foo(int a, int b);
  event bar(int c);

  handle foo(int a, int b) {
    generate(foo(a, b));
    if (...) {
      generate(bar(a));
    }
    else {
      generate(bar(b));
    } 
  }

  ==> 
  headers : {
    generate_0_active : bool;
    generate_1_active : bool;
    generate_2_active : bool;
    generate_0_arg0 : int;
    generate_0_arg1 : int;
    generate_1_arg0 : int;
    generate_2_arg0 : int;
  }
  handle foo(int a, int b) {
    // generate(foo(a, b));
    generate_0_active = true;
    generate_0_arg0 = a;
    generate_0_arg1 = b;
    if (...) {
      // generate(bar(a));
      generate_1_active = true;
      generate_1_arg0 = a;
    }
    else {
      // generate(bar(b));
      generate_2_active = true;
      generate_2_arg0 = b;
    } 
  }
  
  *)


(* what do we do when we see a generate statement? *)
(* 
   0) assign the generate statement a unique iid.
   1) create header parameters for the generate statement. (do we just need to know the event name?)
   2) create a new validity bool for that generate statement.
   3) somehow track the current sequence of generate statements?
*)


open CoreSyntax
open TofinoCore
open CoreCfg

exception Error of string
let error s = raise (Error s)


let eliminate is_ingress tds =
  let _ = is_ingress in tds 
  (* let m = (main tds) in 
  let rev_hdl_enum = List.map (fun (x, y) -> (y, x)) m.hdl_enum in
  let stmt = m.main_body |> List.hd in
  let new_stmt, new_vars = match (stmt.s) with 
    | SMatch(es, bs) -> (
      let new_bs, new_vars = 
        List.map (invalidate_input_event_header_when_not_reused tds rev_hdl_enum m.hdl_params) bs
        |> List.split
      in
      let new_vars = List.flatten new_vars in
      {stmt with s=SMatch(es, new_bs)}, new_vars
    )
    | _ -> error "[generate elimination] body of main handler must start with match on event id"
  in
  (* update the main body and the list of shared locals. *)
  let tds = update_main tds { m with 
    main_body = [new_stmt];
    shared_locals = m.shared_locals@new_vars;
    } 
  in
  (* finally, if this is an ingress program, increment the multicast group for recirculating generates *)
  if (is_ingress) then (count_recirc_generates tds) else (tds)
  tds *)
;;
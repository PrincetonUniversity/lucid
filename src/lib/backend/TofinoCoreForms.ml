(* TofinoCoreForms
    Most of the tofino backend operates on 
    programs in the TofinoCore IR. However, different 
    passes require the code to be in different forms. 
    For example, code that runs before layout expects 
    that the body of the main function has only 1 statement
    (which may be a sequence). 
    Code that runs after layout expects the body of the 
    main handler to have a list of multiple statements, 
    one statement for each stage.
    This module has checks that passes use to verify 
    that the form of their input is as expected. *)
open CoreSyntax
open TofinoCore

let main_with_event_match tds = 
  (* required input form:
    a main handler whose body is a single match statement 
    that branches on event id *)
  let estr = "[Forms.main_single_branch] the input program to this \
        pass is not in the correct form. Either their is a bug in a \
        previous pass, or this pass has been moved to a phase of the \
        compiler where it was not intended to go."
  in
  let m = (main tds) in
  match m.main_body with
  | [stmt] -> (
    match stmt.s with 
    | SMatch([evid_var], _) -> (
      if (id_of_exp evid_var = (m.hdl_selector |> fst))
      then ()
      else (error estr))
    | _ -> (error estr))
  | _ -> (error estr)
;;

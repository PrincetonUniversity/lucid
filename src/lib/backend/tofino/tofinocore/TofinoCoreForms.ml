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

(* get the source globals declared at this point in the code *)
let source_globals tds = 
  let global_ids = ref [] in
  let v =
    object
      inherit [_] s_iter as super
      method! visit_tdecl _ tdecl =
        match tdecl.td with
        | TDGlobal(id, _, _) -> 
          print_endline (TofinoCorePrinting.tdecl_to_string tdecl);
          global_ids := id::(!global_ids);
        | TDActionConstr({aid=aid; _}) -> 
          global_ids := aid::(!global_ids);
        | _ -> ()
    end
  in
  v#visit_tdecls () tds;
  !global_ids
;;

(* have the globals changed? *)
let same_globals orig_globals tds =
  let new_globals = source_globals tds in
  if (MiscUtils.test_set_eq orig_globals new_globals)
  then (print_endline ("globals unchanged."))
  else (
    print_endline ("error: globals changed. old and new:");
    CorePrinting.comma_sep CorePrinting.id_to_string orig_globals |> print_endline;
    CorePrinting.comma_sep CorePrinting.id_to_string new_globals|> print_endline
  )
;;


let main_with_event_match label tds = 
  let _, _ = label, tds in 
  error "form check not implemented"
  (* required input form:
    a main handler whose body is a single match statement 
    that branches on event id *)
  (* let estr = "["^(label)^"]"^" the program IR is not in the \
expected form. The expected form at this point in the compiler is a \
TofinoCore program with a main handler that consists of a single statement, \
a match statement that branches on the handler ID variable. "
  in
  let m = (main_handler_of_decls tds) in
  match m.main_body with
  | [stmt] -> (
    match stmt.s with 
    | SMatch([evid_var], _) -> (
      if (id_of_exp evid_var = (m.hdl_selector |> fst))
      then ()
      else (error estr))
    | _ -> (error estr))
  | _ -> (error estr) *)
;;

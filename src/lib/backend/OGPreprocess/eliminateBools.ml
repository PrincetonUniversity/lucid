(* convert all booleans into 1-bit ints *)
open Syntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf
let bool_sz = 1

let eliminate_complex_bool_assign stmt =
  (*   
    bool x = expr;
    -->
    bool x = false; 
    if (expr) {
      x = true;
    }
  *)
  let rhs_false = Syntax.exp (EVal (vbool false)) in
  let rhs_true = Syntax.exp (EVal (vbool true)) in
  match stmt.s with
  | SAssign (id, rhs) ->
    let sinit = { stmt with s = SAssign (id, rhs_false) } in
    let set_true = sassign id rhs_true in
    sseq sinit (sifte rhs set_true snoop)
  | SLocal (id, ty, rhs) ->
    let sinit = { stmt with s = SLocal (id, ty, rhs_false) } in
    let set_true = sassign id rhs_true in
    sseq sinit (sifte rhs set_true snoop)
  | _ -> stmt
;;

let eliminate_complex_bool_assigns ds =
  let v =
    object
      inherit [_] s_map as super

      (* skip memops! *)
      method! visit_DMemop _ id body = DMemop (id, body)

      method! visit_statement ctx statement =
        match rhs_of_stmt statement.s with
        | Some exp ->
          (match is_bool_non_immediate exp with
          | true ->
            trans_info
              ("[to_bool_immediate_rhs] eliminating non-immediate boolean \
                assignment in: "
              ^ Printing.stmt_to_string statement);
            eliminate_complex_bool_assign statement
          | false -> super#visit_statement ctx statement)
        | None -> super#visit_statement ctx statement
    end
  in
  v#visit_decls (statement SNoop) ds
;;

let zint_of_bool b =
  match b with
  | true -> Integer.create 1 bool_sz
  | false -> Integer.create 0 bool_sz
;;

(* eliminate boolean values and types *)
let eliminate_bool_values_and_types ds =
  let v =
    object
      inherit [_] s_map as super

      (* skip memops! *)
      method! visit_DMemop _ id body = DMemop (id, body)
      method! visit_VBool _ b = VInt (zint_of_bool b)
      method! visit_TBool _ = TInt (IConst bool_sz)
    end
  in
  v#visit_decls (statement SNoop) ds
;;

let log_prog comment ds =
  !dprint_endline ("-----" ^ comment ^ "-------");
  !dprint_endline (Printing.decls_to_string ds);
  !dprint_endline ("-----end " ^ comment ^ "-------")
;;

let elimination_only ds =
  let ds = eliminate_bool_values_and_types ds in
  log_prog "after phase 3: immediate elimination" ds;
  ds
;;

let do_passes (ds : decls) : Syntax.decls =
  trans_info "Starting cannonization before IR...";
  DBG.start_mlog __FILE__ outc dprint_endline;
  (* 1: transform boolean assignment rhs to always be immediates. *)
  log_prog "before boolean elimination" ds;
  let ds = eliminate_complex_bool_assigns ds in
  log_prog "after phase 1: non-immediate elimination" ds;
  trans_info "boolean elimination complete. ";
  let ds = Typer.infer_prog ds in
  (* 2: put if expressions into a normal form 
    Note -- this must never introduce any new boolean variables. *)
  let ds = NormalizeBools.do_passes ds in
  log_prog "after phase 2: if expression cannonization" ds;
  trans_info "if-else cannonization complete.";
  let ds = Typer.infer_prog ds in
  (* 3: eliminate boolean values, variables, and types *)
  let ds = eliminate_bool_values_and_types ds in
  log_prog "after phase 3: immediate elimination" ds;
  trans_info "immediate elimination complete.";
  let ds = Typer.infer_prog ds in
  (* 4: future improvement: convert directly into a match statement *)

  (* type check. *)
  print_endline "TYPE CHECKING AFTER BOOL ELIM";
  print_endline "program: ";
  (* print_endline (Printing.decls_to_string ds); *)
  let ds = Typer.infer_prog ds in
  print_endline "DONE TYPE CHECKING AFTER BOOL ELIM";
  ds
;;

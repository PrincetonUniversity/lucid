(* 
    interpreter for operating 
    on live traffic from interfaces 
*)
open Batteries
open Dpt

let main () =
  Config.base_cfg.verbose <- false;
  let _ = SoftSwitchConfig.parse_args () in
  let ds = Input.parse Config.base_cfg.dpt_file in
  let renaming, ds =
    FrontendPipeline.process_prog Builtins.interp_builtin_tys ds
  in
  let ds =  MidendPipeline.process_prog ds in
  let nst, pp, spec = Interp.initialize_softswitch renaming ds in
  ignore (Interp.run_softswitch pp renaming spec nst)


  (* Simple rawlink test *)
  (* InterpSocket.test_rawlink "feth0";   *)

  (* () *)
  (* ignore (Interp.run_softswitch pp renaming spec nst) *)
;;

let _ = main ()

(* A lucid software switch that operates on raw linux interfaces
    and uses the interpreter to process packets. *)
open Batteries
open Dpt

let main () =
  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 32 * 1024 * 1024 (* words *) };
  Config.base_cfg.verbose <- false;
  let _ = SwitchConfig.parse_args () in
  let ds = Input.parse Config.base_cfg.dpt_file in
  let renaming, ds =
    FrontendPipeline.process_prog Builtins.interp_builtin_tys ds
  in
  let ds =  MidendPipeline.process_prog ds in
  let nst, pp, spec = Interp.initialize_softswitch renaming ds in
  Gc.compact ();
  print_endline @@ "{\"Init complete.\":[]}";
  flush stdout;
  ignore (Interp.run_softswitch pp renaming spec nst)
;;

let _ = main ()

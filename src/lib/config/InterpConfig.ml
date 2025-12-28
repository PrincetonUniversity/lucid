(* Interpreter config *)
open Config

type config = 
  { mutable spec_file : string (** Path to an interpreter specification file *)
  ; mutable symb_file : string (** Path to a symbolic specification file *)
  ; mutable show_interp_state : bool (* report final interpreter state *)
  ; mutable show_interp_events : bool (* report events processed by interpreter *)
  ; mutable show_printf        : bool (* report printf statements *)
      (** Run interactively (stdin / stdout) **)
  ; mutable interactive : bool
  ; mutable json : bool  (** Print json outputs **)
  }
;;

(* interp config from spec / test file *)
type interp_config_from_test =
  { num_switches : int
  ; max_time : int
  ; default_input_gap : int
  ; generate_delay : int
  ; propagate_delay : int
  ; random_seed : int
  ; random_delay_range : int
  ; random_propagate_range : int
  ; drop_chance : int
  }
;;

let cfg : config = 
  { spec_file = ""
  ; symb_file = ""
  ; show_interp_state = true
  ; show_interp_events = true
  ; show_printf = true
  ; interactive = false
  ; json = false
  }
;;

let interp_speclist = 
  let hide_final_state () = cfg.show_interp_state <- false in
  let hide_event_reports () = cfg.show_interp_events <- false in
  let hide_printf () = cfg.show_printf <- false in
  let set_interactive () =
    cfg.interactive <- true;
    base_cfg.verbose <- false;
    cfg.show_interp_state <- false;
    cfg.show_interp_events <- false
  in
  let set_json () =
    cfg.json <- true;
    base_cfg.verbose <- false
  in
  let set_spec s = cfg.spec_file <- s in
  [ 
      ( "--json"
        , Arg.Unit set_json
        , "If true, print all interpreter output as json records" )
    
      ; ( "--spec"
        , Arg.String set_spec
        , "Path to the interpreter specification file" )
      ; ( "--suppress-final-state"
        , Arg.Unit hide_final_state
        , "If set, don't print the final state of the interpreter when it \
          finishes." )
      ; ( "--suppress-event-reports"
        , Arg.Unit hide_event_reports
        , "If set, don't print event arrival / generation reports while \
            interpreting. ")
      ; ( "--no-printf"
        , Arg.Unit hide_printf
        , "If set, don't print printf statements while interpreting. ")
      ; ( "--interactive"
        , Arg.Unit set_interactive
        , "Run interpreter interactively, piping events to/from stdin/stdout \
            disables verbose and suppresses final state and event reports" )
      ; ( "-i"
        , Arg.Unit set_interactive
        , "Run interpreter interactively, piping events to/from stdin/stdout \
            disables verbose and suppresses final state and event reports" )
   ]  
;;

let parse_interp () = parse 
  interp_speclist
  "Lucid interpreter. Options:"
;;

type config =
  { mutable verbose : bool
        (* Print out each transformation name before we do it *)
  ; mutable debug : bool
        (** Print out the whole program after each transformation *)
  ; mutable show_effects : bool (** Print out effect annotations *)
  ; mutable verbose_types : bool (** Print out extra typing information *)
  ; mutable show_tvar_links : bool
        (** Print out a little more typing information *)
  ; mutable show_constraints : bool
  ; mutable show_queries : bool
        (* Try to print out record types using the user-defined name *)
  ; mutable use_type_names : bool
  ; mutable show_all_effects : bool (* Show effects even for non-global types *)
  ; mutable spec_file : string (** Path to an interpreter specification file *)
  ; mutable symb_file : string (** Path to a symbolic specification file *)
  ; mutable dpt_file : string (** Path to the input dpt file *)
  ; mutable show_interp_state : bool
  }

(* TODO: We might want to add more parameters controlling which transformations
   are applied (if we're using the interpreter) *)

let default () =
  { verbose = true
  ; debug = false
  ; show_effects = false
  ; verbose_types = false
  ; show_tvar_links = false
  ; show_queries = false
  ; show_constraints = false
  ; use_type_names = true
  ; show_all_effects = false
  ; spec_file = ""
  ; symb_file = ""
  ; dpt_file = ""
  ; show_interp_state = true
  }
;;

let cfg = default ()
let set_dpt_file fname = cfg.dpt_file <- fname

let parse () =
  let unset_verbose () = cfg.verbose <- false in
  let set_debug () = cfg.debug <- true in
  let set_effects () = cfg.show_effects <- true in
  let set_types () = cfg.verbose_types <- true in
  let set_tvars () = cfg.show_tvar_links <- true in
  let set_spec s = cfg.spec_file <- s in
  let set_symb s = cfg.symb_file <- s in
  let print_all () =
    set_debug ();
    set_effects ();
    set_types ();
    set_tvars ()
  in
  let set_constraints () = cfg.show_constraints <- true in
  let set_queries () = cfg.show_queries <- true in
  let set_type_names () = cfg.use_type_names <- false in
  let set_all_effects () = cfg.show_all_effects <- true in
  let really_print_all () =
    print_all ();
    set_type_names ();
    set_all_effects ()
  in
  let set_final_state () = cfg.show_interp_state <- false in
  let speclist =
    [ ( "--silent"
      , Arg.Unit unset_verbose
      , "Don't print transformation names before we do them" )
    ; ( "--debug"
      , Arg.Unit set_debug
      , "Print out the whole program after each transformation" )
    ; ( "-d"
      , Arg.Unit set_debug
      , "Print out the whole program after each transformation" )
    ; "--effects", Arg.Unit set_effects, "Print out effect annotations"
    ; "--types", Arg.Unit set_types, "Print out extra typing information"
    ; ( "--tvars"
      , Arg.Unit set_tvars
      , "Print out a little more typing information" )
    ; "-m", Arg.Unit print_all, "Enable all printing options"
    ; ( "--spec"
      , Arg.String set_spec
      , "Path to the interpreter specification file" )
    ; "--symb", Arg.String set_symb, "Path to the symbolic specification file"
    ; ( "--queries"
      , Arg.Unit set_queries
      , "If true, print out SMT queries made during typechecking. Not enabled \
         by -m." )
    ; ( "--no-tynames"
      , Arg.Unit set_type_names
      , "Normally the printer tries to print user types using the user-defined \
         name for the type. This disables that." )
    ; ( "--all-effects"
      , Arg.Unit set_all_effects
      , "Show effects for all types, even non-global ones." )
    ; ( "-mm"
      , Arg.Unit really_print_all
      , "Equivalent to -m --no-tynames --all-effects" )
    ; ( "-q"
      , Arg.Unit set_queries
      , "If true, print out SMT queries made during typechecking. Not enabled \
         by -m." )
    ; ( "-c"
      , Arg.Unit set_constraints
      , "If true, print out each set of constraints we try to solve, but not \
         the SMT query itself. Not enabled by -m." )
    ; ( "--suppress-final-state"
      , Arg.Unit set_final_state
      , "If set, don't print the final state of the interpreter when it \
         finishes." ) ]
  in
  let target_filename = ref "" in
  let usage_msg = "Lucid command line. Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

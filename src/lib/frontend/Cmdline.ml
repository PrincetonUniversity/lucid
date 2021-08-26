type config =
  { mutable verbose : bool
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
  }

(* TODO: We might want to add more parameters controlling which transformations
   are applied (if we're using the interpreter) *)

let default () =
  { verbose = false
  ; show_effects = false
  ; verbose_types = false
  ; show_tvar_links = false
  ; show_queries = false
  ; show_constraints = false
  ; use_type_names = true
  ; show_all_effects = false
  ; spec_file = ""
  }
;;

let cfg = default ()

let parse () =
  let set_verbose () = cfg.verbose <- true in
  let set_effects () = cfg.show_effects <- true in
  let set_types () = cfg.verbose_types <- true in
  let set_tvars () = cfg.show_tvar_links <- true in
  let set_spec s = cfg.spec_file <- s in
  let print_all () =
    set_verbose ();
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
  let speclist =
    [ ( "--verbose"
      , Arg.Unit set_verbose
      , "Print out the whole program after each transformation" )
    ; ( "-v"
      , Arg.Unit set_verbose
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
         the SMT query itself. Not enabled by -m." ) ]
  in
  let target_filename = ref "" in
  let usage_msg = "Lucid command line. Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

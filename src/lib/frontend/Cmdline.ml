
type config =
  { mutable verbose : bool
      (* Print out each transformation name before we do it *)
  ; mutable debug : bool
      (** Print out the whole program after each transformation *)
  ; mutable logging : bool 
      (* enable logging to files (for tofino backend) *)
  ; mutable show_effects : bool (** Print out effect annotations *)
  ; mutable verbose_types : bool (** Print out extra typing information *)
  ; mutable show_tvar_links : bool
      (** Print out a little more typing information *)
  ; mutable show_constraints : bool
  ; mutable show_queries : bool
      (* Try to print out record types using the user-defined name *)
  ; mutable use_type_names : bool
  ; mutable show_all_effects : bool (* Show effects even for non-global types *)
  ; mutable partial_interp : bool (* Enable partial interpretation *)
  ; mutable spec_file : string (** Path to an interpreter specification file *)
  ; mutable symb_file : string (** Path to a symbolic specification file *)
  ; mutable dpt_file : string (** Path to the input dpt file *)
  ; mutable show_interp_state : bool
  ; mutable interactive : bool
      (** Run interpreter interactively (stdin / stdout) **)
  ; mutable output : string
  ; mutable json : bool (* tofino backend *) (** Print json outputs **)
  ; mutable builddir : string (* build directory where p4 + other code goes *)
  ; mutable portspec : string option (* path to port specification json *)
  ; mutable profile_cmd :
      string option (* something with profiling -- probably depreciated *)
  ; mutable ctl_fn : string option (* path to optional python control program *)
  ; mutable old_layout : bool (* use the older, slower layout algorithm *)
  ; mutable serverlib : bool
      (* If false, disable the python event library generation *)
  ; mutable old_ifelim : bool (* use the old if to match translation pass, which has a different set of bugs! *)
  ; mutable new_tofino : bool;
  }

(* TODO: We might want to add more parameters controlling which transformations
   are applied (if we're using the interpreter) *)

let default () =
  { verbose = true
  ; debug = false
  ; logging = true
  ; show_effects = false
  ; verbose_types = false
  ; show_tvar_links = false
  ; show_queries = false
  ; show_constraints = false
  ; use_type_names = true
  ; show_all_effects = false
  ; partial_interp = true
  ; spec_file = ""
  ; symb_file = ""
  ; dpt_file = ""
  ; show_interp_state = true
  ; interactive = false
  ; output = "lucid.output"
  ; json = false
  ; builddir = "lucid_tofino_build"
  ; portspec = None
  ; profile_cmd = None
  ; ctl_fn = None
  ; old_layout = false
  ; serverlib = false
  ; old_ifelim = false
  ; new_tofino = false
  }
;;
let cfg = default ()
let set_dpt_file fname = cfg.dpt_file <- fname

let parse_common () =
  let unset_verbose () = cfg.verbose <- false in
  let set_debug () = cfg.debug <- true in
  let set_effects () = cfg.show_effects <- true in
  let set_types () = cfg.verbose_types <- true in
  let set_tvars () = cfg.show_tvar_links <- true in
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
  let set_partial_interp () = cfg.partial_interp <- false in
  let set_json () =
    cfg.json <- true;
    cfg.verbose <- false
  in
  let set_serverlib () = cfg.serverlib <- true in
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
    ; ( "--json"
      , Arg.Unit set_json
      , "If true, print all interpreter output as json records" )
    ; ( "--no-partial-interp"
      , Arg.Unit set_partial_interp
      , "If true, disable partial interpretation for the program" )
    ; ( "--serverlib"
      , Arg.Unit set_serverlib
      , "If true, generate the python event library generator" ) 
      ]
  in
  speclist
;;

let parse_interp () =
  (* common options *)
  let speclist = parse_common () in
  (* added options for interp *)
  let set_final_state () = cfg.show_interp_state <- false in
  let set_interactive () =
    cfg.interactive <- true;
    cfg.verbose <- false;
  in
  let set_spec s = cfg.spec_file <- s in
  let speclist =
    speclist
    @ [ ( "--spec"
        , Arg.String set_spec
        , "Path to the interpreter specification file" )
      ; ( "--suppress-final-state"
        , Arg.Unit set_final_state
        , "If set, don't print the final state of the interpreter when it \
           finishes." )
      ; ( "--interactive"
        , Arg.Unit set_interactive
        , "Run interpreter interactively, piping events to/from stdin/stdout" )
      ; ( "-i"
        , Arg.Unit set_interactive
        , "Run interpreter interactively, piping events to/from stdin/stdout" )
      ]
  in
  let target_filename = ref "" in
  let usage_msg = "Lucid command line. Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

let parse_tofino () =
  (* common options *)
  let speclist = parse_common () in
  (* added options for tofino backend *)
  let set_builddir (s : string) = cfg.builddir <- s in
  let set_portspec (s : string) = cfg.portspec <- Some s in
  let set_profile_cmd (s : string) = cfg.profile_cmd <- Some s in
  let set_ctl_fn (s : string) = cfg.ctl_fn <- Some s in
  let set_old_layout () = cfg.old_layout <- true in
  let set_old_ifelim () = cfg.old_ifelim <- true in
  let speclist =
    speclist
    @ [ "-o", Arg.String set_builddir, "Output build directory."
      ; ( "--ports"
        , Arg.String set_portspec
        , "Path to the ports specification file" )
      ; ( "-p"
        , Arg.String set_profile_cmd
        , "Profile program instead of compiling." )
      ; "--control", Arg.String set_ctl_fn, "Python control program."
      ; "--oldlayout", Arg.Unit set_old_layout, "Use old layout algorithm." 
      ; "--oldifelim", Arg.Unit set_old_ifelim, "Use old if to match elimination algorithm."
      ; ( "--new-tofino"
      , Arg.Unit (fun () -> cfg.new_tofino <- true)
      , "If true, use the new tofino core IR and backend, which features parsers,\
        egress blocks, and optimizations to reduce phv pressure and stage utilization." )
      ]
  in
  let target_filename = ref "" in
  let usage_msg = "Lucid command line. Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

(* event parse / deparse generator for servers *)
let parse_serverlib () =
  let speclist = parse_common () in
  let set_output s = cfg.output <- s in
  let speclist = speclist @ ["-o", Arg.String set_output, "Output filename."] in
  let target_filename = ref "" in
  let usage_msg = "Lucid command line. Options available:" in
  Arg.parse speclist (fun s -> target_filename := s) usage_msg;
  !target_filename
;;

let parse = parse_interp

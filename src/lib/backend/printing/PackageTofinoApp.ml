(* Package a compiled P4 program and a switchd control program 
    into an app that can run on the Tofino simulator or 
    a physical switch. *)
open Printf
open FileUtil

(* these are all the artifacts in a package, that this 
module generates from a P4 and manager program string. *)
let libs_src_dir = "/tofinoLibs"
let p4_fn = "lucid.p4"
let c_fn = "lucid.cpp"
let py_fn = "lucid.py"

let silent = ref false ;;
let report str = 
  if (not !silent) then (
  Console.show_message str ANSITerminal.Green "Packager"
  )

(* runtime libraries + launcher *)
let copy_libs builddir =
  let rtLibs =
    Core.Filename.dirname (Core.Filename.realpath Sys.argv.(0)) ^ libs_src_dir
  in
  report ("copying runtime libraries from: " ^ rtLibs);
  FileUtil.cp ~force:FileUtil.Force ~recurse:true [rtLibs] (builddir ^ "/libs")
;;

(* P4 binary *)
let write_p4 builddir p4_str =
  report (sprintf "Writing P4 to: %s" p4_fn);
  IoUtils.writef (builddir ^ "/" ^ p4_fn) p4_str
;;

(* bf_switchd agent *)
let write_cpp builddir c_str =
  report (sprintf "Writing C to: %s" c_fn);
  IoUtils.writef (builddir ^ "/" ^ c_fn) c_str
;;

(* python manager setup script *)
let write_py builddir py_str =
  report (sprintf "Writing python to: %s" py_fn);
  IoUtils.writef (builddir ^ "/" ^ py_fn) py_str
;;

let write_mk builddir =
  let makef = builddir ^ "/makefile" in
  report (sprintf "generating makefile: %s " makef);
  (* generic start of makefile. *)
  let run_args_string = "$(RUN_ARGS)" in 
  let make_str_prefix =
    {x|
MAKEFLAGS += -s
# If the first argument is "test"...
ifeq (test,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
   $(eval $(RUN_ARGS):dummy)
endif
.PHONY: test
|x}
  in
  (* body of makefile *)
  let make_str =
    [%string
      "build: %{p4_fn} %{py_fn} %{c_fn}\n\
       \t./libs/p4tapp.sh build %{p4_fn}\n\
       hw:\n\
       \t./libs/p4tapp.sh hw %{p4_fn} ports_up\n\
       test:\n\
       \t./libs/p4tapp.sh test %{p4_fn} %{run_args_string}\n\
       hw:\n\
       \t./libs/p4tapp.sh hw %{p4_fn} ports_up\n\
       dummy:"]
  in
  IoUtils.writef makef (make_str_prefix ^ make_str)
;;

let generate p4_str c_str py_str builddir =
  report ("Packaging lucid tofino app in: " ^ builddir);
  copy_libs builddir;
  write_p4 builddir p4_str;
  write_cpp builddir c_str;
  write_py builddir py_str;
  write_mk builddir;
  report ("Lucid app generation complete in: " ^ builddir)
;;

(*   report (sprintf "Writing mgr C to: %s" mgr_fn);
  IoUtils.writef (builddir^"/"^mgr_fn) mgr_str
 *)

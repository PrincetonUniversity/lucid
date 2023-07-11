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
let py_eventlib_fn = "eventlib.py"
let globals_dir_fn = "globals.json"


let make_fn = "makefile"
let make_str = 
  let run_args_string = "$(RUN_ARGS)" in 
  let make_hdr =
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
  let make_body =
    [%string
      "build: %{p4_fn} %{py_fn} %{c_fn}\n\
       \t./libs/p4tapp.sh build %{p4_fn}\n\
       hw:\n\
       \t./libs/p4tapp.sh hw %{p4_fn}\n\
       test:\n\
       \t./libs/p4tapp.sh test %{p4_fn} %{run_args_string}\n\
       sim:\n\
       \t./libs/p4tapp.sh sim %{p4_fn}\n\
       dummy:"]
  in
  make_hdr ^ make_body
;;

let manifest_fn = "manifest.txt"

let manifest_str = [%string 
"\
Lucid-generated tofino project folder\
Contents: \n\
 %{p4_fn} -- P4 data plane program\n\
 %{py_fn} -- Python script to install multicast rules after starting %{p4_fn}\n\
 %{py_eventlib_fn} -- Python event parsing library\n\
 %{globals_dir_fn} -- Globals name directory (maps lucid global variable names to names in compiled P4)\n\
 %{make_fn} -- simple makefile to build and run P4 program\n\
 %{c_fn} -- c control plane (currently unused)\n\
"]
;;

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

let writef builddir fn str =
  IoUtils.writef (builddir ^ "/" ^ fn) str
;;

let generate p4_str c_str py_str py_eventlib globals_dir builddir =
  report ("Packaging lucid tofino app in: " ^ builddir);
  copy_libs builddir;
  let outputs = [
    (p4_fn, p4_str);
    (c_fn, c_str);
    (py_fn, py_str);
    (py_eventlib_fn, py_eventlib);
    (globals_dir_fn, globals_dir);
    (manifest_fn, manifest_str);
    (make_fn, make_str)
  ] in
  List.iter 
    (fun (fn, str) -> 
      writef builddir fn str)
    outputs;
  report ("Lucid app generation complete in: " ^ builddir)
;;

(*   report (sprintf "Writing mgr C to: %s" mgr_fn);
  IoUtils.writef (builddir^"/"^mgr_fn) mgr_str
 *)

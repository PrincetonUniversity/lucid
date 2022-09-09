(* misc IO utils for tofino compiler. *)
open Printf

exception Error of string

let error s = raise (Error s)

(* read file *)
let readf fname = Core.In_channel.read_all fname
let writef fname outstr = Core.Out_channel.write_all fname ~data:outstr

let cpy_src_to_build fn build_dir =
  (* copy fn to output directory. *)
  let base_fn = Filename.basename fn in
  let new_fn = sprintf "%s/src/%s" build_dir base_fn in
  let cmd = sprintf "cp %s %s" fn new_fn in
  let _ = Sys.command cmd in
  new_fn
;;

let basefn_of fn = Filename.remove_extension (Filename.basename fn)

(* rm -rf *)
let rec rmrf path =
  match Sys.file_exists path with
  | true ->
    (match Sys.is_directory path with
    | true ->
      Sys.readdir path
      |> Array.iter (fun name -> rmrf (Filename.concat path name));
      Unix.rmdir path
    | false -> Sys.remove path)
  | false -> ()
;;

(* remove all the files from a directory. *)
let clear_dir path =
  match Sys.file_exists path with
  | true ->
    (match Sys.is_directory path with
    | true ->
      (* remove non-directories *)
      Array.iter
        (fun fn ->
          let abs_fn = Filename.concat path fn in
          match Sys.is_directory abs_fn with
          | false -> Sys.remove abs_fn
          | true -> ())
        (Sys.readdir path)
    | false -> Sys.remove path)
  | false -> ()
;;

(* delete the directory and recreate it. *)
let clear_dir_recursive dir =
  rmrf dir;
  Core.Unix.mkdir_p dir
;;

(* Make sure that a directory exists. Create it if not. *)
let ensure_dir dir =
  match Sys.file_exists dir with
  | true ->
    (match Sys.is_directory dir with
    | true -> ()
    | false ->
      error ("tried to create directory " ^ dir ^ ", but that's already a file."))
  | false -> Core.Unix.mkdir_p dir
;;

let copy_dir src dst =
  Console.report (sprintf "copying: %s --> %s" src dst);
  FileUtil.cp ~recurse:true [src] dst
;;

(* load a template file that uses 1 formatter. *)
let load_format_1 infn =
  let instr = readf infn in
  Scanf.format_from_string instr "%s"
;;

let outDir = ref "./LucidCompileLogs"
let srcDir = ref (!outDir ^ "/src")
let logDir = ref (!outDir ^ "/logs")
let scriptsDir = ref (!outDir ^ "/scripts")

(* Prepare the build directory. *)
let setup_build_dir out_dir =
  outDir := out_dir;
  (* print_endline (sprintf "clearing build directory: %s\n" !outDir); *)
  clear_dir_recursive !outDir;
  logDir := !outDir ^ "/logs";
  ensure_dir !logDir;
  srcDir := !outDir ^ "/src";
  ensure_dir !srcDir;
  scriptsDir := !outDir ^ "/scripts";
  ensure_dir !scriptsDir;
  BackendLogging.moduleLogDir := !logDir ^ "/modules";
  BackendLogging.irLogDir := !logDir ^ "/ir";
  BackendLogging.graphLogDir := !logDir ^ "/graphs";
  ensure_dir !BackendLogging.irLogDir;
  ensure_dir !BackendLogging.moduleLogDir;
  ensure_dir !BackendLogging.graphLogDir
;;

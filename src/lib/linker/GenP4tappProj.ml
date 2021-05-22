(* wrap a DPT-generated P4 program in a P4tApp project *)
open Printf 
open IoUtils
module CL = Caml.List

let templates_dir = (Filename.dirname Sys.argv.(0))^"/tofinoAssets/p4tappTemplates"
let p4tjson_basename = "p4tapp.json"
let p4tappfn_of dir base = dir ^ "/" ^ base ^ ".p4tapp"
;;

let p4tgen_report str = Console.show_message str ANSITerminal.Green "P4tapp generator"
;;

let report_args p4fn dptfn builddir p4tappfn = 
	p4tgen_report (sprintf "builddir: %s" builddir);
	p4tgen_report (sprintf "dptfn: %s" dptfn);
	p4tgen_report (sprintf "p4fn: %s" p4fn);
	p4tgen_report (sprintf "p4tappfn: %s" p4tappfn)
;;

(* read json template, replace single format string, write string to same file. *)
let update_p4tapp_config json_fn local_p4fn = 
	p4tgen_report (sprintf "updating p4tapp config json in %s with P4 src name: %s" json_fn local_p4fn);	
	let in_fmt = load_format_1 json_fn in 
	sprintf in_fmt local_p4fn |> writef json_fn
;;

(* TODO: 5/14/21 -- update this to print manager code. *)
let generate p4_str p4fn dptfn builddir p4tappfn = 
	p4tgen_report ("Generating P4Tapp project in: "^(p4tappfn));
	IoUtils.writef (lucidp4fn_of builddir p4fn) p4_str;
	report_args p4fn dptfn builddir p4tappfn;
	(* 1. copy template to target directory. *)
	let full_p4tapp_src_dir = p4tappfn_of templates_dir (basefn_of p4tappfn) in 
	let full_p4tapp_dst_dir = p4tappfn_of builddir (basefn_of dptfn) in 
	p4tgen_report ("setting up P4tapp project in "^full_p4tapp_dst_dir);
	copy_dir full_p4tapp_src_dir full_p4tapp_dst_dir;
	(* 2. copy linked P4 program  to p4tapp/p4src/ *)
	let full_p4lucid_src = lucidp4fn_of builddir (Filename.basename p4fn) in 
	let p4tapp_p4_srcdir = full_p4tapp_dst_dir ^ "/p4src" in 
	IoUtils.ensure_dir p4tapp_p4_srcdir;
	let full_p4lucid_dst = lucidp4fn_of p4tapp_p4_srcdir (Filename.basename p4fn) in 
	copy_dir full_p4lucid_src full_p4lucid_dst;
	(* 3. update p4tapp.json in target directory. *)
	let full_p4tjson = full_p4tapp_dst_dir ^"/"^p4tjson_basename in 
	let p4tapp_local_p4lucid_fn = "p4src/"^(Filename.basename full_p4lucid_dst) in 
	update_p4tapp_config full_p4tjson p4tapp_local_p4lucid_fn
;;

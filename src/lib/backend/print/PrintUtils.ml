open Batteries
open Format
open Base

let tab () =
  pp_open_vbox str_formatter 4;
  ()
;;

let untab () =
  pp_close_box str_formatter ();
  ()
;;

let print s = fprintf str_formatter s
let getstr () = flush_str_formatter ()
let lineSep fmt () = fprintf fmt "@,"
let spaceSep fmt () = fprintf fmt "@"
let commaSep fmt () = fprintf fmt ", "
let semiSep fmt () = fprintf fmt "; "

let indent_block ?(nspaces = 4) s : string =
  String.concat
    ~sep:"\n"
    (List.map (String.split s ~on:'\n') ~f:(fun line ->
         String.make nspaces ' ' ^ line))
;;

let new_tab () = 
  pp_open_vbox str_formatter 4;
  (* when you open a box, the first line doesn't indent... so we adjust. *)
  fprintf str_formatter "    ";(* *)
  ()

let new_open_block () = 
  pp_open_vbox str_formatter 0;
  new_tab ()
;;  

(* open a named block that the linker will recognize *)
let open_block () = 
  pp_open_vbox str_formatter 0;
  tab ()
(*   fprintf str_formatter "@(%s)@," blockname *)
;;
let close_block () = 
  untab ();
(*   fprintf str_formatter "@,@(%s)@," blockname;
  fprintf str_formatter "@,"; *)
  pp_close_box str_formatter ();
  let p4_obj_block = flush_str_formatter () in
  p4_obj_block
  (* Caml.Printf.fprintf out_f "%s" p4_obj_block *)
;;
let ppLineSep fmt () = fprintf fmt "@,"
let ppPrintf fmt str = fprintf fmt "%s" str
;;

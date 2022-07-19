(* TODO: extend the syntax, lexer, and parser to implement P4 (or a subset of it) *)

open Dpt
open Console
open ANSITerminal
open Core
open P4Lexer
open Lexing 

exception Error of string
let error s = raise (Error s)
let report str = Console.show_message str ANSITerminal.Green "linker"

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try P4Parser.prog P4Lexer.token lexbuf with
  | LexError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
;;

let main () = 
  let p4fn = Cmdline.parse () in 
  Console.read_file p4fn;
  print_endline @@ "input file: " ^ p4fn;

  let inx = In_channel.create p4fn in 
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = p4fn};

  let stmts = parse_with_error lexbuf in 
  print_endline ("--- evaluating ---");
  let _ = P4Syntax.eval_stmts [] stmts in
  print_endline ("--- done evaluating ---");

;;

let _ = main ()
  

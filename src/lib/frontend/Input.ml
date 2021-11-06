open Batteries

let debug s = print_endline s

let read ?(filename : string option = None) lexbuf =
  let get_info () =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    tok, line, cnum
  in
  let err_header =
    match filename with
    | None -> Printf.sprintf "[Parser]"
    | Some s -> Printf.sprintf "[Parser] %s:" s
  in
  try Parser.prog Lexer.token lexbuf with
  | Failure x -> Console.error (Printf.sprintf "%s %s" err_header x)
  | End_of_file ->
    Console.error (Printf.sprintf "%s end of file in comment" err_header)
  | _ ->
    let tok, line, cnum = get_info () in
    Console.error
      (Printf.sprintf
         "%s token: %s, line: %s, char: %s"
         err_header
         tok
         (string_of_int line)
         (string_of_int cnum))
;;

(* Make dest_fname relative to the current directory (or absolute),
   instead of relative to the source_fname *)
let adjust_filename source_fname dest_fname =
  dest_fname
  |> FilePath.concat (FilePath.dirname source_fname)
  |> FilePath.reduce ~no_symlink:true
;;

let rec read_from_file fname visited : 'a list * string list =
  if List.mem fname visited
  then [], []
  else (
    let fin = open_in fname in
    let lexbuf = Lexing.from_channel fin in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
    lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = fname };
    let res = read ~filename:(Some fname) lexbuf in
    close_in fin;
    match res with
    | [], d -> d, [fname]
    | f, d ->
      let ajust_f = List.map (adjust_filename fname) f in
      List.fold_left
        (fun a b ->
          let pf = read_from_file b (fname :: visited) in
          fst pf @ fst a, snd a @ snd pf)
        (d, [fname])
        ajust_f)
;;

let parse fname =
  let ajust_fname = adjust_filename FilePath.current_dir fname in
  let ds = read_from_file ajust_fname [] in
  Console.read_files (snd ds);
  fst ds
;;

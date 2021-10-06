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

let read_from_file fname =
  let fin = open_in fname in
  let lexbuf = Lexing.from_channel fin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = fname };
  let res = read ~filename:(Some fname) lexbuf in
  close_in fin;
  res
;;

(* Make dest_fname relative to the current directory (or absolute),
   instead of relative to the source_fname *)
let adjust_filename source_fname dest_fname =
  dest_fname
  |> FilePath.concat (FilePath.dirname source_fname)
  |> FilePath.reduce ~no_symlink:true
;;

(* Process include directives: return a list of filenames to be processesed
   in order. Do not include the same file more than once *)
let process_includes (fname : string) : string list =
  let rec process_includes_aux (seen, imports) fname =
    (* print_endline @@ "Processing " ^ fname; *)
    if List.mem fname seen
    then seen, imports
    else (
      (* Get any imports in this file *)
      let lines =
        try File.lines_of fname with
        | _ -> Console.error ("File not found: " ^ fname)
      in
      let includes =
        Enum.take_while
          (fun s -> String.starts_with s "include")
          (Enum.filter
             (fun s ->
               (not @@ String.starts_with s "//") && String.trim s <> "")
             lines)
      in
      let imported_fnames =
        Enum.map
          (fun s ->
            if Str.string_match (Str.regexp "include[ ]*\\\"\\(.+\\)\\\"") s 0
            then adjust_filename fname (Str.matched_group 1 s)
            else
              Console.error
              @@ "Bad include directive (did you forget the quotes?): "
              ^ s)
          includes
      in
      (* Recursively process those imports *)
      let rec_seen, rec_imports =
        Enum.fold process_includes_aux (fname :: seen, imports) imported_fnames
      in
      rec_seen, fname :: rec_imports)
  in
  let _, imports =
    process_includes_aux ([], []) (adjust_filename FilePath.current_dir fname)
  in
  List.rev imports
;;

let parse fname =
  let files_to_parse = process_includes fname in
  Console.read_files files_to_parse;
  let ds = List.concat (List.map read_from_file files_to_parse) in
  ds
;;

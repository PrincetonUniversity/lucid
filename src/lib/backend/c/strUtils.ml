(* string helpers -- useful throughout the compiler, but here for now *)
open Printf

let char_sep c f ls = 
  String.concat c (List.map f ls)
let underscore_sep f ls = char_sep "_" f ls;;
let comma_sep f ls = char_sep ", " f ls;;
let sp n = String.make n ' '
let indent n str = 
  String.split_on_char '\n' str 
  |> List.map (fun s -> (sp n)^s)
  |> String.concat "\n"
;;

let indent_def str = indent 2 str;;
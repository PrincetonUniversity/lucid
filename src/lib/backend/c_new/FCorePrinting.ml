open FCoreSyntax

let decl_to_string decl = 
  FCoreToCore.translate_decl decl |> CorePrinting.decl_to_string
;;
let decls_to_string decls = List.map decl_to_string decls |> String.concat "\n";;
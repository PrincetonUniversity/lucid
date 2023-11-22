(* Simple direct translation pass from core parts of the Lucid IR to c *)
open CoreSyntax

let translate_decls decls = let _ = decls in "//decls";;
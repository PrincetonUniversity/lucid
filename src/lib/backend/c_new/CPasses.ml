
let compile ds = 
  (* 1. translate to core syntax *)
  let ds = SyntaxToCore.translate_prog ~preserve_user_decls:true ds in
  (* 2. run partial interpretation *)
  let ds = PartialInterpretation.interp_prog ds in
  let core_str = CorePrinting.decls_to_string ds in
  (* 3. translate to FCore *)
  let fds = CoreToFCore.translate_prog ds in
  print_endline ("translation to FCore complete");
  let (fds' : FCoreSyntax.fdecl list) = fds in 
  let ds' = FCoreToCore.translate_prog fds' in
  core_str

;;

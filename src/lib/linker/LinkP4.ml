(* 
  links a compiled DPT program into a P4 program. 

  The core of this module is a partial P4 parser that recognizes statements, labeled blocks, 
  and pragmas using the angstrom parser combinator library: 
  https://docs.mirage.io/angstrom/Angstrom/index.html#type-t
  https://discuss.ocaml.org/t/angstrom-parser-optimization/1754
  https://ocamlverse.github.io/content/monadic-parsers-angstrom.html
*)

module CL = Caml.List
module BS = Batteries.String
open Printf
open Format
open Angstrom
module A = Angstrom
module DBG = BackendLogging

let outc = ref None
let dprint_endline = ref DBG.no_printf
let start_log () = DBG.start_mlog __FILE__ outc dprint_endline

exception Error of string

let error s = raise (Error s)
let linker_report str = Console.show_message str ANSITerminal.Green "linker"

(* wrapper for angstrom's lift2 with params in a different order *)
let parse2_map parse_1 parse_2 map_f = A.lift2 map_f parse_1 parse_2

let parse3_map parse_1 parse_2 parse_3 map_f =
  A.lift3 map_f parse_1 parse_2 parse_3
;;

(* the linker parses a P4 program as a sequence 
  of statements and (possibly nested) blocks. *)
type code_expr = { codestr : string }

type code_stmt =
  | LNoop of code_expr (* a noop is something that we never transform, like a comment or whitespace. *)
  | LExpr of code_expr
  | LStmt of code_expr
  (* a block is anything with a label and a body. Like an action, table, or control. *)
  | LBlock of
      { lb_label : code_expr
      ; lb_body : code_stmt
      } (* label * body *)
  (* a pragma is something that should be replaced by compiler code. *)
  | LPragma of
      { lb_name : code_expr
      ; lb_exprs : code_expr
      }
  | LSeq of code_stmt list
  | LAdded of code_expr

let code_expr_equals c1 c2 =
  Batteries.String.trim c1.codestr = Batteries.String.trim c2.codestr
;;

let code_expr_contains_str c s = BS.exists c.codestr s

(* constructors *)
let to_code_expr str = { codestr = str }
let to_noop str = LNoop (to_code_expr str)
let to_lexpr str = LExpr (to_code_expr str)
let to_lstmt str = LStmt (to_code_expr str)

let to_lblock_direct label_exp body_st =
  LBlock { lb_label = label_exp; lb_body = body_st }
;;

let to_lblock label_st body_st =
  match label_st with
  | LExpr label_exp -> to_lblock_direct label_exp body_st
  | _ -> error "expected an LExpr for lblock"
;;

let to_lseq ss = LSeq ss

let to_pragma pname pbody =
  LPragma { lb_name = to_code_expr pname; lb_exprs = to_code_expr pbody }
;;

let to_pragma_name pname =
  LPragma { lb_name = to_code_expr pname; lb_exprs = to_code_expr "" }
;;

let to_added str = LAdded { codestr = str }

(* destructors *)
let stmts_of_block cs =
  match cs with
  | LBlock { lb_body = LSeq lss } -> lss
  | _ -> error "[stmts_of_block] unexpected form of code stmt"
;;

let sigstring_of_block cs =
  match cs with
  | LBlock { lb_label = { codestr = st } } -> st
  | _ -> error "[sigstring_of_block] unexpected form of code stmt"
;;

(* printers *)
let string_of_code_str cs = cs.codestr

let rec string_of_code_stmt ls =
  match ls with
  | LNoop cs -> string_of_code_str cs
  | LExpr cs -> string_of_code_str cs ^ ""
  | LStmt ls -> string_of_code_str ls ^ ";"
  | LBlock { lb_label = lbl; lb_body = bdy } ->
    string_of_code_str lbl ^ "{" ^ string_of_code_stmt bdy ^ "}"
  | LPragma { lb_name = lbl; lb_exprs = bdy } ->
    "@" ^ string_of_code_str lbl ^ "(" ^ string_of_code_str bdy ^ ")"
  | LSeq lss -> String.concat "" (CL.map string_of_code_stmt lss)
  | LAdded cs -> string_of_code_str cs
;;

let stf = str_formatter

let tab () =
  pp_open_vbox stf 4;
  ()
;;

let untab () =
  pp_close_box stf ();
  ()
;;

let getstr () = flush_str_formatter ()
let print s = fprintf stf s

let rec dbg_pprint_code_stmt ls =
  match ls with
  | LNoop cs ->
    fprintf stf "Noop [[%s]]@," (String.trim (string_of_code_str cs))
  | LAdded cs ->
    fprintf stf "Added [[%s]]@," (String.trim (string_of_code_str cs))
  | LExpr cs ->
    fprintf stf "Expr [[%s]]@," (String.trim (string_of_code_str cs))
  | LStmt ls ->
    fprintf stf "Stmt [[%s;]]@," (String.trim (string_of_code_str ls))
  | LBlock { lb_label = lbl; lb_body = bdy } ->
    tab ();
    fprintf stf "Block [[%s]]{@," (String.trim (string_of_code_str lbl));
    dbg_pprint_code_stmt bdy;
    untab ();
    fprintf stf "} EndBlock [[%s]]@," (String.trim (string_of_code_str lbl))
  | LPragma { lb_name = lbl; lb_exprs = bdy } ->
    fprintf
      stf
      "Pragma [[%s]](%s)@,"
      (String.trim (string_of_code_str lbl))
      (String.trim (string_of_code_str bdy))
  | LSeq lss ->
    tab ();
    fprintf stf "Seq [[@,";
    CL.iter dbg_pprint_code_stmt lss;
    untab ();
    fprintf stf "]] EndSeq @,"
;;

let rec dbg_string_of_code_stmt ls =
  pp_open_vbox stf 0;
  dbg_pprint_code_stmt ls;
  getstr ()
;;

let num_stmts s =
  match s with
  | LNoop _ -> 0
  | LExpr _ -> 0
  | LAdded _ -> 1
  | LStmt _ | LBlock _ | LPragma _ -> 1
  | LSeq ss -> CL.length ss
;;

let dbg_print_stree ls =
  print_endline ("number of nodes: " ^ string_of_int (num_stmts ls));
  print_endline "----- debug print -----";
  print_endline (dbg_string_of_code_stmt ls);
  print_endline "----- real print -----";
  print_endline (string_of_code_stmt ls)
;;

(* Token definitions *)
let is_whitespace c =
  match c with
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_not_stmt_delim c =
  match c with
  ';' | '@' -> false
  | _ -> true
;;

let not_body_start_delim c = 
  match c with 
  | '{' -> false
  | _ -> true

let not_body_end_delim c = 
  match c with 
  | '}' -> false
  | _ -> true

(* a block label cannot possibly have these 
   chars in it. If we reach one, we have 
   reached either the end of the label, 
   OR the string we are parsing is not 
   a block label.  *)
let block_label_chars c = 
  match c with 
  | '@' | ';'| '}' | '{' -> false
  | _ -> true


let is_not_pragma_delim c =
  match c with
  | '}' | '{' | ';' | ')' | '(' -> false
  | _ -> true
;;

let valid_statement_start c = 
  match c with 
  | '}' | '{' | ')' | '@'-> false
  | _ -> true
;;

let valid_blocklabel_start c = 
  match c with 
  | '@'-> false
  | _ -> true
;;



let valid_block_label c = 
  match c with 
  | '{' -> false
  | _ -> true

let is_end_of_pragma_name c = is_whitespace c || not (is_not_pragma_delim c)

(* parsers *)
(* <noop:code_expr>*)
let whitespace = take_while is_whitespace
let whitespace_lstmt = whitespace >>| to_noop



(* <statement:code_expr>; *)
let statement =
  map2
    (* after stripping whitespace, a statement must start 
       with a certain character. This disambiguates statements 
       with nested {'s from blocks. *)
    (
      map2 
        whitespace
        (A.satisfy valid_statement_start)
        (fun wss fc -> (Batteries.String.to_list wss@[fc]) |> Batteries.String.of_list)
    )
    (* the rest of the characters can be anything until a ; *)
    (take_while is_not_stmt_delim <* A.char ';')
    (fun sa sb ->
      let codestr =  sa^sb in 
      print_endline ("[statement]"^(codestr));
      to_lstmt codestr
    )
;;

(*@<name:code_expr>(<body:code_expr>)*)
let pragma_with_params =
  A.string "@"
  *> (take_till is_end_of_pragma_name
     <* A.char '('
     >>= fun pname_str ->
     take_while is_not_pragma_delim
     <* A.char ')'
     >>| fun pbdy_str -> to_pragma pname_str pbdy_str)
;;

(* @<name:code_expr> *)
let name_pragma =
  A.string "@" *> take_till is_end_of_pragma_name
  >>| fun pname_str -> to_pragma_name pname_str
;;

let raw_pragma = pragma_with_params <|> name_pragma

let pragma =
  whitespace
  >>= fun ws -> raw_pragma >>| fun lprag -> to_lseq [to_lexpr ws; lprag]
;;

(* <delim> <text> EOL *)
let newline_delim_stmt delim_str =
  parse2_map
    whitespace
    (A.string delim_str *> A.many_till A.any_char A.end_of_line >>| BS.of_list)
    (fun wss cs -> to_noop (wss ^ delim_str ^ cs ^ "\n"))
;;

(* single line comments, includes, and defines *)
let line_comment = newline_delim_stmt "//"
let line_include = newline_delim_stmt "#include"
let line_define = newline_delim_stmt "#define"

(* multiline comments *)
let multiline_comment =
  parse2_map
    whitespace
    (A.string "/*" *> A.many_till A.any_char (A.string "*/") >>| BS.of_list)
    (fun wss cs -> to_noop (wss ^ "/*" ^ cs ^ "*/"))
;;

let comments = 
  line_comment
  <|> line_include
  <|> line_define
  <|> multiline_comment


(* all statements besides blocks *)
let basic_statement =
      pragma
  <|> statement
;;




let isnot b = not b ;;
(* <label:code_expr>:lexpr {<body>:lseq} *)
let block =
  fix (fun block ->
    print_endline "[start block parse]";
      (* a block label is a sequence of characters with no delims *)
      let block_label =
        take_while (block_label_chars)
        >>| fun ps ->
        print_endline ("[block_label]"^(ps));
        to_lexpr ps
      in       

      (* the body of a block is a sequence of statements, pragmas, or blocks *)
      let block_body =
        parse2_map
          (    
               A.char '{' (* take the {, throw it away *)
            *> A.many (comments <|> block<|> basic_statement ) 
            (* take many comments, blocks, or basic statements, return a list of parsed things *)
          ) 
          (    
               whitespace_lstmt (* take whitespace *)
            <* A.char '}' (* take the }, throw it away. *)
            >>| fun v -> 
            [v] (* return the whitespace object. *)
          )
          (
            fun bdy_lstmts ws_stmts -> 
              (* combine the statements and the trailing whitespace statement 
                 into a sequence of statements. *)
              let entire_body = to_lseq (bdy_lstmts @ ws_stmts) in 
              print_endline ("[block_body] "^(dbg_string_of_code_stmt entire_body));
              entire_body
          )
            (* after parsing both, make a sequence *)
      in
      (* a block is a label followed by a body *)
      let whole_block = parse2_map 
        block_label 
        block_body 
        (fun bl bb -> 
          let res = to_lblock bl bb in 
          print_endline ("[complete block] "^(dbg_string_of_code_stmt res));
          res

        )
        (* to_lblock  *)
      in
      whole_block)
;;

(* a program is a sequence of basic statements and blocks, surrounded 
by whitespace that we discard *)
let prog =
  whitespace *> (A.many (comments <|> block <|> basic_statement ) >>| to_lseq) <* whitespace
;;

let parse_prog progstr =
  let res = Angstrom.parse_string ~consume:Consume.Prefix prog progstr in
  match res with
  | Ok ls -> ls
  | Error msg ->
    printf "error!\n";
    failwith msg
;;

(*** tokenize code in parts of the tree, as needed ***)

module Tok = struct
  let is_ident_start c =
    match c with
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_ident c =
    match c with
    | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false
  ;;

  let is_delim c = (not (is_ident_start c)) && not (is_ident c)

  type p4token =
    | Ident of string
    | Delim of string

  type p4tokens = p4token list

  let p_Ident = take_while1 is_ident >>| fun s -> Ident s
  (*     parse2_map
      (take_while1 is_ident_start)
      (take_while is_ident)
      (fun s1 s2 -> s1 ^ s2)
    >>| fun s -> Ident s *)

  let p_Delim = take_while1 is_delim >>| fun s -> Delim s
  let p_p4tokens = A.many (p_Ident <|> p_Delim) <* A.end_of_input

  let tokenize_codeexpr ce =
    print_endline ("[tokenize_codeexpr] tokenizing:\n" ^ string_of_code_str ce);
    let res =
      Angstrom.parse_string ~consume:Consume.All p_p4tokens ce.codestr
    in
    match res with
    | Ok ts -> ts
    | Error msg ->
      printf "error tokenizing string!\n";
      failwith msg
  ;;

  let string_of_p4token t =
    match t with
    | Ident s | Delim s -> s
  ;;

  let tokens_contains (tok_str : string) (ts : p4tokens) : bool =
    CL.map string_of_p4token ts |> CL.exists (BS.equal tok_str)
  ;;

  let code_expr_contains_token tok_str ce =
    tokenize_codeexpr ce |> tokens_contains tok_str
  ;;
end

(*** insert generated code ***)
let rec replace_prag (prag_name : string) (new_str : string) (cs : code_stmt)
    : code_stmt
  =
  match cs with
  | LPragma { lb_name = lbl; _ } ->
    (match code_expr_equals lbl { codestr = prag_name } with
    | true ->
      to_added ("//Compiler-generated code for: @" ^ prag_name ^ "\n" ^ new_str)
    | false -> cs)
  | LBlock brec ->
    LBlock { brec with lb_body = replace_prag prag_name new_str brec.lb_body }
  | LSeq lss -> LSeq (CL.map (replace_prag prag_name new_str) lss)
  | _ -> cs
;;

(*** does the pragma exist in the code? ***)
let rec prag_exists (prag_name : string) (cs : code_stmt) : bool =
  match cs with
  | LPragma { lb_name = lbl; _ } -> code_expr_equals lbl { codestr = prag_name }
  | LBlock brec -> prag_exists prag_name brec.lb_body
  | LSeq lss -> CL.map (prag_exists prag_name) lss |> CL.exists (fun f -> f)
  | _ -> false
;;

(***** transformation helpers *****)
(* if cs.name == bname -> tfun cs
   else -> cs *)
let rec find_and_transform bname tfun cs : code_stmt =
  print_endline (sprintf "[find_and_transform] looking for: %s" bname);
  match cs with
  | LBlock brec ->
    (match Tok.code_expr_contains_token bname brec.lb_label with
    | true ->
      (*           print_endline (sprintf "[find_and_transform] found: %s in\n---\n%s\n---\n" bname (sigstring_of_block cs)); *)
      tfun cs
    | false ->
      LBlock { brec with lb_body = find_and_transform bname tfun brec.lb_body }
      (* recurse on block body *))
  | LSeq lss ->
    let lss_transformed = CL.map (find_and_transform bname tfun) lss in
    let new_cs = LSeq lss_transformed in
    new_cs
  | _ -> cs
;;

(**** ingress apply transformation ****)
let rec fold_split_f
    (prag_name : string)
    (pre_css, prag_cs, post_css)
    (cs : code_stmt)
    : code_stmt list * code_stmt option * code_stmt list
  =
  let non_match_outp () =
    match prag_cs with
    | None -> pre_css @ [cs], prag_cs, post_css
    | Some _ -> pre_css, prag_cs, post_css @ [cs]
  in
  match cs with
  | LPragma { lb_name = lbl; _ } ->
    (match code_expr_equals lbl { codestr = prag_name } with
    (* found --cs is the pragma *)
    | true -> pre_css, Some cs, post_css
    (* not found -- decide if cs is before or after. *)
    | false -> non_match_outp ())
  | LSeq inner_css ->
    (* step into a sequence *)
    CL.fold_left (fold_split_f prag_name) (pre_css, prag_cs, post_css) inner_css
  | _ -> non_match_outp ()
;;

(* split a list of code statements based on where the pragma with "prag_name" appears *)
let split_by_prag prag_name css =
  match CL.fold_left (fold_split_f prag_name) ([], None, []) css with
  | a, Some b, c -> a, b, c
  | _ -> error ("[split_by_prag] did not find pragma: " ^ prag_name)
;;

(* wrap the code statement sequence in a block "block_label" *)
let encap_in_block block_label css =
  to_lseq [to_lblock_direct block_label (to_lseq css); to_noop "\n\t"]
;;

(* takes an "apply" block, returns an "apply" block with the code 
   before and after dpt dispatch conditioned to not execute on 
   event packets. *)
let igr_condition_pre_and_post_dispatch igr_apply_cs =
  let igr_apply_lbl, igr_apply_stmts =
    match igr_apply_cs with
    | LBlock { lb_label; lb_body = cs } ->
      (match cs with
      | LSeq css -> lb_label, css
      | _ -> lb_label, [cs])
    | _ ->
      error "[transform_pre_dpt_ingress] ingress apply block... is not a block"
  in
  linker_report
    (sprintf
       "transforming existing P4 ingress.apply to skip background event \
        packets...");
  (* split it at @DPT_CALLS *)
  let pre_css, call_cs, post_css =
    split_by_prag Consts.dpt_igr_call igr_apply_stmts
  in
  (* wrap the pre_block with the if statement *)
  let pre_label =
    let branch_arg_name = Consts.branchArg |> fst |> Cid.to_id |> fst in
    to_code_expr
      (sprintf
         "\n\
          \t//Linker-added condition: don't apply pre-dispatch tables to \
          background event packets.\n\
          \tif (%s == 0)"
         (Consts.dpa_global_name ^ "." ^ branch_arg_name))
  in
  let pre_css_new = [encap_in_block pre_label pre_css] in
  let post_label =
    let exit_event_name = Consts.exitEventArg |> fst |> Cid.to_id |> fst in
    to_code_expr
      (sprintf
         "\n\
          \t//Linker-added condition: don't apply post-dispatch tables to \
          packets without exit events.\n\
          \tif (%s != 0)"
         (Consts.dpa_global_name ^ "." ^ exit_event_name))
  in
  let post_css_new = [encap_in_block post_label post_css] in
  let igr_apply_stmts = pre_css_new @ [call_cs] @ post_css_new in
  let igr_apply_cs = to_lblock_direct igr_apply_lbl (to_lseq igr_apply_stmts) in
  igr_apply_cs
;;

(* transform the apply block of the ingress so that the 
  code before the call to DPT does not execute 
  for event packets. *)
let ingress_bg_event_trans cs =
  (* transform the ingress block by:
      transforming the "apply" block using igr_condition_pre_and_post_dispatch *)
  let igr_tf igr_cs =
    print_endline
      (sprintf
         "[igr_tf] transforming block with sig: %s"
         (sigstring_of_block igr_cs));
    let new_igr_cs =
      find_and_transform "apply" igr_condition_pre_and_post_dispatch igr_cs
    in
    new_igr_cs
  in
  let cs = find_and_transform "Ingress" igr_tf cs in
  cs
;;

(* replace all the DPT pragmas with compiler generated P4 code *)
let pragma_replace_trans cs pragma_obj_dict =
  let fold_f p4_tree (pragname, newcode) =
    linker_report (sprintf "replacing block %s in P4" pragname);
    !dprint_endline
      ("[pragma_replace_trans] -------------- block: "
      ^ pragname
      ^ "--------------");
    !dprint_endline ("[pragma_replace_trans] code:\n" ^ newcode);
    (* Console.show_message newcode ANSITerminal.Green "linker"; *)
    let new_p4_tree = replace_prag pragname newcode p4_tree in
    match p4_tree <> new_p4_tree with
    | true -> new_p4_tree
    | false ->
      Console.warning (sprintf "did not replace DPT pragma %s in P4" pragname);
      new_p4_tree
  in
  let cs = CL.fold_left fold_f cs pragma_obj_dict in
  cs
;;

(* The public function. Link the lucid compiler's output into a 
P4 harness, produce a string output. *)
let link_p4 obj_dict p4fn =
  start_log ();
  let p4prog = IoUtils.readf p4fn in
  let tree_prog = parse_prog p4prog in
  dbg_print_stree tree_prog;
  (* exit 1; *)
  !dprint_endline (sprintf "[link_p4] p4_syntax_tree:");
  !dprint_endline (dbg_string_of_code_stmt tree_prog);
  (* transform the ingress function so that the code before and after the 
  call to DPT dispatch doesn't apply to event packets. *)
  (* if there is a call to DPT_DISPATCH, add the pre and post guards *)
  let tree_prog =
    if prag_exists Consts.dpt_igr_call tree_prog
    then ingress_bg_event_trans tree_prog
    else tree_prog
  in
  (* replace all the DPT pragmas with compiler generated P4 code *)
  let tree_prog = pragma_replace_trans tree_prog obj_dict in
  string_of_code_stmt tree_prog
;;

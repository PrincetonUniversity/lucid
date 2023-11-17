
{
  open Batteries
  open Parser
  open Printf
  open Span
  exception Eof

  let position lexbuf =
    Span.create
      ((lexbuf.Lexing.lex_start_p).pos_fname)
      (Lexing.lexeme_start lexbuf)
      (Lexing.lexeme_end lexbuf)

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                 Lexing.pos_bol = pos.Lexing.pos_cnum; } ;;

  let extract_string s =
    String.chop ~l:1 ~r:1 s

  let extract_bitpat s =
    s |> String.lchop ~n:2 |> String.to_list
    |> List.map (function | '0' -> 0 | '1' -> 1 | _ -> (-1))

  let extract_numwidth s =
    match String.split_on_char 'w' s with
    | [hd; tl] -> Z.of_string hd, Int.of_string tl
    | _ -> failwith "PARSING ERROR: Bad number with width?"
}

let num = ['0'-'9']+|'0''b'['0' '1']+|'0''x'['0'-'9''a'-'f''A'-'F']+
let width = 'w'num
let bitpat = '0''b'['0' '1' '*']+
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let wspace = [' ' '\t']
(* let str = '"'['a'-'z' 'A'-'Z' '_' '0'-'9' '~' '!' '@' '#' '$' '%' '^' '&' '|' ':' '?' '>' '<' '[' ']' '=' '-' '.' ' ' '\\' ',']*'"' *)
let str = '"'[^'"']*'"'
let filename = "\""(['a'-'z' 'A'-'Z' '0'-'9' '_' '\\' '/' '.' '-'])+"\""

rule token = parse
  | "/*"              { comments 0 lexbuf }
  | "//"              { comments (-1) lexbuf }
  | "include"         { INCLUDE (position lexbuf) }
  | "false"           { FALSE (position lexbuf) }
  | "true"            { TRUE (position lexbuf) }
  | "if"              { IF (position lexbuf) }
  | "else"            { ELSE (position lexbuf) }
  | "int"             { TINT (position lexbuf) }
  | "bool"            { TBOOL (position lexbuf) }
  | "event"           { EVENT (position lexbuf) }
  | "generate"        { GENERATE (position lexbuf) }
  | "generate_switch" { SGENERATE (position lexbuf) }
  | "generate_ports"  { MGENERATE (position lexbuf) }
  | "generate_port"   { PGENERATE (position lexbuf) }
  | "printf"          { PRINTF (position lexbuf) }
  | "handle"	        { HANDLE (position lexbuf) }
  | "fun"             { FUN (position lexbuf)}
  | "memop"(num as n) { MEMOP (position lexbuf, Int.of_string n) }
  | "memop"           { MEMOP (position lexbuf, 0) }
  | "return"          { RETURN (position lexbuf)}
  | "size"            { SIZE (position lexbuf) }
  | "global"          { GLOBAL (position lexbuf) }
  | "unordered"       { UNORDERED (position lexbuf) }
  | "const"           { CONST (position lexbuf) }
  | "extern"          { EXTERN (position lexbuf) }
  | "void"            { VOID (position lexbuf) }
  | "hash"            { HASH (position lexbuf) }
  | "auto"            { AUTO (position lexbuf) }
  | "group"           { GROUP (position lexbuf) }
  | "control"         { CONTROL (position lexbuf) }
  | "@egress"         { EGRESS (position lexbuf) }
  | "@"(num as n)     { ANNOT (position lexbuf, Int.of_string n) }
  | "packet"          { PACKET (position lexbuf) }
  | "match"           { MATCH (position lexbuf) }
  | "with"            { WITH (position lexbuf) }
  | "type"            { TYPE (position lexbuf) }
  | "noinline"        { NOINLINE (position lexbuf) }

  | "table_type"              { TABLE_TYPE (position lexbuf) }
  | "key_type:"               { KEY_TYPE (position lexbuf) }
  | "arg_type:"               { ARG_TYPE (position lexbuf) }
  | "ret_type:"               { RET_TYPE (position lexbuf) }
  | "action_constr"           { ACTION_CONSTR (position lexbuf) }
  | "action"                  { ACTION (position lexbuf) }
  | "table_create"            { TABLE_CREATE (position lexbuf) }
  | "table_match"             { TABLE_MATCH (position lexbuf) }
  | "table_install"           { TABLE_INSTALL (position lexbuf) }
  | "table_multi_install"     { TABLE_MULTI_INSTALL (position lexbuf) }

  | "parser"          { PARSER (position lexbuf) }
  | "read"            { READ (position lexbuf) }
  | "skip"            { SKIP (position lexbuf) }
  | "drop"            { DROP (position lexbuf) }
  | "bitstring"       { BITSTRING (position lexbuf)}

  | "constructor"     { CONSTR (position lexbuf) }
  | "constr"          { CONSTR (position lexbuf) }
  | "module"          { MODULE (position lexbuf) }
  | "end"             { END (position lexbuf) }
  | "for"             { FOR (position lexbuf) }
  | "size_to_int"     { SIZECAST (position lexbuf) }
  | "symbolic"        { SYMBOLIC (position lexbuf) }
  | "flood"           { FLOOD (position lexbuf) }
  | id as s           { ID (position lexbuf, Id.create s) }
  | "'"(id as s)      { QID (position lexbuf, Id.create s) }
  | num width as w    { NUMWITDH (position lexbuf, extract_numwidth w) }
  | num as n          { NUM (position lexbuf, Z.of_string n) }
  | bitpat as p       { BITPAT (position lexbuf, extract_bitpat p) }
  | "#"               { PROJ (position lexbuf) }
  | "|+|"             { SATPLUS (position lexbuf) }
  | "+"               { PLUS (position lexbuf) }
  | "|-|"             { SATSUB (position lexbuf) }
  | "-"               { SUB  (position lexbuf)}
  | "!"               { NOT (position lexbuf) }
  | "&&"              { AND (position lexbuf) }
  | "||"              { OR (position lexbuf) }
  | "&"               { BITAND (position lexbuf) }
  | "=="              { EQ (position lexbuf) }
  | "!="              { NEQ (position lexbuf)}
  | "<<"              { LSHIFT (position lexbuf) }
  | ">>"              { RSHIFT (position lexbuf) }
  | "<="              { LEQ (position lexbuf) }
  | ">="              { GEQ (position lexbuf) }
  | "<"               { LESS (position lexbuf) }
  | ">"               { MORE (position lexbuf)}
  | "^^"              { BITXOR (position lexbuf) }
  | "^"               { CONCAT (position lexbuf)}
  | "~"               { BITNOT (position lexbuf) }
  | ";"               { SEMI (position lexbuf) }
  | ":"               { COLON (position lexbuf) }
  | "("               { LPAREN (position lexbuf) }
  | ")"               { RPAREN (position lexbuf) }
  | "="               { ASSIGN (position lexbuf) }
  | "{"               { LBRACE (position lexbuf) }
  | "}"               { RBRACE (position lexbuf) }
  | "["               { LBRACKET (position lexbuf) }
  | "]"               { RBRACKET (position lexbuf) }
  | ","               { COMMA (position lexbuf) }
  | "."               { DOT (position lexbuf) }
  | "|"               { PIPE (position lexbuf) }
  | "->"              { ARROW (position lexbuf) }
  | "&&&"             { PATAND (position lexbuf) }
  | "_"               { WILDCARD (position lexbuf) }
  | wspace            { token lexbuf }
  | '\n'              { incr_linenum lexbuf; token lexbuf}
  | str as s          { STRING (position lexbuf, extract_string s) }
  | _ as c            { printf "[Parse Error] Unrecognized character: %c\n" c; token lexbuf }
  | eof		            { EOF }

and comments level = parse
  | "*/"  { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "/*"  { comments (level+1) lexbuf }
  | '\n'  { incr_linenum lexbuf; if level < 0 then token lexbuf else comments level lexbuf}
  | _     { comments level lexbuf }
  | eof   { raise End_of_file }

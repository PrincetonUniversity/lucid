
{
  open Batteries
  open Parser
  open Printf
  open Span
  exception Eof

  let position lexbuf =
    {fname=(lexbuf.Lexing.lex_start_p).pos_fname; start=Lexing.lexeme_start lexbuf; finish=Lexing.lexeme_end lexbuf; spid=0}

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
}

let num = ['0'-'9']+|'0''b'['0' '1']+
let bitpat = '0''b'['0' '1' '*']+
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let wspace = [' ' '\t']
(* let str = '"'['a'-'z' 'A'-'Z' '_' '0'-'9' '~' '!' '@' '#' '$' '%' '^' '&' '|' ':' '?' '>' '<' '[' ']' '=' '-' '.' ' ' '\\' ',']*'"' *)
let str = '"'[^'"']*'"'
let filename = "\""(['a'-'z' 'A'-'Z' '0'-'9' '_' '\\' '/' '.' '-'])+"\""

rule token = parse
  | "include" wspace* filename { token lexbuf } (* Include directives are processed separately *)
  | "/*"              { comments 0 lexbuf }
  | "//"              { comments (-1) lexbuf }
  | "false"           { FALSE (position lexbuf) }
  | "true"            { TRUE (position lexbuf) }
  | "if"              { IF (position lexbuf) }
  | "else"            { ELSE (position lexbuf) }
  | "int"             { TINT (position lexbuf) }
  | "bool"            { TBOOL (position lexbuf) }
  | "event"           { EVENT (position lexbuf) }
  | "mevent"          { MEVENT (position lexbuf) }
  | "generate"        { GENERATE (position lexbuf) }
  | "mgenerate"       { MGENERATE (position lexbuf) }
  | "printf"          { PRINTF (position lexbuf) }
  | "handle"	        { HANDLE (position lexbuf) }
  | "fun"             { FUN (position lexbuf)}
  | "memop"           { MEMOP (position lexbuf)}
  | "return"          { RETURN (position lexbuf)}
  | "size"            { SIZE (position lexbuf) }
  | "global"          { GLOBAL (position lexbuf) }
  | "const"           { CONST (position lexbuf) }
  | "extern"          { EXTERN (position lexbuf) }
  | "void"            { VOID (position lexbuf) }
  | "hash"            { HASH (position lexbuf) }
  | "auto"            { AUTO (position lexbuf) }
  | "group"           { GROUP (position lexbuf) }
  | "control"         { CONTROL (position lexbuf) }
  | "entry"           { ENTRY (position lexbuf) }
  | "exit"            { EXIT (position lexbuf) }
  | "match"           { MATCH (position lexbuf) }
  | "with"            { WITH (position lexbuf) }
  | "type"            { TYPE (position lexbuf) }
  | "constr"          { CONSTR (position lexbuf) }
  | "module"          { MODULE (position lexbuf) }
  | "end"             { END (position lexbuf) }
  | "for"             { FOR (position lexbuf) }
  | "size_to_int"     { SIZECAST (position lexbuf) }
  | id as s           { ID (position lexbuf, Id.create s) }
  | "'"(id as s)      { QID (position lexbuf, Id.create s) }
  | num as n          { NUM (position lexbuf, Z.of_string n) }
  | bitpat as p       { BITPAT (position lexbuf, extract_bitpat p) }
  | "#"               { PROJ (position lexbuf) }
  | "+"               { PLUS (position lexbuf) }
  | "|-|"             { SATSUB (position lexbuf) }
  | "-"               { SUB  (position lexbuf)}
  | "!"               { NOT (position lexbuf) }
  | "&&"              { AND (position lexbuf) }
  | "||"              { OR (position lexbuf) }
  | "&"               { BITAND (position lexbuf) }
  | "=="              { EQ (position lexbuf) }
  | "!="              { NEQ (position lexbuf)}
  | "<<<"             { LSHIFT (position lexbuf) }
  | ">>>"             { RSHIFT (position lexbuf) }
  | "<<"              { LLEFT (position lexbuf) }
  | ">>"              { RRIGHT (position lexbuf) }
  | "<="              { LEQ (position lexbuf) }
  | ">="              { GEQ (position lexbuf) }
  | "<"               { LESS (position lexbuf) }
  | ">"               { MORE (position lexbuf)}
  | "^"               { CONCAT (position lexbuf)}
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

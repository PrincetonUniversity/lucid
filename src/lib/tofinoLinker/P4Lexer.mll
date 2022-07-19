{
  open P4Parser
  exception LexError of string
    let error s = raise (LexError s)

}

(* complex tokens defined by regexes *)
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let num = ['0'-'9']*
let wspace = [' ' '\t' '\n']

(* parse a single token *)
rule token = parse
    | "int"     { TINT }
    | "+"       { PLUS }
    | ";"       { SEMI }
    | "print"   { PRINT }
    | "="       { ASSIGN }
    | id as s   { ID (Id.create s)}
    | num as n  { NUM (int_of_string n)}
    | wspace    { token lexbuf }
    | eof       { EOF }
    | _ as c  {error @@ "unrecognized character: " ^ (String.make 1 c)}

%{
open P4Syntax

%}
%token TINT
%token PLUS
%token SEMI
%token PRINT
%token ASSIGN
%token <Id.t> ID
%token <int> NUM
%token EOF


%start prog
%type <P4Syntax.statements> prog

%right ID
%left PLUS

%%

ty:
    | TINT { TInt }

id:
    | ID {$1}

binop:
    | exp PLUS exp {EOp(OPlus, [$1; $3])}

value:
    | NUM {VInt($1)}

exp:
    | id {EVar($1)}
    | value {EVal($1)}
    | binop {$1}

statement:
    | ty id ASSIGN exp SEMI     {SCreate($2, $1, $4)}
    | id ASSIGN exp SEMI        {SAssign($1, $3)}
    | PRINT exp SEMI             {SPrint($2)}

statements:
    | statement {[$1]}
    | statement statements {[$1] @ $2}

prog:
    | statements EOF {$1}
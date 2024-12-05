%{
open Ast
%}

%token TRUE FALSE
%token LPAREN RPAREN
%token IF THEN ELSE
%token AND OR NOT ISZERO
%token SUCC PRED ZERO
%token EOF

%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc ISZERO SUCC PRED

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | ZERO { Zero }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | NOT; e = expr; { Not e }
  | ISZERO; e = expr; { IsZero e }
  | SUCC; e = expr; { Succ e }
  | PRED; e = expr; { Pred e }
  | LPAREN; e=expr; RPAREN {e}
;


%{
  open Ast
%}

%token TRUE FALSE
%token LPAREN RPAREN
%token EQ LEQ NOT AND OR
%token IF THEN ELSE DO WHILE SKIP GETS SEQ
%token PLUS MINUS TIMES
%token <string> ID CONST
%token EOF

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left TIMES

%left SEQ
%nonassoc ELSE DO

%start <cmd> prog

%%

prog:
  | e = cmd; EOF; { e }

cmd:
  | SKIP; { Skip }
  | x = ID; GETS; e = expr; { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e,c1,c2) }
  | WHILE; e = expr; DO; c = cmd; { While(e, c) }
  | LPAREN; c = cmd; RPAREN { c }

expr:
  | TRUE { True }
  | FALSE { False }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1, e2) }
  | e1 = expr; MINUS; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; PLUS; e2 = expr; { Add(e1, e2) }
  | e1 = expr; TIMES; e2 = expr; { Mul(e1, e2) }
  | NOT; e = expr; { Not e }
  | x = ID; { Var x }
  | n = CONST; { Const (int_of_string n) }
  | LPAREN; e = expr; RPAREN; { e }

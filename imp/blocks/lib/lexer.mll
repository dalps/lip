{
  open Parser
}

let white = [' ' '\t' '\n']+
let const = ['0'-'9'] | ['1'-'9']['0'-'9']*
let id = ['a'-'z']['_''a'-'z''A'-'Z''0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | ":=" { GETS }
  | "=" { EQ }
  | "<=" { LEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "skip" { SKIP }
  | "int" { INT }
  | ";" { SEQ }
  | const { CONST (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
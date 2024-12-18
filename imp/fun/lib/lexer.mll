{
  open Parser

  exception LexError of string

  let illegal c = raise (LexError (Printf.sprintf "[lexer] unrecognized character %c" c))
}

let white = [' ' '\t' '\n']+
let const = ['0'-'9'] | ['1'-'9']['0'-'9']*
let id = ['a'-'z']['_''a'-'z''A'-'Z''0'-'9']*

rule next_token =
  parse
  | white { next_token lexbuf }
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
  | "fun" { FUN }
  | "return" { RETURN }
  | ";" { SEQ }
  | const { CONST (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ as c { illegal c }
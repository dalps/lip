{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hexdigit = ['0'-'9''A'-'F''a'-'f']
let hexnum = "0"['x''X']hexdigit+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | (num | hexnum) { CONST (Lexing.lexeme lexbuf) } (* Will be converted in the parser by [int_of_string] *)
  | eof { EOF }

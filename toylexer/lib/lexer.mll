{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let digit = ['0'-'9']
let atok = ['A'-'Z'](letter | digit)
let vowel = ['a''e''i''o''u''A''E''I''O''U']
let low_vowel = vowel # ['A'-'Z']
let consonant = letter # vowel
let dtok = '-'? (digit* '.'? digit+ | digit+ '.'?digit*)
let hex_digit = ['0'-'9''a'-'f''A'-'F']
let etok = '0'['x''X'] hex_digit+

rule read_token =
  parse
  | white { read_token lexbuf }
  | ['A'-'Z'](letter | digit)* { ATOK }
  | low_vowel+ { BTOK }
  | consonant* vowel? consonant+ { CTOK }
  | dtok { DTOK }
  | etok { ETOK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }


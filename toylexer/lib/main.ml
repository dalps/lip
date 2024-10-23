open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency (n : int) (l : 'a list) : ('a * int) list =
  let rec remove_duplicates = function
    | [] -> []
    | x :: xs -> if List.mem x xs then
        remove_duplicates xs
      else x :: remove_duplicates xs
  in
  let keys = remove_duplicates l in
  let frequencies =
    List.map (fun k -> (k, List.filter ((=) k) l |> List.length)) keys
  in
  let sorted = List.sort (fun (_,n1) (_,n2) -> compare n2 n1) frequencies in
  let rec take = function
    | 0, _ -> []
    | _, [] -> []
    | n, x :: xs -> x :: take ((n - 1), xs)
  in
    take (n,sorted)


(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s : token list =
  explode s |>
  List.map (function
    | 'a' | 'A' -> A
    | 'b' | 'B' -> B
    | '=' -> X
    | c -> failwith (Printf.sprintf "invalid character: %c" c)
  )

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)

let bail : int = -1

let step (q : int) (t : token) : int =
  match (q,t) with
  | 0, A -> 0
  | 0, X -> 1
  | 0, B -> 2
  | 1, X -> 1
  | 1, B -> 2
  | 2, B -> 2
  | _ -> bail

let valid (l : token list) : bool =
  let final = List.fold_left step 0 l in
  List.mem final [0;1;2]

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l =
  let rec go (na, nb) = function
  | [] -> (na, nb)
  | A :: tokens -> go (na + 1, nb) tokens
  | B :: tokens -> go (na, nb + 1) tokens
  | _ :: tokens -> go (na, nb) tokens
  in
  let na, nb = go (0,0) l in
  match compare na nb with
  | 0 -> X
  | n when n < 0 -> B
  | _ -> A

(* val string_of_winner : token -> string *)
let string_of_winner w =
  match w with
  | A -> "The winner is team A"
  | B -> "The winner is team B"
  | X -> "It's a tie!"
  


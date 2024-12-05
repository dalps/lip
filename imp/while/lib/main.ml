open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let ( let^ ) : exprval -> (bool -> exprval) -> exprval =
 fun result next ->
  match result with
  | Nat _ -> raise @@ TypeError "Expected a bool"
  | Bool b -> next b

let ( let+ ) : exprval -> (int -> exprval) -> exprval =
 fun result next ->
  match result with
  | Bool _ -> raise @@ TypeError "Expected a nat"
  | Nat n -> next n

let rec eval_expr st = function
  | True -> Bool true
  | False -> Bool false
  | Const n -> Nat n
  | Var x -> st x
  | Not e ->
      let^ b = eval_expr st e in
      Bool (not b)
  | And (e1, e2) ->
      let^ b1 = eval_expr st e1 in
      let^ b2 = eval_expr st e2 in
      Bool (b1 && b2)
  | Or (e1, e2) ->
      let^ b1 = eval_expr st e1 in
      let^ b2 = eval_expr st e2 in
      Bool (b1 || b2)
  | Eq (e1, e2) ->
      let+ n1 = eval_expr st e1 in
      let+ n2 = eval_expr st e2 in
      Bool (Int.compare n1 n2 = 0)
  | Leq (e1, e2) ->
      let+ n1 = eval_expr st e1 in
      let+ n2 = eval_expr st e2 in
      Bool (Int.compare n1 n2 <= 0)
  | Add (e1, e2) ->
      let+ n1 = eval_expr st e1 in
      let+ n2 = eval_expr st e2 in
      Nat (n1 + n2)
  | Sub (e1, e2) ->
      let+ n1 = eval_expr st e1 in
      let+ n2 = eval_expr st e2 in
      Nat (n1 - n2)
  | Mul (e1, e2) ->
      let+ n1 = eval_expr st e1 in
      let+ n2 = eval_expr st e2 in
      Nat (n1 * n2)

let bottom : state = fun y -> raise (UnboundVar y)
let bind st x v : state = fun y -> if String.compare x y = 0 then v else st y

let rec trace1 = function
  | Cmd (c, st) -> (
      match c with
      | Skip -> St st
      | Assign (x, e) ->
          let v = eval_expr st e in
          St (bind st x v)
      | Seq (c1, c2) -> (
          let conf' = trace1 (Cmd (c1, st)) in
          match conf' with
          | St st' -> Cmd (c2, st')
          | Cmd (cmd', st') -> Cmd (Seq (cmd', c2), st'))
      | If (e, c1, c2) -> (
          match eval_expr st e with
          | Bool b -> if b then Cmd (c1, st) else Cmd (c2, st)
          | _ -> raise (TypeError "Expected a boolean value in if condition"))
      | While (e, c1) -> (
          match eval_expr st e with
          | Bool b -> if b then Cmd (Seq (c1, c), st) else St st
          | _ -> raise (TypeError "Expected a boolean value in while condition")))
  | St _ -> raise NoRuleApplies

let rec helper i conf =
  if i >= 0 then
    try
      let conf' = trace1 conf in
      conf :: helper (i - 1) conf'
    with _ -> [ conf ]
  else []

let trace (steps : int) (c : cmd) : conf list =
  let conf0 = Cmd (c, bottom) in
  helper steps conf0

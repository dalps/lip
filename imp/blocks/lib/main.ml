open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let ( let^ ) : memval -> (bool -> memval) -> memval =
 fun result next ->
  match result with
  | Int _ -> raise @@ TypeError "Expected a bool"
  | Bool b -> next b

let ( let+ ) : memval -> (int -> memval) -> memval =
 fun result next ->
  match result with
  | Bool _ -> raise @@ TypeError "Expected a nat"
  | Int n -> next n

let eval_expr (st : state) (e : expr) =
  let rec aux = function
    | True -> Bool true
    | False -> Bool false
    | Const n -> Int n
    | Var x -> ( match (topenv st) x with BVar l | IVar l -> (getmem st) l)
    | Not e ->
        let^ b = aux e in
        Bool (not b)
    | And (e1, e2) ->
        let^ b1 = aux e1 in
        let^ b2 = aux e2 in
        Bool (b1 && b2)
    | Or (e1, e2) ->
        let^ b1 = aux e1 in
        let^ b2 = aux e2 in
        Bool (b1 || b2)
    | Eq (e1, e2) ->
        let+ n1 = aux e1 in
        let+ n2 = aux e2 in
        Bool (Int.compare n1 n2 = 0)
    | Leq (e1, e2) ->
        let+ n1 = aux e1 in
        let+ n2 = aux e2 in
        Bool (Int.compare n1 n2 <= 0)
    | Add (e1, e2) ->
        let+ n1 = aux e1 in
        let+ n2 = aux e2 in
        Int (n1 + n2)
    | Sub (e1, e2) ->
        let+ n1 = aux e1 in
        let+ n2 = aux e2 in
        Int (n1 - n2)
    | Mul (e1, e2) ->
        let+ n1 = aux e1 in
        let+ n2 = aux e2 in
        Int (n1 * n2)
  in
  aux e

let exec_decl (st : state) (ds : decl list) : state =
  let loc', env' =
    List.fold_left
      (fun (l, env) d ->
        ( l + 1,
          match d with
          | IntVar x -> bind_env env x (IVar l)
          | BoolVar x -> bind_env env x (BVar l) ))
      (getloc st, topenv st)
      ds
  in
  make_state (env' :: getenv st) (getmem st) loc'

let rec trace1 = function
  | Cmd (c, st) -> (
      match c with
      | Skip -> St st
      | Assign (x, e) ->
          let v = eval_expr st e in
          St (bindvar st x v)
      | Seq (c1, c2) -> (
          match trace1 @@ Cmd (c1, st) with
          | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
          | St st' -> Cmd (c2, st'))
      | If (e, c1, c2) -> (
          match eval_expr st e with
          | Bool b -> if b then Cmd (c1, st) else Cmd (c2, st)
          | Int _ -> raise @@ TypeError "while guard expected a bool")
      | While (e, cbody) -> (
          match eval_expr st e with
          | Bool b -> if b then Cmd (Seq (cbody, c), st) else St st
          | Int _ -> raise @@ TypeError "while guard expected a bool")
      | Decl (ds, c) -> Cmd (Block c, exec_decl st ds)
      | Block c -> (
          match trace1 @@ Cmd (c, st) with
          (* exit the block and restore the outer scope *)
          | St st -> St (setenv st (popenv st)) (* stay in the block *)
          | Cmd (c', st') -> Cmd (Block c', st')))
  | St _ -> raise NoRuleApplies

let trace (n_steps : int) (c : cmd) =
  let conf0 = Cmd (c, state0) in
  let rec go (i : int) (conf : conf) =
    if i >= 0 then
      try conf :: go (i - 1) (trace1 conf) with NoRuleApplies -> [ conf ]
    else []
  in
  go n_steps conf0

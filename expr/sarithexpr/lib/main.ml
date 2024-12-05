open Ast

type exprtype = BoolT | NatT
type exprval = Bool of bool | Nat of int

exception TypeError of string

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec string_of_expr : expr -> string = function
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | If (e0, e1, e2) ->
      Printf.sprintf "If(%s,%s,%s)" (string_of_expr e0) (string_of_expr e1)
        (string_of_expr e2)
  | And (e0, e1) ->
      Printf.sprintf "And(%s,%s)" (string_of_expr e0) (string_of_expr e1)
  | Or (e0, e1) ->
      Printf.sprintf "Or(%s,%s)" (string_of_expr e0) (string_of_expr e1)
  | Not e0 -> Printf.sprintf "Not %s" (string_of_expr e0)
  | Succ e0 -> Printf.sprintf "Succ %s" (string_of_expr e0)
  | Pred e0 -> Printf.sprintf "Pred %s" (string_of_expr e0)
  | IsZero e0 -> Printf.sprintf "IsZero %s" (string_of_expr e0)

let string_of_type : exprtype -> string = function
  | BoolT -> "BoolT"
  | NatT -> "NatT"

let string_of_val : exprval -> string = function
  | Bool true -> "true"
  | Bool false -> "false"
  | Nat n -> string_of_int n

let expect (e : expr) (actual : exprtype) (expected : exprtype)
    (next : exprtype -> 'a) : 'a =
  if actual = expected then next actual
  else
    raise
    @@ TypeError
         (Printf.sprintf "%s has type %s, but type %s was expected"
            (string_of_expr e) (string_of_type actual) (string_of_type expected))

let expect_nat e a ty = expect e a NatT (fun _ -> ty)
let expect_bool e a ty = expect e a BoolT (fun _ -> ty)

let rec typecheck : expr -> exprtype = function
  | True | False -> BoolT
  | Zero -> NatT
  | Succ e | Pred e -> expect_nat e (typecheck e) NatT
  | IsZero e -> expect_nat e (typecheck e) BoolT
  | Not e -> expect_bool e (typecheck e) BoolT
  | And (e1, e2) | Or (e1, e2) ->
      expect_bool e2 (typecheck e2) @@ expect_bool e1 (typecheck e1) BoolT
  | If (e0, e1, e2) ->
      expect_bool e0 (typecheck e0)
      @@ expect e2 (typecheck e1) (typecheck e2) (fun ty -> ty)

let rec eval : expr -> exprval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Succ e -> (
      match eval e with
      | Bool _ -> failwith "succ expects a nat"
      | Nat n -> Nat (n + 1))
  | Pred e -> (
      match eval e with
      | Bool _ -> failwith "pred exprects a nat"
      | Nat n ->
          if n > 0 then Nat (n - 1) else failwith "pred 0 is not a legal number"
      )
  | IsZero e -> (
      match eval e with
      | Bool _ -> failwith "iszero expects a nat"
      | Nat n -> Bool (n = 0))
  | And (e1, e2) -> (
      match (eval e1, eval e2) with
      | Nat _, _ -> failwith "and first expression is not a bool"
      | _, Nat _ -> failwith "and second expression is not a bool"
      | Bool b1, Bool b2 -> Bool (b1 && b2))
  | Or (e1, e2) -> (
      match (eval e1, eval e2) with
      | Nat _, _ -> failwith "or first expression is not a bool"
      | _, Nat _ -> failwith "or second expression is not a bool"
      | Bool b1, Bool b2 -> Bool (b1 || b2))
  | Not e -> (
      match eval e with
      | Nat _ -> failwith "not expects a bool"
      | Bool b -> Bool (not b))
  | If (e0, e1, e2) -> (
      match eval e0 with
      | Nat _ -> failwith "if expects a bool"
      | Bool b -> if b then eval e1 else eval e2)

let rec is_nv : expr -> bool = function
  | Zero -> true
  | Succ e -> is_nv e
  | _ -> false

exception NoRuleApplies

let rec trace1 : expr -> expr = function
  | If (True, e1, _) -> e1
  | If (False, _, e2) -> e2
  | If (e0, e1, e2) -> If (trace1 e0, e1, e2)
  | Not True -> False
  | Not False -> True
  | Not e -> Not (trace1 e)
  | And (True, e2) -> e2
  | And (False, _) -> False
  | And (e1, e2) -> And (trace1 e1, e2)
  | Or (True, _) -> True
  | Or (False, e2) -> e2
  | Or (e1, e2) -> Or (trace1 e1, e2)
  | Succ e -> Succ (trace1 e)
  | IsZero Zero -> True
  | IsZero (Succ e) when is_nv e -> False
  | IsZero e -> IsZero (trace1 e)
  | Pred (Succ e) when is_nv e -> e
  | Pred e -> Pred (trace1 e)
  | _ -> raise NoRuleApplies

let rec trace (e : expr) : expr list =
  try
    let e' = trace1 e in
    e :: trace e'
  with NoRuleApplies -> [ e ]

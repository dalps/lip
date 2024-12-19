open Ast
open Types
open Prettyprint

(*
  opam install nice_parser
  https://github.com/smolkaj/nice-parser
*)
module P = Nice_parser.Make (struct
  type result = Ast.prog
  type token = Parser.token
  exception ParseError = Parser.Error
  let parse = Parser.prog
  include Lexer
end)

(* Turn on nice parser errors *)
let _ = P.pp_exceptions ()

let parse = P.parse_string

let exec_decl (st : state) (ds : decl list) : state =
  let loc', env' =
    List.fold_left
      (fun (l, env) d ->
        ( l + 1,
          match d with
          | IntVar x -> bind_env env x (IVar l)
          | Fun (f, par, body, e) -> bind_env env f (IFun (par, body, e)) ))
      (getloc st, topenv st)
      ds
  in
  make_state (env' :: getenv st) (getmem st) loc'

module WithState = struct
  type 'a t = state -> state * 'a

  let return (e : 'a) : 'a t = fun st -> (st, e)

  let set (st : state) : unit t = fun _ -> (st, ())

  let get : state t = fun st -> (st, st)

  let bind (e : 'a t) (next : 'a -> 'b t) : 'b t =
   fun st ->
    let st', a = e st in
    next a st'

  let ( let$ ) = bind
end

open WithState

let rec trace1_expr (e : expr) : expr WithState.t =
  print_endline (string_of_expr e);
  match e with
  | True -> return True
  | False -> return False
  | Const n -> return (Const n)
  | Var x ->
      let$ st = get in
      return (Const (apply st x))
  | Not True -> return False
  | Not False -> return True
  | Not e ->
      let$ e2 = trace1_expr e in
      return (Not e2)
  | And (False, _) -> return False
  | And (True, e2) -> trace1_expr e2
  | And (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (And (e1', e2))
  | Or (True, _) -> return True
  | Or (False, e2) -> trace1_expr e2
  | Or (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Or (e1', e2))
  | Add (Const n1, Const n2) -> return (Const (n1 + n2))
  | Add ((Const _ as e1), e2) ->
      let$ e2' = trace1_expr e2 in
      return (Add (e1, e2'))
  | Add (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Add (e1', e2))
  | Sub (Const n1, Const n2) -> return (Const (n1 - n2))
  | Sub ((Const _ as e1), e2) ->
      let$ e2' = trace1_expr e2 in
      return (Sub (e1, e2'))
  | Sub (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Sub (e1', e2))
  | Mul (Const n1, Const n2) -> return (Const (n1 * n2))
  | Mul ((Const _ as e1), e2) ->
      let$ e2' = trace1_expr e2 in
      return (Mul (e1, e2'))
  | Mul (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Mul (e1', e2))
  | Eq (Const n1, Const n2) -> return (if Int.equal n1 n2 then True else False)
  | Eq ((Const _ as e1), e2) ->
      let$ e2' = trace1_expr e2 in
      return (Eq (e1, e2'))
  | Eq (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Eq (e1', e2))
  | Leq (Const n1, Const n2) ->
      return (if Int.compare n1 n2 <= 0 then True else False)
  | Leq ((Const _ as e1), e2) ->
      let$ e2' = trace1_expr e2 in
      return (Leq (e1, e2'))
  | Leq (e1, e2) ->
      let$ e1' = trace1_expr e1 in
      return (Leq (e1', e2))
  | Call (f, Const n) ->
      let$ st = get in
      let par, body, ret = apply_fun st f in
      let loc = getloc st in
      let env' = bind_env (topenv st) par (IVar loc) in
      let mem' = bind_mem (getmem st) loc n in
      let$ _ = set (make_state (pushenv st env') mem' (loc + 1)) in
      return (CallExec (body, ret))
  | Call (f, e) ->
      let$ e' = trace1_expr e in
      return (Call (f, e'))
  | CallExec (c, e) -> (
      let$ st = get in
      match trace1 (Cmd (c, st)) with
      | St st' ->
          let$ _ = set st' in
          return (CallRet e)
      | Cmd (c', st') ->
          let$ _ = set st' in
          return (CallExec (c', e)))
  | CallRet (Const n) ->
      let$ st = get in
      let$ _ = set (setenv st (popenv st)) in
      return (Const n)
  | CallRet e ->
      let$ e' = trace1_expr e in
      return (CallRet e')

and trace1 : conf -> conf = function
  | Cmd (c, st) -> (
      match c with
      | Skip -> St st
      | Assign (x, e) -> (
          match trace1_expr e st with
          | st', Const n ->
              let st'' = bind_ivar st' x n in
              St st''
          | st', e' -> Cmd (Assign (x, e'), st'))
      | Seq (c1, c2) -> (
          match trace1 @@ Cmd (c1, st) with
          | St st' -> Cmd (c2, st')
          | Cmd (c1', st') -> Cmd (Seq (c1', c2), st'))
      | If (e, c1, c2) -> (
          match trace1_expr e st with
          | st', True -> Cmd (c1, st')
          | st', False -> Cmd (c2, st')
          | st', e' -> Cmd (If (e', c1, c2), st'))
      | While (e, cbody) ->
          Cmd (If (e, Seq (cbody, c), Skip), st)
          (* (match trace1_expr e st with
             | st', True -> Cmd (Seq (cbody, c), st')
             | st', False -> St st'
             | st', e' -> Cmd (While (e', cbody), st'))

             ^^^ This code doesn't work because when it produces the sequence,
             the condition expression has been fully reduced!

             The simplest way to "remember" the original guard expression is
             by the If trick used above.
          *)
        )
  | St _ -> raise NoRuleApplies

let trace (n_steps : int) (Prog (ds, c) : prog) =
  let state0 = exec_decl state0 ds in
  let conf0 = Cmd (c, state0) in
  let rec go (i : int) (conf : conf) =
    if i >= 0 then (
      try conf :: go (i - 1) (trace1 conf)
      with NoRuleApplies ->
        [ conf ])
    else []
  in
  go n_steps conf0

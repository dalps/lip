open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false
let%test "test_eval_2" = test_eval "true" true
let%test "test_eval_3" = test_eval "if true then false else true" false
let%test "test_eval_4" = test_eval "if false then false else true" true
let%test "test_eval_5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval_6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval_7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)

let%test "test_trace1_if_makes_progress_1" = 
  If (True, False, True) |> trace1 = False

let%test "test_trace1_if_makes_progress_2" = 
  If (False, False, True) |> trace1 = True

let%test "test_trace1_if_makes_progress_3" = 
  If (If (True, False, True), False, True) |> trace1 = If (False, False, True)

let ( ==> ) a b = not a || b

let%test "test_normal_form_is_value_True" =
  try trace1 True = False with NoRuleApplies -> true
  ==> is_value True

let%test "test_normal_form_is_value_False" =
  try trace1 False = False with NoRuleApplies -> true
  ==> is_value True

let%test "test_reduce_steps" =
  "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> parse |> trace |> List.length <= 10

(* ### Unit tests for task 6 *)

let%test "test_and" =
  List.for_all (fun b -> b = true) [
    test_eval "true && true" true;
    test_eval "true && false" false;
    test_eval "false && true" false;
    test_eval "false && false" false;
    test_eval "if true then true else true && false" true;
    test_eval "(if true then true else true) && false" false;
  ]

let%test "test_and" =
  List.for_all (fun b -> b = true) [
    test_eval "true || true" true;
    test_eval "true || false" true;
    test_eval "false || true" true;
    test_eval "false || false" false;
    test_eval "if true then false else true || true" false;
    test_eval "(if true then false else true) || true" true;
  ]
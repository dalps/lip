open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)

let%test "test_eval_unary_minus" = parse "-1 - 2 - -3" |> eval = Ok 0

let%test "test_eval_div_ok" = parse "(0xff + 1) / -0x2" |> eval = Ok (-128)

let%test "test_eval_div_error" = parse "(0xff + 1) / 0" |> eval |> function 
  | Ok _ -> false
  | Error _ -> true

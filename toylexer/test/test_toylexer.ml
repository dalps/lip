open Toylexer.Token
open Toylexer.Main

let p1 : string = "(foo+1=foo; ; foo; 1+foo=1+foo)(foo)"

let p2 : string = "(((())"

let%test "test_frequencies_1" =
  lexer p1 |> frequency 3 = [(ID "foo", 6); (SEQ, 3); (DTOK, 3)]

let%test "test_frequencies_2" =
  lexer p2 |> frequency 2 = [(LPAREN, 4); (RPAREN, 2)]

let tok_tests =[
  "Foo",        ATOK;
  "aeiou",      BTOK;
  "aeiDDD",     ID "aeiDDD";
  "aba",        ID "aba";
  "bab",        CTOK;
  "crabppl",    CTOK;
  "-3.14",      DTOK;
  "-7.",        DTOK;
  "-.3",        DTOK;
  "1234",       DTOK;
  "0xbeefb055", ETOK;
  "0xff",       ETOK;

]

let%test "test_toks" =
  List.for_all (fun (input, expected) -> 
    let result = lexer input |> List.hd in
    let test = result = expected in
    (match test with
    | true -> print_endline @@ "ok " ^ input;
    | false -> print_endline @@ "bad token for " ^ input ^ " " ^ string_of_token result);
    test
      
  ) tok_tests
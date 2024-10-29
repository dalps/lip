open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = {
  symbols = [ S; A ];
  terminals = [ '0'; '1' ];
  productions = [
    S --> "";
    S --> "A";
    A --> "01";
    A --> "0A1";
  ];
  start = S;
}


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = {
  symbols = [ S ];
  terminals = [ '0'; '1' ];
  productions = [
    S --> "0S0";
    S --> "1S1";
    S --> "00";
    S --> "11";
    S --> "1";
    S --> "0";
  ];
  start = S;
}


(* #### Exercise 3, medium (balanced_parentheses)*)
let balanced_parentheses : grammar =  {
  symbols = [ S ];
  terminals = [ '('; ')'; '['; ']'; '{'; '}' ];
  productions = [
    (* 0 *) S --> "SS";
    (* 1 *) S --> "(S)";
    (* 2 *) S --> "[S]";
    (* 3 *) S --> "{S}";
    (* 4 *) S --> "()";
    (* 5 *) S --> "[]";
    (* 6 *) S --> "{}";
  ];
  start = S;
}


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar = {
  terminals = [ '0'; '1' ];
  symbols = [ S; A; B ];
  productions = [
  (* A: the number of 0 is one plus the number of 1 *)
  (* B: the number of 1 is one plus the number of 0 *)
  (* 0 *) S --> "0B";
  (* 1 *) S --> "1A";
  (* 2 *) A --> "1AA";
  (* 3 *) A --> "0";
  (* 4 *) A --> "0S";
  (* 5 *) B --> "0BB";
  (* 6 *) B --> "1";
  (* 7 *) B --> "1S";
  (* 8 *) S --> "";
  ];
  start = S;
}

let debug = true

type state = int
type word = char list
type trans = state -> char -> state

type dfa = { qi : state; delta : trans; qf : state list }
(**
  The type of deterministic finite-state automaton.
  - [qi] is the initial state
  - [delta] is the transition function
  - [qf] is the set of final (accepting) states
*)

type recognizer = dfa -> word -> bool

(** A sink state. If reached, the word is definitely not accepted. *)
let bail : state = -1

(** Converts a [char list] to a [string] *)
let chl_of_string l = l |> String.to_seq |> List.of_seq

(** [steps t q w] is the final state reached by reading the word [w]
    starting from state [q] according to the transition function [t].

    Set [debug] to true or [#trace steps] in utop to print
    intermediate states.
*)
let rec steps (t : trans) (q : state) (w : word) : state =
  match w with
  | [] -> q
  | c :: w' ->
      let q' = t q c in
      if debug then Printf.printf "(%3d,'%c') -> %3d\n" q c q';
      steps t q' w'

(** [recognize a w] feeds the word [w] to the DFA [a] and
    returns [true] if it lands on a final state of [a].
*)
let recognize : recognizer =
 fun a w ->
  let res = steps a.delta a.qi w in
  (* or [List.fold_left a.delta a.qi w]*)
  List.mem res a.qf

(* #### Language-specific definitions #### *)

let lang1_trans : trans =
 fun q c ->
  match (q, c) with
  | 0, '0' -> 1
  | 0, '1' -> 1
  | 1, '0' -> 1
  | 1, '1' -> 1
  | _ -> bail

let lang2_trans : trans =
 fun q c ->
  match (q, c) with
  | 0, '0' -> 1
  | 0, '1' -> 1
  | 1, '0' -> bail
  | 1, '1' -> 1
  | _ -> bail

let lang3_trans : trans =
 fun q c ->
  match (q, c) with
  | 0, '0' -> 1
  | 0, _ -> bail
  | 1, '0' -> 2
  | 1, '1' -> 1
  | 2, '0' -> 2
  | 2, '1' -> 1
  | _ -> bail

let lang4_trans : trans =
 fun q c ->
  match (q, c) with
  | 0, '0' -> 0
  | 0, '1' -> 1
  | 1, '0' -> 1
  | 1, '1' -> 2
  | 2, '0' -> 2
  | _ -> bail

let lang5_trans : trans =
 fun q c ->
  match (q, c) with
  | 0, '0' -> 1
  | 1, '0' -> 2
  | 1, _ -> bail
  | 0, '1' -> 3
  | 3, '1' -> 2
  | 3, _ -> bail
  | 2, '0' -> 1
  | 2, '1' -> 3
  | _ -> bail

let lang1_r : dfa = { qi = 0; delta = lang1_trans; qf = [ 1 ] }
let lang2_r : dfa = { qi = 0; delta = lang2_trans; qf = [ 0; 1 ] }
let lang3_r : dfa = { qi = 0; delta = lang3_trans; qf = [ 2 ] }
let lang4_r : dfa = { qi = 0; delta = lang4_trans; qf = [ 2 ] }
let lang5_r : dfa = { qi = 0; delta = lang5_trans; qf = [ 2 ] }

let lang1 = recognize lang1_r
let lang2 = recognize lang2_r
let lang3 = recognize lang3_r
let lang4 = recognize lang4_r
let lang5 = recognize lang5_r

let recognizers = [ lang1; lang2; lang3; lang4; lang5 ]

let belongsTo w = List.map (fun f -> f w) recognizers

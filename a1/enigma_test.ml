open OUnit2
(* If you get an "unbound module" error from the line below,
   it's most likely because you have not (re)compiled [enigma.ml]. 
   To do that, run [make build]. *)
open Enigma

(** [make_index_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [index input]. *)
let make_index_test 
    (name : string) 
    (input: char) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (index input) ~printer:string_of_int)
(* TODO: you will find it helpful to write functions like [make_index_test]
   for each of the other functions you are testing.  They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. *)

let index_tests = [
  make_index_test "index of A is 0" 'A' 0;
  (* TODO: add your tests here *)
  make_index_test "index of B is 1" 'B' 1;
  make_index_test "index of C is 2" 'C' 2;
  make_index_test "index of D is 3" 'D' 3;
  make_index_test "index of E is 4" 'E' 4;
  make_index_test "index of F is 5" 'F' 5;
  make_index_test "index of G is 6" 'G' 6;
  make_index_test "index of H is 7" 'H' 7;
  make_index_test "index of I is 8" 'I' 8;
  make_index_test "index of J is 9" 'J' 9;
  make_index_test "index of K is 10" 'K' 10;
  make_index_test "index of L is 11" 'L' 11;
  make_index_test "index of M is 12" 'M' 12;
  make_index_test "index of N is 13" 'N' 13;
  make_index_test "index of O is 14" 'O' 14;
  make_index_test "index of P is 15" 'P' 15;
  make_index_test "index of Q is 16" 'Q' 16;
  make_index_test "index of R is 17" 'R' 17;
  make_index_test "index of S is 18" 'S' 18;
  make_index_test "index of T is 19" 'T' 19;
  make_index_test "index of U is 20" 'U' 20;
  make_index_test "index of V is 21" 'V' 21;
  make_index_test "index of W is 22" 'W' 22;
  make_index_test "index of X is 23" 'X' 23;
  make_index_test "index of Y is 24" 'Y' 24;
  make_index_test "index of Z is 25" 'Z' 25;


  
]

(** [make_map_rl_test name wires top_let pos expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [map_r_to_l wires top_let pos. *)
let make_map_rl_test
    (name : string)
    (wires : string)
    (top_let : char)
    (pos : int)
    (expected_output : int) : test =
  name >:: (fun _ ->
    (* the [printer] tells OUnit how to convert the output to a string *)
    assert_equal expected_output (map_r_to_l wires top_let pos) ~printer:string_of_int)

let map_rl_tests = [
  (* TODO: add your tests here *)
  make_map_rl_test "The wiring specification ABCDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter ‘A’ should cause current to flow from position 0 to position 0."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
  make_map_rl_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘A’ should cause current to flow from position 0 to position 4."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 4;
  make_map_rl_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘B’ should cause current to flow from position 0 to position 19."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9;
  make_map_rl_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘B’ should cause current to flow from position 1 to position 11."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 1 11;
  make_map_rl_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘K’ should cause current to flow from position 16 to position 20."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'K' 16 20;
  make_map_rl_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'A' should cause current to flow from position 5 to position 5."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 5 5;
  (*make_map_rl_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'B' should cause current to flow from position 13 to position ?."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 0 0;*)
  (*make_map_rl_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'C' should cause current to flow from position 25 to position ?."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 0 0;*)
]

let make_map_lr_test
    (name : string)
    (wires : string)
    (top_let : char)
    (pos : int)
    (expected_output : int) : test =
  name >:: (fun _ ->
    (*the [printer] tells OUnit how to convert the output to a string *)
    assert_equal expected_output (map_l_to_r wires top_let pos) ~printer:string_of_int)

let map_lr_tests = [
  (* TODO: add your tests here *)
  make_map_lr_test "The wiring specification ABCDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter ‘A’ should cause current to flow from position 0 to position 0."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
  make_map_lr_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘A’ should cause current to flow from position 0 to position 20."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 20;
  make_map_lr_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘B’ should cause current to flow from position 0 to position 20."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 21;
]

let map_refl_tests = [
  (* TODO: add your tests here *)
]

let map_plug_tests = [
  (* TODO: add your tests here *)
]

let cipher_char_tests = [
  (* TODO: add your tests here *)
]

let step_tests = [
  (* TODO: add your tests here *)
]

let cipher_tests = [
  (* TODO: add your tests here *)
]

let tests =
  "test suite for A1"  >::: List.flatten [
    index_tests;
    map_rl_tests;
    map_lr_tests;
    map_refl_tests;
    map_plug_tests;
    cipher_char_tests;
    step_tests;
    cipher_tests;
  ]

let _ = run_test_tt_main tests

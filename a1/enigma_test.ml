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
  make_map_rl_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'B' should cause current to flow from position 13 to position 13."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 13 13;
  make_map_rl_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'C' should cause current to flow from position 25 to position 24"
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 25 24;
  make_map_rl_test "The wiring specification BDFHJLCPRTXVZNYEIWGAKMUSQO with
  top letter 'O' should cause current to flow from position 14 to position 17."
  "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O' 14 17;
]

(** [make_map_lr_test name wires top_let pos expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [map_l_to_r wires top_let pos. *)
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
  make_map_lr_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter ‘D’ should cause current to flow from position 3 to position 3."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'D' 3 2;
  make_map_lr_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'A' should cause current to flow from position 2 to position 2."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 2 2;
  make_map_lr_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'B' should cause current to flow from position 14 to position 14."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 14 14;
  make_map_lr_test "The wiring specification BACDEFGHIJKLMNOPQRSTUVWXYZ with
  top letter 'C' should cause current to flow from position 23 to position 23."
  "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 23 23;
  make_map_lr_test "The wiring specification EKMFLGDQVZNTOWYHXUSPAIBRCJ with
  top letter 'F' should cause current to flow from position 10 to position 14."
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;
]

(** [make_reflector_test name wires pos expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [map_refl wires pos. *)
let make_refl_test
    (name : string)
    (wires : string)
    (pos : int)
    (expected_output : int) : test = 
  name >:: (fun _ ->
    (*the [printer] tells OUnit how to convert the output to a string *)
    assert_equal expected_output (map_refl wires pos) ~printer:string_of_int)

let map_refl_tests = [
  (* TODO: add your tests here *)
  make_refl_test "Reflection of ABCDEFGHIJKLMNOPQRSTUVWXYZ pos = 0 is 0."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 0;
  make_refl_test "Reflection of YRUHQSLDPXNGOKMIEBFZCWVJAT pos = 0 is 24."
  "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0 24;
  make_refl_test "Reflection of YRUHQSLDPXNGOKMIEBFZCWVJAT pos = 24 is 0."
  "YRUHQSLDPXNGOKMIEBFZCWVJAT" 24 0;
  make_refl_test "Reflection of FVPJIAOYEDRZXWGCTKUQSBNMHL pos = 0 is 5."
  "FVPJIAOYEDRZXWGCTKUQSBNMHL" 0 5;
  make_refl_test "Reflection of FVPJIAOYEDRZXWGCTKUQSBNMHL pos = 5 is 0."
  "FVPJIAOYEDRZXWGCTKUQSBNMHL" 5 0;
  make_refl_test "Reflection of EJMZALYXVBWFCRQUONTSPIKHGD pos = 1 is 9."
  "EJMZALYXVBWFCRQUONTSPIKHGD" 1 9;
  make_refl_test "Reflection of EJMZALYXVBWFCRQUONTSPIKHGD pos = 9 is 1."
  "EJMZALYXVBWFCRQUONTSPIKHGD" 9 1;
  make_refl_test "Reflection of YRUHQSLDPXNGOKMIEBFZCWVJAT pos = 6 is 11."
  "YRUHQSLDPXNGOKMIEBFZCWVJAT" 6 11;
  make_refl_test "Reflection of YRUHQSLDPXNGOKMIEBFZCWVJAT pos = 11 is 6."
  "YRUHQSLDPXNGOKMIEBFZCWVJAT" 11 6;
  make_refl_test "Reflection of FVPJIAOYEDRZXWGCTKUQSBNMHL pos = 11 is 25."
  "FVPJIAOYEDRZXWGCTKUQSBNMHL" 11 25;
  make_refl_test "Reflection of FVPJIAOYEDRZXWGCTKUQSBNMHL pos = 11 is 25."
  "FVPJIAOYEDRZXWGCTKUQSBNMHL" 25 11;
  make_refl_test "Reflection of ENKQAUYWJICOPBLMDXZVFTHRGS pos = 20 is 5."
  "ENKQAUYWJICOPBLMDXZVFTHRGS" 20 5;
  make_refl_test "Reflection of ENKQAUYWJICOPBLMDXZVFTHRGS pos = 5 is 20."
  "ENKQAUYWJICOPBLMDXZVFTHRGS" 5 20;
  make_refl_test "Reflection of RDOBJNTKVEHMLFCWZAXGYIPSUQ pos = 21 is 8."
  "RDOBJNTKVEHMLFCWZAXGYIPSUQ" 21 8;
  make_refl_test "Reflection of RDOBJNTKVEHMLFCWZAXGYIPSUQ pos = 8 is 21."
  "RDOBJNTKVEHMLFCWZAXGYIPSUQ" 8 21;
]

(** [make_plug_test name plugs input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [map_plug plugs input. *)
let make_plug_test
    (name : string)
    (plugs: (char*char) list)
    (input : char)
    (expected_output : char) : test = 
  name >:: (fun _ ->
    assert_equal expected_output (map_plug plugs input))

let map_plug_tests = [
  (* TODO: add your tests here *)
  make_plug_test "Empty list & input 'A' should return 'A'." [] 'A' 'A';
  make_plug_test "Empty list & input 'Z' should return 'Z'." [] 'Z' 'Z';
  make_plug_test "[('A','B')] & input 'A' should return 'B'." 
  [('A','B')] 'A' 'B';
  make_plug_test "[('B','A')] & input 'A' should return 'B'." 
  [('B','A')] 'A' 'B';
  make_plug_test "[('A','B')] & input 'B' should return 'A'." 
  [('A','B')] 'B' 'A';
  make_plug_test "[('B','A')] & input 'B' should return 'A'." 
  [('B','A')] 'B' 'A';
  make_plug_test "[('A','Z'); ('X','Y')] & input 'A' should return 'Z'."
  [('A','Z'); ('X','Y')] 'A' 'Z';
  make_plug_test "[('A','Z'); ('X','Y')] & input 'Z' should return 'A'."
  [('A','Z'); ('X','Y')] 'Z' 'A';
  make_plug_test "[('A','Z'); ('X','Y')] & input 'X' should return 'Y'."
  [('A','Z'); ('X','Y')] 'X' 'Y';
  make_plug_test "[('A','Z'); ('X','Y')] & input 'Y' should return 'X'."
  [('A','Z'); ('X','Y')] 'Y' 'X';
  make_plug_test "[('Y','X'); ('Z','A')] & input 'A' should return 'Z'."
  [('Y','X'); ('Z','A')] 'A' 'Z';
  make_plug_test "[('Y','X'); ('Z','A')] & input 'Z' should return 'A'."
  [('Y','X'); ('Z','A')] 'Z' 'A';
  make_plug_test "[('Y','X'); ('Z','A')] & input 'X' should return 'Y'."
  [('Y','X'); ('Z','A')] 'X' 'Y';
  make_plug_test "[('Y','X'); ('Z','A')] & input 'Y' should return 'X'."
  [('Y','X'); ('Z','A')] 'Y' 'X';
  make_plug_test "Every sequential pair of letters in the alphabet has a plug
  & input 'Q' should return 'R'." [('A','B'); ('C','D'); ('E','F'); ('G','H');
  ('I','J'); ('K','L'); ('M','N'); ('O','P'); ('Q','R'); ('S','T'); ('U','V');
  ('W','X'); ('Y','Z')] 'Q' 'R';
  make_plug_test "Every sequential pair of letters in the alphabet has a plug
  & input 'R' should return 'Q'." [('A','B'); ('C','D'); ('E','F'); ('G','H');
  ('I','J'); ('K','L'); ('M','N'); ('O','P'); ('Q','R'); ('S','T'); ('U','V');
  ('W','X'); ('Y','Z')] 'R' 'Q';
]

(** [make_cipher_char_test name config input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [cipher_char config input. *)
let make_cipher_char_test
    (name : string)
    (config : config)
    (input : char)
    (expected_output : char) : test =
  name >:: (fun _ ->
      assert_equal expected_output (cipher_char config input))

(* Rotors, Oriented Rotors, and Configs for use in
cipher_car_tests. *)

let config1 = {
  refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  rotors = [];
  plugboard = [];
}

let rotor1 = {
  wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
  turnover = 'A';
}

let rotor2 = {
  wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE";
  turnover = 'A';
}

let rotor3 = {
  wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO";
  turnover = 'A';
}

let oriented_rotor1 = {
  rotor = rotor1;
  top_letter = 'A';
}

let oriented_rotor2 = {
  rotor = rotor2;
  top_letter = 'A';
}

let oriented_rotor3 = {
  rotor = rotor3;
  top_letter = 'A';
}

let config2 = {
  refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
  rotors = [oriented_rotor1; oriented_rotor2; oriented_rotor3];
  plugboard = [];
}

let cipher_char_tests = [
  (* TODO: add your tests here *)
  make_cipher_char_test "Empty plugboard & no rotors should behave like 
  identify function 'A' -> 'A'" config1 'A' 'A';
  make_cipher_char_test "Empty plugboard & no rotors should behave like 
  identify function 'Z' -> 'Z'" config1 'Z' 'Z';
  make_cipher_char_test "Reflector B; Rotors I(A), II(A), III(A);
  Empty Plugboard should translate 'G' -> 'P'." config2 'G' 'P';
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

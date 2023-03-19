open OUnit2
open Chess
open Command
open State

(** tests *)
let state_tests = []


let parse_test (name : string) (input : string) 
  (expected_output : string) = 
  name >:: fun _ ->
  assert_equal expected_output (get_command (parse input)) ~printer:Fun.id

let parse_test_invalid (name : string) (input : string) = 
  name >:: fun _ ->
  assert_raises MalformedInput (fun() -> parse input)


let command_tests = [
  parse_test "basic input" "a3 a4" "a3 a4";
  parse_test "uppercase both" "B3 C6" "b3 c6"; 
  parse_test "uppercase one" "g8 F6" "g8 f6"; 
  parse_test "extreme bounds" "a1 h8" "a1 h8"; 
  parse_test_invalid "out of bounds invalid" "a0 c1"; 
  parse_test_invalid "bad spacing" "a3   a4"; 
  parse_test_invalid "no spaces" "a3a4"; 
  parse_test_invalid "same square" "c1 c1";
  parse_test_invalid "random" "asdflk214p9u124 1249u09v";
]
let piece_tests = []
let gui_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ state_tests; command_tests; piece_tests; gui_tests ]

let _ = run_test_tt_main suite

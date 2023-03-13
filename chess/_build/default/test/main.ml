open OUnit2
open Chess

(** tests *)
let state_tests = []

let command_tests = []
let piece_tests = []
let gui_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ state_tests; command_tests; piece_tests; gui_tests ]

let _ = run_test_tt_main suite

open OUnit2
open Chess
open Command
open State

(* Test Plan: For OUnit testing, we test out our UI and game. With the UI, the
   tests handle any inputs with upper case letters, lower case letters, spaces,
   valid or invalid inputs, etc... in our [command_tests]. Both black-box and
   glass-box testing were used. To achieve correctness, edge cases for various
   parameters are tested. For the actual game, we have tests to ensure that the
   basic pieces move correctly. Furthermore, we cover many different features of
   the game and their edge cases with castling, check, checkmate, stalemate,
   etc... Our test cases cover most of the aspects in the game that we wanted to
   implement, providing evidence for correctness.

   For manual testing, we tested all systems, the UI, and the game
   implementation, individually as we progressed through the project using the
   UI. Not only was the UI constantly tested for, every time a piece or game
   aspect like check, castle, checkmate, etc..., was implemented, the basic game
   funcitonality and more intricate features were also manually tested in on our
   UI. With both black-box and glass-box testing manually, further support for
   correctness was developed. *)

let parse_test (name : string) (input : string) (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (get_command (parse input)) ~printer:Fun.id

let parse_test_invalid (name : string) (input : string) =
  name >:: fun _ -> assert_raises MalformedInput (fun () -> parse input)

let pawn_tests (name : string) (board_state : board_state) =
  name >:: fun _ ->
  let pawn_pos = moves_pawn_single board_state false in
  let combined =
    List.fold_right Int64.logor (List.map snd pawn_pos) Int64.zero
  in
  let _ = Printf.printf "print_start\n%s\n" (Int64.to_string combined) in
  assert_equal true true

let rec gen_board board move_lst =
  match move_lst with
  | [] -> board
  | h :: t -> gen_board (move board (parse h)) t

let command_tests =
  [
    parse_test "basic input" "a3 a4" "a3 a4";
    parse_test "uppercase both" "B3 C6" "b3 c6";
    parse_test "uppercase one" "g8 F6" "g8 f6";
    parse_test "extreme bounds" "a1 h8" "a1 h8";
    parse_test "white space allowed" "    a2 a3     " "a2 a3";
    parse_test "quit game" "quit" "quit game";
    parse_test "quit capital white space" "     QUIT     " "quit game";
    parse_test_invalid "out of bounds invalid" "a0 c1";
    parse_test_invalid "no in between spacing" "a3   a4";
    parse_test_invalid "no spaces" "a3a4";
    parse_test_invalid "same square" "c1 c1";
    parse_test_invalid "random" "asdflk214p9u124 1249u09v";
  ]

let move_test_illegal (name : string) (board_state : board_state) (cmd : string)
    =
  name >:: fun _ ->
  let nbs = move board_state (Command.parse cmd) in
  assert_equal nbs board_state

let castle_test (name : string) (board_state : board_state) (w_move : bool)
    (expected_output : (Int64.t * Int64.t) list) =
  name >:: fun _ ->
  let possible_moves = moves_kingcastle board_state w_move in
  assert_equal possible_moves expected_output

let rec board_printer board_list =
  match board_list with
  | [] -> ()
  | (_, a, b) :: t ->
      let _ = print_board b in
      let _ = print_endline (Int64.to_string (get_val b)) in
      board_printer t

let print_Int64_list lst =
  List.map (fun a -> print_endline (Int64.to_string a)) lst

let cmp_move_lists move_lst1 move_lst2 =
  let move_lst2 = List.fold_left (fun acc (_, b) -> b :: acc) [] move_lst2 in
  let lst1 = List.sort_uniq Int64.compare move_lst1 in
  let lst2 = List.sort_uniq Int64.compare move_lst2 in
  lst1 = lst2

let stalemate_moves =
  [
    "e2 e3";
    "a7 a5";
    "d1 h5";
    "a8 a6";
    "h5 a5";
    "h7 h5";
    "h2 h4";
    "a6 h6";
    "a5 c7";
    "f7 f6";
    "c7 d7";
    "e8 f7";
    "d7 b7";
    "d8 d3";
    "b7 b8";
    "d3 h7";
    "b8 c8";
    "f7 g6";
    "c8 e6";
  ]

let scholars_mate =
  [ "e2 e4"; "a7 a6"; "f1 c4"; "a6 a5"; "d1 h5"; "a5 a4"; "h5 f7" ]

let sym_mate =
  [
    "e2 e4";
    "e7 e5";
    "g1 f3";
    "g8 f6";
    "b1 c3";
    "b8 c6";
    "f1 b5";
    "f8 b4";
    "e1 g1";
    "e8 g8";
    "d2 d3";
    "d7 d6";
    "b5 c6";
    "b4 c3";
    "c6 b7";
    "c3 b2";
    "b7 a8";
    "b2 a1";
    "c1 g5";
    "c8 g4";
    "d1 a1";
    "d8 a8";
    "g5 f6";
    "g4 f3";
    "f6 e5";
    "f3 e4";
    "e5 g7";
    "e4 g2";
    "g7 f8";
    "g2 f1";
    "a1 g7";
  ]

let draw_by_50 =
  [
    "e2 e4";
    "e7 e5";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
  ]

let disc_check =
  [ "e2 e4"; "d7 d5"; "d1 e2"; "e8 d7"; "h2 h3"; "d7 e6"; "e4 d5" ]

let disc_ep_check =
  [
    "e2 e4";
    "e7 e5";
    "d2 d4";
    "e5 d4";
    "d1 e2";
    "d4 d3";
    "e4 e5";
    "d7 d5";
    "e5 d6";
    "e8 d7";
    "g1 f3";
  ]

let ep_delay =
  [
    "e2 e4";
    "e7 e5";
    "d2 d4";
    "e5 d4";
    "d1 e2";
    "d4 d3";
    "e4 e5";
    "d7 d5";
    "g1 f3";
    "e8 d7";
  ]

let castle_setup =
  [
    "e2 e4";
    "e7 e5";
    "g1 f3";
    "g8 f6";
    "f1 c4";
    "f8 c5";
    "d2 d3";
    "d7 d6";
    "b1 c3";
    "b8 c6";
    "c1 g5";
    "c8 g4";
    "d1 d2";
    "d8 d7";
  ]

let castle_setup_check =
  [ "e2 e4"; "d7 d5"; "g1 f3"; "e7 e6"; "f1 d3"; "d8 e7"; "e4 d5"; "e6 d5" ]

let castle_setup_king_move =
  [
    "e2 e4";
    "e7 e5";
    "g1 f3";
    "g8 f6";
    "f1 c4";
    "f8 c5";
    "e1 e2";
    "e8 e7";
    "e2 e1";
    "e7 e8";
  ]

let castle_setup_thru_check =
  [ "e2 e3"; "d7 d5"; "g1 f3"; "c8 f5"; "f1 d3"; "f5 d3" ]

let castle_setup_rook_move =
  [
    "e2 e4";
    "e7 e5";
    "g1 f3";
    "g8 f6";
    "f1 c4";
    "f8 c5";
    "d2 d3";
    "d7 d6";
    "b1 c3";
    "b8 c6";
    "c1 g5";
    "c8 g4";
    "d1 d2";
    "d8 d7";
    "h1 g1";
    "h8 g8";
    "g1 h1";
    "g8 h8";
  ]

let castle_setup_into_check_b =
  [
    "d2 d4";
    "d7 d5";
    "b1 c3";
    "b8 c6";
    "c1 g5";
    "c8 g4";
    "d1 d3";
    "d8 d6";
    "d3 f5";
  ]

let stalemate_board = gen_board init_chess stalemate_moves
let schl_mate_board = gen_board init_chess scholars_mate
let sym_mate_board = gen_board init_chess sym_mate
let draw_by_50_board = gen_board init_chess draw_by_50
let e4_board = move init_chess (parse "e2 e4")
let disc_check_board = gen_board init_chess disc_check
let castle_setup_board = gen_board init_chess castle_setup
let castle_setup_board_b = move castle_setup_board (parse "h2 h3")
let csc_board = gen_board init_chess castle_setup_check
let cskm_board = gen_board init_chess castle_setup_king_move
let cskmb_board = move cskm_board (parse "h2 h3")
let cstc_board = gen_board init_chess castle_setup_thru_check
let csrm_board = gen_board init_chess castle_setup_rook_move
let csrmb_board = move csrm_board (parse "e1 c1")
let csicb_board = gen_board init_chess castle_setup_into_check_b
let csq_board = move csicb_board (parse "a8 d8")
let epd_board = gen_board init_chess ep_delay

let init_pawn_moves =
  [
    Int64.of_int 65536;
    Int64.of_int 131072;
    Int64.of_int 262144;
    Int64.of_int 524288;
    Int64.of_int 1048576;
    Int64.of_int 2097152;
    Int64.of_int 4194304;
    Int64.of_int 8388608;
    Int64.of_int 16777216;
    Int64.of_int 33554432;
    Int64.of_int 67108864;
    Int64.of_int 134217728;
    Int64.of_int 268435456;
    Int64.of_int 536870912;
    Int64.of_int 1073741824;
    Int64.of_int 2147483648;
  ]

let init_knight_moves =
  [
    Int64.of_int 65536;
    Int64.of_int 262144;
    Int64.of_int 2097152;
    Int64.of_int 8388608;
  ]

let castle_tests =
  [
    castle_test "no castle to start" init_chess true [];
    castle_test "no castle black to start" e4_board false [];
    castle_test "castle both white" castle_setup_board true
      [ (8L, 32L); (8L, 2L) ];
    castle_test "castle both black" castle_setup_board_b false
      [
        (Int64.shift_left 8L 56, Int64.shift_left 32L 56);
        (Int64.shift_left 8L 56, Int64.shift_left 2L 56);
      ];
    castle_test "no castle out of check" csc_board true [];
    castle_test "no castle if king move" cskm_board true [];
    castle_test "no castle if king move black" cskmb_board false [];
    castle_test "no castling thru check" cstc_board true [];
    castle_test "no l castle if l rook moved w" csrm_board true [ (8L, 32L) ];
    castle_test "no l castle if l rook moved b" csrmb_board false
      [ (Int64.shift_left 8L 56, Int64.shift_left 32L 56) ];
    castle_test "no castle into check" csicb_board false [];
    castle_test "no castle into check" csq_board true [ (8L, 32L) ];
  ]

let move_tests =
  [
    move_test_illegal "big pawn" init_chess "e2 e5";
    move_test_illegal "castle start" init_chess "e1 g1";
    move_test_illegal "big knight" init_chess "g1 f4";
    move_test_illegal "bishop thru" init_chess "f1 d3";
    move_test_illegal "big pawn black" e4_board "d7 d4";
    move_test_illegal "castle start" e4_board "e8 c8";
    move_test_illegal "rook thru" e4_board "a8 a6";
    move_test_illegal "delayed en passant" epd_board "e5 d6";
  ]

let state_tests =
  [
    ( "Stalemate" >:: fun _ ->
      assert_equal [] (all_legal_moves (pseudolegal_moves stalemate_board)) );
    ( "Checkmate 4 moves" >:: fun _ ->
      assert_equal [] (all_legal_moves (pseudolegal_moves schl_mate_board)) );
    ( "Checkmate Symmetrical Game" >:: fun _ ->
      assert_equal [] (all_legal_moves (pseudolegal_moves sym_mate_board)) );
    ("Draw By Fifty" >:: fun _ -> assert_equal 101 (get_fifty draw_by_50_board));
    ( "Initial Attacking Squares White" >:: fun _ ->
      assert_equal 16711680 (Int64.to_int (enemy_attacks init_chess)) );
    ( "Initial Attacking Squares Black" >:: fun _ ->
      assert_equal 280375465082880 (Int64.to_int (enemy_attacks e4_board)) );
    (*("Previous Board After One Move" >:: fun _ -> assert_equal e4_board
      (List.hd (get_prev_boards e4_board)));*)
    ( "Inital King Moves" >:: fun _ ->
      assert_equal [] (moves_king init_chess true) );
    ( "Inital Queen Moves" >:: fun _ ->
      assert_equal [] (moves_queen init_chess true) );
    ( "Inital Rook Moves" >:: fun _ ->
      assert_equal [] (moves_rook init_chess true) );
    ( "Inital Knight Moves" >:: fun _ ->
      assert (cmp_move_lists init_knight_moves (moves_knight init_chess true))
    );
    ( "Inital Bishop Moves" >:: fun _ ->
      assert_equal [] (moves_bishop init_chess true) );
    ( "Inital Pawn Moves" >:: fun _ ->
      assert (
        cmp_move_lists init_pawn_moves
          (moves_pawn_single init_chess true @ moves_pawn_double init_chess true))
    );
    ( "Inital En Passant Moves" >:: fun _ ->
      assert ([] = moves_ep_captures init_chess true) );
    ("No check initially" >:: fun _ -> assert_equal false (in_check init_chess));
    ( "Discovered Check" >:: fun _ ->
      assert_equal true (in_check disc_check_board) );
    ( "Discovered Check Not Mate" >:: fun _ ->
      assert ([] <> all_legal_moves (pseudolegal_moves disc_check_board)) );
    ( "Moves Still Generated in Check Queen" >:: fun _ ->
      assert ([] <> moves_queen disc_check_board false) );
    ( "No Valid Moves For Rook Discovered Check" >:: fun _ ->
      assert ([] = moves_rook disc_check_board false) );
    ( "Moves Still Generated in Check Bishop" >:: fun _ ->
      assert ([] <> moves_bishop disc_check_board false) );
    ( "Moves Still Generated in Check Knight" >:: fun _ ->
      assert ([] <> moves_knight disc_check_board false) );
    ( "Moves Still Generated in Check Pawns (Single)" >:: fun _ ->
      assert ([] <> moves_pawn_single disc_check_board false) );
    ( "Moves Still Generated in Checks Pawns (Double)" >:: fun _ ->
      assert ([] <> moves_pawn_double disc_check_board false) );
    ( "No Valid Moves For Pawn (EP) Discovered Check" >:: fun _ ->
      assert ([] = moves_ep_captures disc_check_board false) );
  ]

let piece_tests = [ pawn_tests "test" init_chess ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [ state_tests; command_tests; castle_tests; move_tests; piece_tests ]

let _ = run_test_tt_main suite

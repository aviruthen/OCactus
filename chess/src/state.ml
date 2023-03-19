type board_state = {
  b_pawns : Int64.t;
  b_bishops : Int64.t;
  b_knights : Int64.t;
  b_rooks : Int64.t;
  b_queen : Int64.t;
  b_king : Int64.t;
  w_pawns : Int64.t;
  w_bishops : Int64.t;
  w_knights : Int64.t;
  w_rooks : Int64.t;
  w_queen : Int64.t;
  w_king : Int64.t;
  all_whites : Int64.t;
  all_blacks : Int64.t;
  ep : Int64.t;
  b_castle_l : bool;
  b_castle_r : bool;
  w_castle_l : bool;
  w_castle_r : bool;
  w_turn : bool;
  in_check_w : bool;
  in_check_b : bool;
}

<<<<<<< Updated upstream
let init_chess = {
   b_pawns = Int64.(logxor (shift_right_logical (minus_one) 8) (shift_right_logical (minus_one) 16));
   b_bishops = Int64.(shift_left (logor (shift_left (one) 2) (shift_left (one) 5)) 56);
   b_knights = Int64.(shift_left (logor (shift_left (one) 1) (shift_left (one) 6)) 56);
   b_rooks = Int64.(shift_left (logor one (shift_left (one) 7)) 56);
   b_queen = Int64.(shift_left one 60);
   b_king = Int64.(shift_left one 59);
   w_pawns = Int64.(logxor (shift_right_logical (minus_one) 48) (shift_right_logical (minus_one) 56));
   w_bishops = Int64.(logor (shift_left (one) 2) (shift_left (one) 5));
   w_knights = Int64.(logor (shift_left (one) 1) (shift_left (one) 6));
   w_rooks = Int64.(logor one (shift_left (one) 7));
   w_queen = Int64.(shift_left one 4);
   w_king = Int64.(shift_left one 3);
   all_whites = Int64.(shift_right_logical minus_one 48);
   all_blacks = Int64.(logxor minus_one (shift_right_logical (minus_one) 16));
   ep = Int64.zero;
   b_castle_l = false;
   b_castle_r = false;
   w_castle_l = false;
   w_castle_r = false;
   w_turn = true;
   in_check_w = false;
   in_check_b = false;
}
=======
let white_last_file =
  Int64.logxor
    (Int64.shift_right_logical Int64.minus_one 8)
    (Int64.shift_right_logical Int64.minus_one 16)

let black_last_file =
  Int64.logxor
    (Int64.shift_left Int64.minus_one 8)
    (Int64.shift_left Int64.minus_one 16)

let white_first_files = Int64.shift_right_logical Int64.minus_one 8
let black_first_files = Int64.shift_left Int64.minus_one 8

let rec logarithm (num : Int64.t) (acc : int) : int = 
  if num = Int64.one then acc else logarithm (Int64.shift_right_logical num 1) (acc + 1)

let logarithm_iter (num : Int64.t) = logarithm num 0
>>>>>>> Stashed changes

let pseudolegal_moves (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  raise (Failure "Unimplemented")

let all_legal_moves (board_moves : (Int64.t * Int64.t * board_state) list) :
    (Int64.t * Int64.t * board_state) list =
    List.filter (fun (_,_,c) -> (not c.w_turn <>  c.in_check_w) || (c.w_turn <>  c.in_check_b)) board_moves

let rec bit_loop (bitmap : Int64.t) (acc_maps : Int64.t list) (acc_count : int)
    : Int64.t list =
  if bitmap = Int64.zero then acc_maps
  else if Int64.rem bitmap (Int64.shift_left Int64.one 1) = Int64.zero then
    bit_loop (Int64.shift_right_logical bitmap 1) acc_maps (acc_count + 1)
  else
    bit_loop
      (Int64.shift_right_logical bitmap 1)
      (Int64.shift_right_logical Int64.one acc_count :: acc_maps)
      (acc_count + 1)

let bit_loop_iter (bitmap : Int64.t) : Int64.t list = bit_loop bitmap [] 0

let rec list_join (list1 : Int64.t list) (list2 : Int64.t list)
    (acc : (Int64.t * Int64.t) list) =
  if List.length list1 = 0 then acc
  else
    list_join (List.tl list1) (List.tl list2)
      ((List.hd list1, List.hd list2) :: acc)

let list_join_iter (list1 : Int64.t list) (list2 : Int64.t list) :
    (Int64.t * Int64.t) list =
  list_join list1 list2 []

let moves_king (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_kingcastle (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_queen (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_rook (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_knight (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_bishop (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let rec move_pawn_double_helper (board_state : board_state) (white_turn : bool)
    (mask : Int64.t) (color_mask : Int64.t) : (Int64.t * Int64.t) list =
  let occupied = Int64.logor board_state.all_whites board_state.all_blacks in
  if white_turn then
    let to_pos = Int64.shift_left mask 16 in
    match
      if
        Int64.zero <> Int64.logand mask board_state.w_pawns
        && Int64.zero <> Int64.logand mask color_mask
        && Int64.zero
           <> Int64.logor
                (Int64.logand (Int64.shift_left mask 8) occupied)
                (Int64.logand to_pos occupied)
      then Some (mask, to_pos)
      else None
    with
    | Some tup ->
        tup
        :: move_pawn_double_helper board_state white_turn
             (Int64.shift_left mask 1) color_mask
    | None ->
        move_pawn_double_helper board_state white_turn (Int64.shift_left mask 1)
          color_mask
  else
    let to_pos = Int64.shift_right_logical mask 16 in
    match
      if
        Int64.zero <> Int64.logand mask board_state.b_pawns
        && Int64.zero <> Int64.logand mask color_mask
        && Int64.zero
           <> Int64.logor
                (Int64.logand (Int64.shift_right_logical mask 8) occupied)
                (Int64.logand to_pos occupied)
      then Some (mask, to_pos)
      else None
    with
    | Some tup ->
        tup
        :: move_pawn_double_helper board_state white_turn
             (Int64.shift_right_logical mask 1)
             color_mask
    | None ->
        move_pawn_double_helper board_state white_turn
          (Int64.shift_right_logical mask 1)
          color_mask

let moves_pawn_double (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  if white_turn then
    let rank =
      Int64.logxor
        (Int64.shift_right_logical Int64.minus_one 48)
        (Int64.shift_right_logical Int64.minus_one 56)
    in
    move_pawn_double_helper board_state white_turn
      (Int64.shift_left Int64.one 8)
      rank
  else
    let rank =
      Int64.logxor
        (Int64.shift_left Int64.minus_one 8)
        (Int64.shift_left Int64.minus_one 16)
    in
    move_pawn_double_helper board_state white_turn
      (Int64.shift_left Int64.one 55)
      rank

(* Enumerates all forward pawn moves as long as square not blocked*)
let _moves_pawn_forward (board_state : board_state) (white_turn : bool)
    (filter : Int64.t) : (Int64.t * Int64.t) list =
  let filtered =
    if white_turn then Int64.logand filter board_state.w_pawns
    else Int64.logand filter board_state.b_pawns
  in
  let new_positions =
    if white_turn then Int64.shift_left filtered 8
    else Int64.shift_right_logical filtered 8
  in
  let blocked_map =
    Int64.logand
      (Int64.logor board_state.all_blacks board_state.all_whites)
      new_positions
  in
  let valid_positions = Int64.logxor new_positions blocked_map in
  let original_positions =
    if white_turn then Int64.shift_right_logical valid_positions 8
    else Int64.shift_left valid_positions 8
  in
  list_join_iter
    (bit_loop_iter original_positions)
    (bit_loop_iter valid_positions)

(* Enumerates all diagonal pawn moves regardless of if to position is occupied*)
let _moves_pawn_diagonal (board_state : board_state) (white_turn : bool)
    (filter : Int64.t) : (Int64.t * Int64.t) list =
  let filtered =
    if white_turn then Int64.logand filter board_state.w_pawns
    else Int64.logand filter board_state.b_pawns
  in
  let new_positions_left =
    if white_turn then Int64.shift_left filtered 7
    else Int64.shift_right_logical filtered 7
  in
  let new_positions_right =
    if white_turn then Int64.shift_left filtered 9
    else Int64.shift_right_logical filtered 9
  in
  let original_positions =
    bit_loop_iter (if white_turn then filtered else filtered)
  in
  List.append
    (list_join_iter original_positions (bit_loop_iter new_positions_left))
    (list_join_iter original_positions (bit_loop_iter new_positions_right))

(* filters pawns and finds all pseudolegal captures *)
let _moves_pawn_cap (board_state : board_state) (white_turn : bool) (filter : Int64.t) : (Int64.t * Int64.t) list =
  let filtered_pawns =
    if white_turn then Int64.logand filter board_state.w_pawns
    else Int64.logand filter board_state.b_pawns
  in
  let occupied = Int64.logor board_state.all_blacks board_state.all_whites in
  let can_atk_left =
    if white_turn then Int64.logand occupied (Int64.shift_left filtered_pawns 9)
    else Int64.logand occupied (Int64.shift_right_logical filtered_pawns 9)
  in
  let can_atk_right =
    if white_turn then Int64.logand occupied (Int64.shift_left filtered_pawns 7)
    else Int64.logand occupied (Int64.shift_right_logical filtered_pawns 7)
  in
  let original_atk_left =
    if white_turn then Int64.shift_right_logical can_atk_left 9
    else Int64.shift_left can_atk_left 9
  in
  let original_atk_right =
    if white_turn then Int64.shift_right_logical can_atk_right 7
    else Int64.shift_left can_atk_right 7
  in
  let filter = Int64.logor original_atk_left original_atk_right in
  _moves_pawn_diagonal board_state white_turn filter

let moves_pawn_single (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter = if white_turn then white_first_files else black_first_files in
  let forward_moves = _moves_pawn_forward board_state white_turn filter in
  let capture_moves = _moves_pawn_cap board_state white_turn filter in
  let ep = if board_state.ep = Int64.zero then [] else
    let row = (logarithm_iter board_state.ep) / 8 in
    let ones_row = Int64.shift_left black_last_file (row * 8) in
    let ep_neighbors = Int64.logor (Int64.shift_left board_state.ep 1) (Int64.shift_right_logical board_state.ep 1) in
    let ep_neighbors = Int64.logand ep_neighbors ones_row in
    let ep_candidates = if white_turn then Int64.logand ep_neighbors board_state.w_pawns else Int64.logand ep_neighbors board_state.b_pawns in
    [] 
  in
  List.append forward_moves capture_moves

let moves_pawn_attacks (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  _moves_pawn_diagonal board_state white_turn Int64.minus_one

let moves_promote_no_cap (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter = if white_turn then white_last_file else black_last_file in
  _moves_pawn_forward board_state white_turn filter

let moves_promote_cap (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter = if white_turn then white_last_file else black_last_file in
  _moves_pawn_cap board_state white_turn filter

let list_or (bitmaps : (Int64.t * Int64.t) list) : Int64.t =
  let bitmaps = List.map (fun tup -> fst tup) bitmaps in
  List.fold_right Int64.logor bitmaps Int64.minus_one

let enemy_attacks (board_state : board_state) : Int64.t =
  let king_atk = list_or (moves_king board_state (not board_state.w_turn)) in
  let queen_atk = list_or (moves_queen board_state (not board_state.w_turn)) in
  let rook_atk = list_or (moves_rook board_state (not board_state.w_turn)) in
  let bishop_atk =
    list_or (moves_bishop board_state (not board_state.w_turn))
  in
  let knight_atk =
    list_or (moves_knight board_state (not board_state.w_turn))
  in
  let pawn_atk =
    list_or (moves_pawn_attacks board_state (not board_state.w_turn))
  in
  let promote_atk =
    list_or (moves_promote_cap board_state (not board_state.w_turn))
  in
  queen_atk |> Int64.logor king_atk |> Int64.logor rook_atk
  |> Int64.logor bishop_atk |> Int64.logor knight_atk |> Int64.logor pawn_atk
  |> Int64.logor promote_atk

(* Obtains the square the user would like to move to from their input command
   represented as an Int64.t that corresponds to the bitboard cmd has type
   Command.t  -- we assume that the string input is of the form 
   "starting_space ending_space", or e.g. "a4 a5"*)
let process_square cmd = 
  let raw_cmd = Command.get_command cmd in
  let sq1_letter = String.get raw_cmd 0 in
  let sq1_number = String.get raw_cmd 1 in
  let sq2_letter = String.get raw_cmd (String.length raw_cmd - 2) 
    |> Char.lowercase_ascii in
  let sq2_number = (String.get raw_cmd (String.length raw_cmd - 1)) 
    |> Char.lowercase_ascii in

  (Int64.shift_left Int64.one (8*(Char.code sq1_letter - 97) + 
  Char.code sq1_number - 49), 
  Int64.shift_left Int64.one (8*(Char.code sq2_letter - 97) + 
  Char.code sq2_number - 49))


(* Obtains the piece that the user would like to move as a string. cmd has type
   Command.t *)
let process_piece cmd = raise (Failure "Unimplemented")

(* Given board_state, computes all legal moves (and prInt64.ts message about the
   game ending in this step if that is the case), queries and repeatedly waits
   for command corresponding to legal move, then recurses on BoardState
   corresponding to chosen move *)
let rec_func (board_state : board_state) = raise (Failure "Unimplemented")
   game ending in this step if that is the case), queries and repeatedly waits
   for command corresponding to legal move, then recurses on BoardState
   corresponding to chosen move *)
let rec_func (board_state : board_state) = raise (Failure "Unimplemented")

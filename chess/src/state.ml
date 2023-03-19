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

let pseudolegal_moves (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  raise (Failure "Unimplemented")

let all_legal_moves (board_moves : (Int64.t * Int64.t * board_state) list) :
    (Int64.t * Int64.t * board_state) list =
  raise (Failure "Unimplemented")

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
        (Int64.shift_right_logical Int64.minus_one 8)
        (Int64.shift_right_logical Int64.minus_one 16)
    in
    move_pawn_double_helper board_state white_turn
      (Int64.shift_left Int64.one 55)
      rank

let moves_pawn_forward (board_state : board_state) (white_turn : bool)
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

let moves_pawn_capture (board_state : board_state) (white_turn : bool)
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

let moves_pawn_single (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter =
    if white_turn then Int64.shift_right_logical Int64.minus_one 16
    else Int64.shift_left Int64.minus_one 16
  in
  let forward_moves = moves_pawn_forward board_state white_turn filter in
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
  let capture_moves = moves_pawn_capture board_state white_turn filter in
  List.append forward_moves capture_moves

let moves_pawn_attacks (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  moves_pawn_capture board_state white_turn Int64.minus_one

let moves_promote_no_cap (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_promote_cap (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

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
   Command.t *)
let process_square cmd = raise (Failure "Unimplemented")

(* Obtains the piece that the user would like to move as a string. cmd has type
   Command.t *)
let process_piece cmd = raise (Failure "Unimplemented")

(* Given board_state, computes all legal moves (and prInt64.ts message about the
   game ending in this step if that is the case), queries and repeatedly waits
   for command corresponding to legal move, then recurses on BoardState
   corresponding to chosen move *)
let rec_func (board_state : board_state) = raise (Failure "Unimplemented")

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

let init_chess =
  {
    b_pawns =
      Int64.(
        logxor
          (shift_right_logical minus_one 8)
          (shift_right_logical minus_one 16));
    b_bishops =
      Int64.(shift_left (logor (shift_left one 2) (shift_left one 5)) 56);
    b_knights =
      Int64.(shift_left (logor (shift_left one 1) (shift_left one 6)) 56);
    b_rooks = Int64.(shift_left (logor one (shift_left one 7)) 56);
    b_queen = Int64.(shift_left one 60);
    b_king = Int64.(shift_left one 59);
    w_pawns =
      Int64.(
        logxor
          (shift_right_logical minus_one 48)
          (shift_right_logical minus_one 56));
    w_bishops = Int64.(logor (shift_left one 2) (shift_left one 5));
    w_knights = Int64.(logor (shift_left one 1) (shift_left one 6));
    w_rooks = Int64.(logor one (shift_left one 7));
    w_queen = Int64.(shift_left one 4);
    w_king = Int64.(shift_left one 3);
    all_whites = Int64.(shift_right_logical minus_one 48);
    all_blacks = Int64.(logxor minus_one (shift_right_logical minus_one 16));
    ep = Int64.zero;
    b_castle_l = false;
    b_castle_r = false;
    w_castle_l = false;
    w_castle_r = false;
    w_turn = true;
    in_check_w = false;
    in_check_b = false;
  }

let white_first_files = Int64.shift_right_logical Int64.minus_one 8
let black_first_files = Int64.shift_left Int64.minus_one 8
let white_last_file = Int64.logxor white_first_files Int64.minus_one
let black_last_file = Int64.logxor black_first_files Int64.minus_one

let rec pad prior mask counts =
  if counts = 8 then mask
  else pad (Int64.shift_left prior 8) (Int64.logor mask prior) (counts + 1)

let edge_mask =
  let left_side = pad Int64.one Int64.zero 0 in
  let right_side = pad (Int64.shift_left Int64.one 7) Int64.zero 0 in
  Int64.logor left_side right_side

let center_mask = Int64.logxor Int64.minus_one edge_mask

(* credits to
   https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating *)
let flip_vertical (num : Int64.t) : Int64.t =
  let const_1 = Int64.of_int 71777214294589695 in
  let const_2 = Int64.of_int 281470681808895 in
  let num =
    Int64.(
      logor
        (logand (shift_right_logical num 8) const_1)
        (shift_left (logand num const_1) 8))
  in
  let num =
    Int64.(
      logor
        (logand (shift_right_logical num 16) const_2)
        (shift_left (logand num const_2) 16))
  in
  Int64.(logor (shift_right_logical num 32) (shift_left num 32))

(* credits to
   https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating *)
let mirror_horizontal (num : Int64.t) : Int64.t =
  let const_1 = Int64.of_string "0x5555555555555555" in
  let const_2 = Int64.of_string "0x3333333333333333" in
  let const_3 = Int64.of_string "0x0f0f0f0f0f0f0f0f" in
  let num =
    Int64.(
      logor
        (logand (shift_right_logical num 1) const_1)
        (shift_left (logand num const_1) 1))
  in
  let num =
    Int64.(
      logor
        (logand (shift_right_logical num 2) const_2)
        (shift_left (logand num const_2) 2))
  in
  Int64.(
    logor
      (logand (shift_right_logical num 4) const_3)
      (shift_left (logand num const_3) 4))

let rec pawn_lookup_builder_white (mask_map : (Int64.t * Int64.t) list)
    (counts : int) =
  if counts > 63 then mask_map
  else if counts mod 8 = 0 then
    pawn_lookup_builder_white
      (( Int64.shift_left Int64.one counts,
         Int64.shift_left Int64.one (counts + 9) )
      :: mask_map)
      (counts + 7)
  else
    pawn_lookup_builder_white
      (( Int64.shift_left Int64.one counts,
         Int64.shift_left Int64.one (counts + 7) )
      :: mask_map)
      (counts + 1)

let pawn_lookup_white = pawn_lookup_builder_white [] 8

let pawn_lookup_black =
  List.map
    (fun tup ->
      ( mirror_horizontal (flip_vertical (fst tup)),
        mirror_horizontal (flip_vertical (snd tup)) ))
    pawn_lookup_white

let rec logarithm (num : Int64.t) (acc : int) : int =
  if num = Int64.one then acc
  else logarithm (Int64.shift_right_logical num 1) (acc + 1)

let logarithm_iter (num : Int64.t) = logarithm num 0

let all_legal_moves (board_moves : (Int64.t * Int64.t * board_state) list) :
    (Int64.t * Int64.t * board_state) list =
  List.filter
    (fun (_, _, c) ->
      (not c.w_turn) <> c.in_check_w || c.w_turn <> c.in_check_b)
    board_moves

let rec bit_loop (bitmap : Int64.t) (acc_maps : Int64.t list) (acc_count : int)
    : Int64.t list =
  if bitmap = Int64.zero then acc_maps
  else if Int64.rem bitmap (Int64.shift_left Int64.one 1) = Int64.zero then
    bit_loop (Int64.shift_right_logical bitmap 1) acc_maps (acc_count + 1)
  else
    bit_loop
      (Int64.shift_right_logical bitmap 1)
      (Int64.shift_left Int64.one acc_count :: acc_maps)
      (acc_count + 1)

let bit_loop_iter (bitmap : Int64.t) : Int64.t list = bit_loop bitmap [] 0

let rec list_join (list1 : Int64.t list) (list2 : Int64.t list)
    (acc : (Int64.t * Int64.t) list) =
  if List.length list1 = 0 then acc
  else
    list_join (List.tl list1) (List.tl list2)
      ((List.hd list1, List.hd list2) :: acc)

(* The first list must be the shortest *)
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

let moves_pawn_double (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filtered_pawns =
    if white_turn then
      Int64.logand board_state.w_pawns (Int64.shift_left black_last_file 8)
    else
      Int64.logand board_state.b_pawns
        (Int64.shift_right_logical white_last_file 8)
  in
  let occupied = Int64.logor board_state.all_blacks board_state.all_whites in
  let forward_1 =
    if white_turn then
      Int64.logand occupied (Int64.shift_left black_last_file 16)
    else Int64.logand occupied (Int64.shift_right_logical white_last_file 16)
  in
  let forward_2 =
    if white_turn then
      Int64.logand occupied (Int64.shift_left black_last_file 24)
    else Int64.logand occupied (Int64.shift_right_logical white_last_file 24)
  in
  let forward_filter =
    if white_turn then
      Int64.logor
        (Int64.shift_right_logical forward_1 8)
        (Int64.shift_right_logical forward_2 16)
    else
      Int64.logor (Int64.shift_left forward_1 8) (Int64.shift_left forward_2 16)
  in
  let filtered_pawns =
    Int64.logand (Int64.lognot forward_filter) filtered_pawns
  in
  list_join_iter
    (bit_loop_iter filtered_pawns)
    (bit_loop_iter
       (if white_turn then Int64.shift_left filtered_pawns 16
       else Int64.shift_right_logical filtered_pawns 16))

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
  let edge_filtered = Int64.logand edge_mask filtered in
  let edge_pieces = bit_loop_iter edge_filtered in
  let lookup_list =
    if white_turn then pawn_lookup_white else pawn_lookup_black
  in
  let edge_moves =
    list_join_iter edge_pieces
      (List.map
         (fun bitmap ->
           snd (List.find (fun tup -> fst tup = bitmap) lookup_list))
         edge_pieces)
  in
  let central_filtered = Int64.logand center_mask filtered in
  let new_positions_left =
    if white_turn then Int64.shift_left central_filtered 9
    else Int64.shift_right_logical central_filtered 9
  in
  let new_positions_right =
    if white_turn then Int64.shift_left central_filtered 7
    else Int64.shift_right_logical central_filtered 7
  in
  let central_list = bit_loop_iter central_filtered in
  let center_moves =
    List.append
      (list_join_iter central_list (bit_loop_iter new_positions_left))
      (list_join_iter central_list (bit_loop_iter new_positions_right))
  in
  List.append edge_moves center_moves

(* filters pawns and finds all pseudolegal captures *)
let _moves_pawn_cap (board_state : board_state) (white_turn : bool)
    (filter : Int64.t) : (Int64.t * Int64.t) list =
  let diagonal_moves = _moves_pawn_diagonal board_state white_turn filter in
  let valid_cap_filter =
    if white_turn then board_state.all_blacks else board_state.all_whites
  in
  List.filter
    (fun tup -> Int64.logand valid_cap_filter (snd tup) <> Int64.zero)
    diagonal_moves

let moves_pawn_single (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter = if white_turn then white_first_files else black_first_files in
  let forward_moves = _moves_pawn_forward board_state white_turn filter in
  let capture_moves = _moves_pawn_cap board_state white_turn filter in
  List.append forward_moves capture_moves

let moves_ep_captures (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  if board_state.ep = Int64.zero then []
  else
    let row = logarithm_iter board_state.ep / 8 in
    let ones_row = Int64.shift_left black_last_file (row * 8) in
    let ep_neighbors =
      Int64.logor
        (Int64.shift_left board_state.ep 1)
        (Int64.shift_right_logical board_state.ep 1)
    in
    let ep_neighbors = Int64.logand ep_neighbors ones_row in
    let ep_candidates =
      if white_turn then Int64.logand ep_neighbors board_state.w_pawns
      else Int64.logand ep_neighbors board_state.b_pawns
    in
    let open_squares =
      Int64.lognot (Int64.logor board_state.all_blacks board_state.all_whites)
    in
    let ep_filter_above =
      if white_turn then
        Int64.logand (Int64.shift_left board_state.ep 8) open_squares
      else
        Int64.logand (Int64.shift_right_logical board_state.ep 8) open_squares
    in
    let ep_candidate_filter =
      if white_turn then
        Int64.logor
          (Int64.shift_right_logical ep_filter_above 7)
          (Int64.shift_right_logical ep_filter_above 9)
      else
        Int64.logor
          (Int64.shift_left ep_filter_above 7)
          (Int64.shift_left ep_filter_above 9)
    in
    let ep_candidates = Int64.logand ep_candidate_filter ep_candidates in
    list_join_iter
      (bit_loop_iter ep_candidates)
      (List.append [ ep_filter_above ] [ ep_filter_above ])

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

let piece_at_spot board_state (move : Int64.t) : string =
  if board_state.w_turn then
    if Int64.(logand move board_state.b_pawns <> zero) then "p"
    else if Int64.(logand move board_state.b_bishops <> zero) then "b"
    else if Int64.(logand move board_state.b_knights <> zero) then "n"
    else if Int64.(logand move board_state.b_rooks <> zero) then "r"
    else "q"
  else if Int64.(logand move board_state.w_pawns <> zero) then "p"
  else if Int64.(logand move board_state.w_bishops <> zero) then "b"
  else if Int64.(logand move board_state.w_knights <> zero) then "n"
  else if Int64.(logand move board_state.w_rooks <> zero) then "r"
  else "q"

let process_capture board_state new_move : board_state =
  if board_state.w_turn then
    match piece_at_spot board_state new_move with
    | "q" -> { board_state with b_queen = Int64.zero }
    | "r" ->
        { board_state with b_rooks = Int64.logxor new_move board_state.b_rooks }
    | "n" ->
        {
          board_state with
          b_knights = Int64.logxor new_move board_state.b_knights;
        }
    | "b" ->
        {
          board_state with
          b_bishops = Int64.logxor new_move board_state.b_bishops;
        }
    | "p" ->
        { board_state with b_pawns = Int64.logxor new_move board_state.b_pawns }
    | _ -> failwith "No Valid Capture Detected"
  else
    match piece_at_spot board_state new_move with
    | "q" -> { board_state with w_queen = Int64.zero }
    | "r" ->
        { board_state with b_rooks = Int64.logxor new_move board_state.w_rooks }
    | "n" ->
        {
          board_state with
          b_knights = Int64.logxor new_move board_state.w_knights;
        }
    | "b" ->
        {
          board_state with
          b_bishops = Int64.logxor new_move board_state.w_bishops;
        }
    | "p" ->
        { board_state with b_pawns = Int64.logxor new_move board_state.w_pawns }
    | _ -> failwith "No Valid Capture Detected"

let move_piece_board board_state (move : Int64.t * Int64.t) (piece : string) =
  match move with
  | old_move, new_move -> (
      match piece with
      | "k" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              (old_move, new_move, { board_state with w_king = new_move })
            else
              let temp_board = process_capture board_state new_move in
              (old_move, new_move, { temp_board with w_king = new_move })
          else if Int64.(logand new_move board_state.all_whites = zero) then
            (old_move, new_move, { board_state with b_king = new_move })
          else
            let temp_board = process_capture board_state new_move in
            (old_move, new_move, { temp_board with b_king = new_move })
      | "q" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_queen =
                    board_state.w_queen |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_queen =
                    board_state.w_queen |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_queen =
                  board_state.b_queen |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_queen =
                  board_state.b_queen |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
      | "r" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_rooks =
                    board_state.w_rooks |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_rooks =
                    board_state.w_rooks |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_rooks =
                  board_state.b_rooks |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_rooks =
                  board_state.b_rooks |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
      | "n" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_knights =
                    board_state.w_knights |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_knights =
                    board_state.w_knights |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_knights =
                  board_state.b_knights |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_knights =
                  board_state.b_knights |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
      | "b" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_bishops =
                    board_state.w_bishops |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_bishops =
                    board_state.w_bishops |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_bishops =
                  board_state.b_bishops |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_bishops =
                  board_state.b_bishops |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
      | "p" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_pawns =
                    board_state.w_pawns |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_pawns =
                    board_state.w_pawns |> Int64.logxor old_move
                    |> Int64.logor new_move;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_knights =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_knights =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
              } )
      | _ -> failwith "Piece Not Recognized")

let pseudolegal_moves (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  List.map
    (fun move -> move_piece_board board_state move "k")
    (moves_king board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "q")
      (moves_queen board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "r")
      (moves_rook board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "k")
      (moves_knight board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "b")
      (moves_bishop board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "p")
      (moves_pawn_single board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "p")
      (moves_pawn_double board_state board_state.w_turn)

(* Obtains the square the user would like to move to from their input command
   represented as an Int64.t that corresponds to the bitboard cmd has type
   Command.t -- we assume that the string input is of the form "starting_space
   ending_space", or e.g. "a4 a5"*)
let process_square cmd =
  let raw_cmd = Command.get_command cmd in
  let sq1_letter = String.get raw_cmd 0 in
  let sq1_number = String.get raw_cmd 1 in
  let sq2_letter =
    String.get raw_cmd (String.length raw_cmd - 2) |> Char.lowercase_ascii
  in
  let sq2_number =
    String.get raw_cmd (String.length raw_cmd - 1) |> Char.lowercase_ascii
  in

  ( Int64.shift_left Int64.one
      ((8 * (Char.code sq1_number - 49)) + Char.code sq1_letter - 97),
    Int64.shift_left Int64.one
      ((8 * (Char.code sq2_number - 49)) + Char.code sq2_letter - 97) )

(* Obtains the piece that the user would like to move as a string. cmd has type
   Command.t *)
let process_piece cmd = raise (Failure "Unimplemented")

(* Given board_state, computes all legal moves (and prInt64.ts message about the
   game ending in this step if that is the case), queries and repeatedly waits
   for command corresponding to legal move, then recurses on BoardState
   corresponding to chosen move *)
let rec_func (board_state : board_state) = raise (Failure "Unimplemented")

(** list_range 10 [] returns [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)
let rec list_range range lst =
  if range = 0 then lst else list_range (range - 1) ([ range - 1 ] @ lst)

let rec print_board board_state range =
  let range_as_list = list_range range [] in
  match List.rev range_as_list with
  (* | [] -> Stdlib.print_string "\ndone!" *)
  | [] -> Stdlib.print_string "\n"
  | h :: t ->
      if Int64.logand (Int64.shift_right_logical board_state.b_pawns h) 1L = 1L
      then Stdlib.print_string "P"
      else Stdlib.print_string "";
      if
        Int64.logand (Int64.shift_right_logical board_state.b_bishops h) 1L = 1L
      then Stdlib.print_string "B"
      else Stdlib.print_string "";
      if
        Int64.logand (Int64.shift_right_logical board_state.b_knights h) 1L = 1L
      then Stdlib.print_string "N"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.b_rooks h) 1L = 1L
      then Stdlib.print_string "R"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.b_queen h) 1L = 1L
      then Stdlib.print_string "Q"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.b_king h) 1L = 1L
      then Stdlib.print_string "K"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.w_pawns h) 1L = 1L
      then Stdlib.print_string "p"
      else Stdlib.print_string "";
      if
        Int64.logand (Int64.shift_right_logical board_state.w_bishops h) 1L = 1L
      then Stdlib.print_string "b"
      else Stdlib.print_string "";
      if
        Int64.logand (Int64.shift_right_logical board_state.w_knights h) 1L = 1L
      then Stdlib.print_string "n"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.w_rooks h) 1L = 1L
      then Stdlib.print_string "r"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.w_queen h) 1L = 1L
      then Stdlib.print_string "q"
      else Stdlib.print_string "";
      if Int64.logand (Int64.shift_right_logical board_state.w_king h) 1L = 1L
      then Stdlib.print_string "k"
      else Stdlib.print_string "";
      if h mod 8 = 0 then Stdlib.print_string "\n" else Stdlib.print_string "";

      print_board board_state (range - 1)

let pseudolegal_moves_working (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  List.map
    (fun move -> move_piece_board board_state move "p")
    (moves_pawn_single board_state board_state.w_turn)
  @ List.map
      (fun move -> move_piece_board board_state move "p")
      (moves_pawn_double board_state board_state.w_turn)

let rec print_moves = function
  | [] -> ()
  | (a, b, c) :: t ->
      print_string (Int64.to_string a ^ " " ^ Int64.to_string b ^ "\n");
      print_moves t

let move bs cmd =
  let move_set = all_legal_moves (pseudolegal_moves_working bs) in
  let s, e = process_square cmd in
  let _ = print_string (Int64.to_string s ^ " " ^ Int64.to_string e ^ "\n") in
  let _ = print_moves move_set in
  let valid_move_list =
    List.filter (fun (a, b, _) -> s = a && e = b) move_set
  in
  if List.length valid_move_list < 1 then bs
  else
    let _, _, next_board = List.hd valid_move_list in
    next_board

(* let move bs cmd = let move_set = all_legal_moves (pseudolegal_moves bs) in
   let s, e = process_square cmd in let _, _, mb = List.hd (List.filter (fun (a,
   b, _) -> (s, e) = (a, b)) move_set) in mb *)

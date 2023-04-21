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

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                  ALL LEGAL MOVES                     *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

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

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                   KING MOVEMENT                      *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let moves_king (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

let moves_kingcastle (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                   QUEEN MOVEMENT                     *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let moves_queen (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                    ROOK MOVEMENT                     *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let rec exponent (num : Int64.t) (exp : Int64.t) =
  if exp = 0L then 1L
  else if exp = 1L then num
  else exponent (Int64.mul num 2L) (Int64.sub exp 1L)

let rec slider_loc_helper (num : Int64.t) (lst : Int64.t list) (acc : int) =
  if num = 0L then lst
  else
    let new_num = Int64.shift_right_logical num 1 in
    if Int64.logand num 1L = 1L then
      slider_loc_helper new_num (exponent 2L (Int64.of_int acc) :: lst) (acc + 1)
    else slider_loc_helper new_num lst (acc + 1)

let slider_loc (num : Int64.t) = slider_loc_helper num [] 0

let rook_helper (board_state : board_state) (new_r : Int64.t) (stay_r : Int64.t)
    =
  if board_state.w_turn then
    if Int64.logand new_r board_state.all_whites = 0L && new_r <> 0L then
      [ Int64.logor new_r stay_r ]
    else []
  else if Int64.logand new_r board_state.all_blacks = 0L && new_r <> 0L then
    [ Int64.logor new_r stay_r ]
  else []

(** TODO: ROOK ONLY STOPS AT WHITE -> BLACK CAPTURE NOT OTHER WAY AROUND *)
let rec rook_up (board_state : board_state) (move_r : Int64.t)
    (stay_r : Int64.t) (bs_lst : Int64.t list) : Int64.t list =
  let new_up_r = Int64.shift_left move_r 8 in
  if Int64.logand new_up_r board_state.all_whites > 0L || new_up_r = 0L then
    bs_lst
  else if Int64.logand new_up_r board_state.all_blacks > 0L then
    let new_bs_lst = rook_helper board_state new_up_r stay_r in
    rook_up board_state 0L stay_r (new_bs_lst @ bs_lst)
  else
    let new_bs_lst = rook_helper board_state new_up_r stay_r in
    rook_up board_state new_up_r stay_r (new_bs_lst @ bs_lst)

(** TODO: ROOK ONLY STOPS AT WHITE -> BLACK CAPTURE NOT OTHER WAY AROUND *)
let rec rook_down (board_state : board_state) (move_r : Int64.t)
    (stay_r : Int64.t) (bs_lst : Int64.t list) : Int64.t list =
  let new_down_r = Int64.shift_right move_r 8 in
  if Int64.logand new_down_r board_state.all_whites > 0L || new_down_r = 0L then
    bs_lst
  else if Int64.logand new_down_r board_state.all_blacks > 0L then
    let new_bs_lst = rook_helper board_state new_down_r stay_r in
    rook_down board_state 0L stay_r (new_bs_lst @ bs_lst)
  else
    let new_bs_lst = rook_helper board_state new_down_r stay_r in
    rook_down board_state new_down_r stay_r (new_bs_lst @ bs_lst)

(** TODO: NEED TO MASK SO DOESN'T WRAP AROUND *)
let rec rook_left (board_state : board_state) (move_r : Int64.t)
    (stay_r : Int64.t) (bs_lst : Int64.t list) : Int64.t list =
  let new_left_r = Int64.shift_left move_r 1 in
  if Int64.logand new_left_r board_state.all_whites > 0L || new_left_r = 0L then
    bs_lst
  else if Int64.logand new_left_r board_state.all_blacks > 0L then
    let new_bs_lst = rook_helper board_state new_left_r stay_r in
    rook_left board_state 0L stay_r (new_bs_lst @ bs_lst)
  else
    let new_bs_lst = rook_helper board_state new_left_r stay_r in
    rook_left board_state new_left_r stay_r (new_bs_lst @ bs_lst)

(** TODO: NEED TO MASK SO DOESN'T WRAP AROUND *)
let rec rook_right (board_state : board_state) (move_r : Int64.t)
    (stay_r : Int64.t) (bs_lst : Int64.t list) : Int64.t list =
  let new_right_r = Int64.shift_left move_r 1 in
  if Int64.logand new_right_r board_state.all_whites > 0L || new_right_r = 0L
  then bs_lst
  else if Int64.logand new_right_r board_state.all_blacks > 0L then
    let new_bs_lst = rook_helper board_state new_right_r stay_r in
    rook_right board_state 0L stay_r (new_bs_lst @ bs_lst)
  else
    let new_bs_lst = rook_helper board_state new_right_r stay_r in
    rook_right board_state new_right_r stay_r (new_bs_lst @ bs_lst)

let rook_all_moves (board_state : board_state) (rooks : Int64.t) =
  match slider_loc rooks with
  | r1 :: r2 :: t ->
      rook_up board_state r1 r2 []
      @ rook_down board_state r1 r2 []
      @ rook_left board_state r1 r2 []
      @ rook_right board_state r1 r2 []
      @ rook_up board_state r2 r1 []
      @ rook_down board_state r2 r1 []
      @ rook_left board_state r2 r1 []
      @ rook_right board_state r2 r1 []
  | _ -> []

let rec combine_all_rooks (board_state : board_state)
    (rook_moves : Int64.t list) lst =
  if board_state.w_turn then
    match rook_moves with
    | [] -> lst
    | h :: t -> combine_all_rooks board_state t ((board_state.w_rooks, h) :: lst)
  else
    match rook_moves with
    | [] -> lst
    | h :: t -> combine_all_rooks board_state t ((board_state.b_rooks, h) :: lst)

let moves_rook (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  if board_state.w_turn then
    combine_all_rooks board_state
      (rook_all_moves board_state board_state.w_rooks)
      []
  else
    combine_all_rooks board_state
      (rook_all_moves board_state board_state.b_rooks)
      []

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                   KNIGHT MOVEMENT                    *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let moves_knight (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                  BISHOP MOVEMENT                     *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let moves_bishop (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  raise (Failure "Unimplemented")

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*               GENERAL PAWN MOVEMENTS                 *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

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
  let filter =
    if white_turn then Int64.shift_right_logical white_first_files 8
    else Int64.shift_left black_first_files 8
  in
  let forward_moves = _moves_pawn_forward board_state white_turn filter in
  let capture_moves = _moves_pawn_cap board_state white_turn filter in
  List.append forward_moves capture_moves

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                SPECIAL PAWN MOVES                    *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

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
  let filter =
    if white_turn then Int64.shift_right_logical white_last_file 8
    else Int64.shift_left black_last_file 8
  in
  _moves_pawn_forward board_state white_turn filter

let moves_promote_cap (board_state : board_state) (white_turn : bool) :
    (Int64.t * Int64.t) list =
  let filter =
    if white_turn then Int64.shift_right_logical white_last_file 8
    else Int64.shift_left black_last_file 8
  in
  _moves_pawn_cap board_state white_turn filter

let list_or (bitmaps : (Int64.t * Int64.t) list) : Int64.t =
  let bitmaps = List.map (fun tup -> fst tup) bitmaps in
  List.fold_right Int64.logor bitmaps Int64.minus_one

(********************************************************)
(*                                                       *)
(*                                                       *)
(*                                                       *)
(*                   ENEMY ATTACKS                       *)
(*                                                       *)
(*                                                       *)
(*                                                       *)
(*********************************************************)

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

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                PSEUDOLEGAL MOVES                     *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

let piece_at_spot board_state (move : Int64.t) : string =
  if board_state.w_turn then
    if Int64.(logand move board_state.b_pawns <> zero) then "p"
    else if Int64.(logand move board_state.b_bishops <> zero) then "b"
    else if Int64.(logand move board_state.b_knights <> zero) then "n"
    else if Int64.(logand move board_state.b_rooks <> zero) then "r"
    else if Int64.(logand move board_state.b_king <> zero) then "k"
    else "q"
  else if Int64.(logand move board_state.w_pawns <> zero) then "p"
  else if Int64.(logand move board_state.w_bishops <> zero) then "b"
  else if Int64.(logand move board_state.w_knights <> zero) then "n"
  else if Int64.(logand move board_state.w_rooks <> zero) then "r"
  else if Int64.(logand move board_state.w_king <> zero) then "k"
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
    (* This king pattern match is only used to process checks. It is not
       actually A capturing move. This is called by detect_check *)
    | "k" -> { board_state with in_check_b = true }
    | _ -> failwith "No Valid Capture Detected"
  else
    match piece_at_spot board_state new_move with
    | "q" -> { board_state with w_queen = Int64.zero }
    | "r" ->
        { board_state with w_rooks = Int64.logxor new_move board_state.w_rooks }
    | "n" ->
        {
          board_state with
          w_knights = Int64.logxor new_move board_state.w_knights;
        }
    | "b" ->
        {
          board_state with
          w_bishops = Int64.logxor new_move board_state.w_bishops;
        }
    | "p" ->
        { board_state with w_pawns = Int64.logxor new_move board_state.w_pawns }
    (* This king pattern match is only used to process checks. It is not
       actually A capturing move. This is called by detect_check *)
    | "k" -> { board_state with in_check_w = true }
    | _ -> failwith "No Valid Capture Detected"

(* Given a move and a piece, recomputes every variable that is affected to match
   the new board state (includes processing captures and new locations) *)
let move_piece_board board_state (move : Int64.t * Int64.t) (piece : string) =
  match move with
  | old_move, new_move -> (
      match piece with
      | "k" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_king = new_move;
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
            else
              let temp_board = process_capture board_state new_move in
              ( old_move,
                new_move,
                {
                  temp_board with
                  w_king = new_move;
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_king = new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_king = new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
              } )
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_queen =
                  board_state.b_queen |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_rooks =
                  board_state.b_rooks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_knights =
                  board_state.b_knights |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_bishops =
                  board_state.b_bishops |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
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
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
              } )
      | "p_s" ->
          if board_state.w_turn then
            if Int64.(logand new_move board_state.all_blacks = zero) then
              ( old_move,
                new_move,
                {
                  board_state with
                  w_pawns =
                    board_state.w_pawns |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
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
                  all_whites =
                    board_state.all_whites |> Int64.logxor old_move
                    |> Int64.logor new_move;
                  ep = Int64.zero;
                } )
          else if Int64.(logand new_move board_state.all_whites = zero) then
            ( old_move,
              new_move,
              {
                board_state with
                b_pawns =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
              } )
          else
            let temp_board = process_capture board_state new_move in
            ( old_move,
              new_move,
              {
                temp_board with
                b_pawns =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = Int64.zero;
              } )
      | "p_d" ->
          if board_state.w_turn then
            ( old_move,
              new_move,
              {
                board_state with
                w_pawns =
                  board_state.w_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_whites =
                  board_state.all_whites |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = new_move;
              } )
          else
            ( old_move,
              new_move,
              {
                board_state with
                b_pawns =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                ep = new_move;
              } )
      | "p_ep" ->
          if board_state.w_turn then
            ( old_move,
              new_move,
              {
                board_state with
                w_pawns =
                  board_state.w_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_whites =
                  board_state.all_whites |> Int64.logxor old_move
                  |> Int64.logor new_move;
                b_pawns =
                  board_state.b_pawns
                  |> Int64.logxor (Int64.shift_right_logical new_move 8);
                all_blacks =
                  board_state.all_blacks
                  |> Int64.logxor (Int64.shift_right_logical new_move 8);
                ep = Int64.zero;
              } )
          else
            ( old_move,
              new_move,
              {
                board_state with
                b_pawns =
                  board_state.b_pawns |> Int64.logxor old_move
                  |> Int64.logor new_move;
                all_blacks =
                  board_state.all_blacks |> Int64.logxor old_move
                  |> Int64.logor new_move;
                w_pawns =
                  board_state.w_pawns
                  |> Int64.logxor (Int64.shift_right_logical new_move 8);
                all_whites =
                  board_state.all_whites
                  |> Int64.logxor (Int64.shift_right_logical new_move 8);
                ep = Int64.zero;
              } )
      | _ -> failwith "Piece Not Recognized")

let piece_movement = function
  | "p_s" | "p_d" | "p_ep" -> moves_pawn_single
  | "q" -> moves_queen
  | "r" -> moves_rook
  | "n" -> moves_knight
  | "b" -> moves_bishop
  | _ -> failwith "Bad Move Call in get_piece_move"

let detect_check board_state move piece =
  if board_state.w_turn then
    match piece with
    | s ->
        let move_list =
          List.map
            (fun move -> move_piece_board board_state move s)
            ((piece_movement s) board_state board_state.w_turn)
        in
        let in_check_lst =
          List.filter (fun (_, _, bs) -> bs.in_check_b) move_list
        in
        if List.length in_check_lst = 0 then
          { board_state with in_check_b = false }
        else
          let _ = print_endline "Black in Check!" in
          { board_state with in_check_b = true }
  else
    match piece with
    | s ->
        let move_list =
          List.map
            (fun move -> move_piece_board board_state move s)
            ((piece_movement s) board_state board_state.w_turn)
        in
        let in_check_lst =
          List.filter (fun (_, _, bs) -> bs.in_check_w) move_list
        in
        if List.length in_check_lst = 0 then
          { board_state with in_check_w = false }
        else
          let _ = print_endline "White in Check!" in
          { board_state with in_check_w = true }

let rec query_promo () =
  print_endline
    "\n\
     Select the piece for promotion:\n\
    \ 
\n\n\n\
    \  Type q for queen, r for rook, b for bishop, and n for night\n";
  match String.trim (read_line ()) with
  | exception End_of_file -> "ivd"
  | m -> (
      match m with
      | "q" -> "q"
      | "r" -> "r"
      | "b" -> "b"
      | "n" -> "n"
      | _ ->
          print_endline "\nInvalid promotion! Try again!\n";
          query_promo ())

let promo_move move_list w_turn =
  if w_turn then
    match query_promo () with
    | "q" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.w_queen nm = Int64.one)
          move_list
    | "r" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.w_rooks nm = Int64.one)
          move_list
    | "b" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.w_bishops nm = Int64.one)
          move_list
    | "n" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.w_knights nm = Int64.one)
          move_list
    | _ -> failwith "Invalid move entered for promotion!"
  else
    match query_promo () with
    | "q" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.b_queen nm = Int64.one)
          move_list
    | "r" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.b_rooks nm = Int64.one)
          move_list
    | "b" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.b_bishops nm = Int64.one)
          move_list
    | "n" ->
        List.filter
          (fun (om, nm, bs) -> Int64.logand bs.b_knights nm = Int64.one)
          move_list
    | _ -> failwith "Invalid move entered for promotion!"

let gen_promos board_state =
  (*let _ = print_int (List.length (moves_promote_cap board_state
    board_state.w_turn @ moves_promote_no_cap board_state board_state.w_turn))
    in*)
  let promos =
    (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_s")
         (moves_promote_cap board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_s")) lst)
    @
    let lst =
      List.map
        (fun move -> move_piece_board board_state move "p_s")
        (moves_promote_no_cap board_state board_state.w_turn)
    in
    List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_s")) lst
  in
  let promos =
    if board_state.w_turn then
      List.map
        (fun (om, nm, bs) ->
          ( om,
            nm,
            {
              bs with
              w_pawns = Int64.logxor nm bs.w_pawns;
              w_queen = Int64.logor nm bs.w_queen;
            } ))
        promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                w_pawns = Int64.logxor nm bs.w_pawns;
                w_rooks = Int64.logor nm bs.w_rooks;
              } ))
          promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                w_pawns = Int64.logxor nm bs.w_pawns;
                w_bishops = Int64.logor nm bs.w_bishops;
              } ))
          promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                w_pawns = Int64.logxor nm bs.w_pawns;
                w_knights = Int64.logor nm bs.w_knights;
              } ))
          promos
    else
      List.map
        (fun (om, nm, bs) ->
          ( om,
            nm,
            {
              bs with
              b_pawns = Int64.logxor nm bs.b_pawns;
              b_queen = Int64.logor nm bs.b_queen;
            } ))
        promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                b_pawns = Int64.logxor nm bs.b_pawns;
                b_rooks = Int64.logor nm bs.b_rooks;
              } ))
          promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                b_pawns = Int64.logxor nm bs.b_pawns;
                b_bishops = Int64.logor nm bs.b_bishops;
              } ))
          promos
      @ List.map
          (fun (om, nm, bs) ->
            ( om,
              nm,
              {
                bs with
                b_pawns = Int64.logxor nm bs.b_pawns;
                b_knights = Int64.logor nm bs.b_knights;
              } ))
          promos
  in
  List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_s")) promos

let is_promo om nm bs nb =
  let promo_moves = List.filter (fun (_, _, bs) -> bs = nb) (gen_promos bs) in
  not (List.length promo_moves = 0)

let pseudolegal_moves (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  List.map
    (fun move -> move_piece_board board_state move "k")
    (moves_king board_state board_state.w_turn)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "q")
         (moves_queen board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "q")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "r")
         (moves_rook board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "r")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "n")
         (moves_knight board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "n")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "b")
         (moves_bishop board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "b")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_s")
         (moves_pawn_single board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_s")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_d")
         (moves_pawn_double board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_d")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_ep")
         (moves_ep_captures board_state board_state.w_turn)
     in
     List.map
       (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_ep"))
       lst)
  @ gen_promos board_state
  |> List.map (fun (a, b, c) -> (a, b, { c with w_turn = not c.w_turn }))

let pseudolegal_moves_pawns (board_state : board_state) :
    (Int64.t * Int64.t * board_state) list =
  let lst =
    List.map
      (fun move -> move_piece_board board_state move "p_s")
      (moves_pawn_single board_state board_state.w_turn)
  in
  List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_s")) lst
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_d")
         (moves_pawn_double board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_d")) lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "p_ep")
         (moves_ep_captures board_state board_state.w_turn)
     in
     List.map
       (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "p_ep"))
       lst)
  @ (let lst =
       List.map
         (fun move -> move_piece_board board_state move "r")
         (moves_rook board_state board_state.w_turn)
     in
     List.map (fun (om, nm, bs) -> (om, nm, detect_check bs (om, nm) "r")) lst)
  @ gen_promos board_state
  (*@ (let lst = List.map (fun move -> move_piece_board board_state move "p_s")
    (moves_promote_cap board_state board_state.w_turn) in List.map (fun (om, nm,
    bs) -> (om, nm, detect_check bs (om, nm) "p_s")) lst) @ (let lst = List.map
    (fun move -> move_piece_board board_state move "p_s") (moves_promote_no_cap
    board_state board_state.w_turn) in List.map (fun (om, nm, bs) -> (om, nm,
    detect_check bs (om, nm) "p_s")) lst)*)
  |> List.map (fun (a, b, c) -> (a, b, { c with w_turn = not c.w_turn }))

(********************************************************)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(*                  SELECTING MOVE                      *)
(*                Processing commands                   *)
(*                                                      *)
(*                                                      *)
(*                                                      *)
(********************************************************)

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
      ((8 * (Char.code sq1_number - 49)) + (104 - Char.code sq1_letter)),
    Int64.shift_left Int64.one
      ((8 * (Char.code sq2_number - 49)) + (104 - Char.code sq2_letter)) )

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

let rec print_board_helper board_state range =
  let range_as_list = list_range range [] in
  Stdlib.print_string "";
  match List.rev range_as_list with
  (* | [] -> Stdlib.print_string "\ndone!" *)
  | [] -> Stdlib.print_string "\n"
  | h :: t ->
      if Int64.logand (Int64.shift_right_logical board_state.b_pawns h) 1L = 1L
      then Stdlib.print_string "♙"
      else if
        Int64.logand (Int64.shift_right_logical board_state.b_bishops h) 1L = 1L
      then Stdlib.print_string "♗"
      else if
        Int64.logand (Int64.shift_right_logical board_state.b_knights h) 1L = 1L
      then Stdlib.print_string "♘"
      else if
        Int64.logand (Int64.shift_right_logical board_state.b_rooks h) 1L = 1L
      then Stdlib.print_string "♖"
      else if
        Int64.logand (Int64.shift_right_logical board_state.b_queen h) 1L = 1L
      then Stdlib.print_string "♕"
      else if
        Int64.logand (Int64.shift_right_logical board_state.b_king h) 1L = 1L
      then Stdlib.print_string "♔"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_pawns h) 1L = 1L
      then Stdlib.print_string "♟︎"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_bishops h) 1L = 1L
      then Stdlib.print_string "♝"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_knights h) 1L = 1L
      then Stdlib.print_string "♞"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_rooks h) 1L = 1L
      then Stdlib.print_string "♜"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_queen h) 1L = 1L
      then Stdlib.print_string "♛"
      else if
        Int64.logand (Int64.shift_right_logical board_state.w_king h) 1L = 1L
      then Stdlib.print_string "♚"
      else Stdlib.print_string ".";
      if h = 0 then
        Stdlib.print_string
          "\n   |_________________ \n\n      a b c d e f g h \n"
      else if h mod 8 = 0 then
        Stdlib.print_string ("\n" ^ Int.to_string (h / 8) ^ "  |  ")
      else Stdlib.print_string " ";

      print_board_helper board_state (range - 1)

let print_board board_state =
  Stdlib.print_string "8  |  ";
  print_board_helper board_state 64

let rec print_moves = function
  | [] -> ()
  | (a, b, c) :: t ->
      print_string (Int64.to_string a ^ " " ^ Int64.to_string b ^ "\n");
      print_moves t

let move bs cmd =
  let move_set = all_legal_moves (pseudolegal_moves_pawns bs) in

  let s, e = process_square cmd in
  (*let _ = print_string (Int64.to_string s ^ " " ^ Int64.to_string e ^ "\n") in *)
  (*let _ = print_moves move_set in*)
  (* let _ = print_endline (Int64.to_string bs.ep) in let _ = print_endline
     (string_of_int (List.length (moves_ep_captures bs true))) in let _ =
     List.map (fun (a,b) -> print_endline ((Int64.to_string a) ^ " " ^
     (Int64.to_string b))) (moves_ep_captures bs true) in*)
  let valid_move_list =
    List.filter (fun (a, b, _) -> s = a && e = b) move_set
  in
  if List.length valid_move_list < 1 then bs
  else
    let om, nm, next_board = List.hd valid_move_list in
    (*let _ = print_endline (string_of_bool (is_promo om nm bs next_board))
      in *)
    (*let _, _, board = if List.length (gen_promos bs) = 0 then (Int64.zero,
      Int64.zero, init_chess) else List.hd (gen_promos bs) in let _ =
      print_board board in*)
    (*let _ = print_endline (string_of_int (List.length (gen_promos bs))) in*)
    if is_promo om nm bs next_board then
      let _, _, nb_promo = List.hd (promo_move valid_move_list bs.w_turn) in
      nb_promo
    else next_board

(* let move bs cmd = let move_set = all_legal_moves (pseudolegal_moves bs) in
   let s, e = process_square cmd in let _, _, mb = List.hd (List.filter (fun (a,
   b, _) -> (s, e) = (a, b)) move_set) in mb *)
let get_val board_state = board_state.b_knights
let get_turn board_state = if board_state.w_turn then "white" else "black"

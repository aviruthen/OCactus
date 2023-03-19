type board_state
(** abstract type of board *)

val init_chess : board_state
(** Initializes a chess board before any moves have been made *)

val pseudolegal_moves : board_state -> (Int64.t * Int64.t * board_state) list
(** Given board_state, generate all pseudolegal moves (piece isn't blocked and
    stays on board) represented by from, to positions and new BoardState; call
    helpers legal_moves_<piece type> as needed. Disregards checks which may
    invalidate moves. *)

val all_legal_moves :
  (Int64.t * Int64.t * board_state) list ->
  (Int64.t * Int64.t * board_state) list
(** Given the pseudolegal moves provided by pseudolegal_moves function, returns
    a pruned version of the list which accounts for checks that may invalidate
    moves. Requires that board_moves is a list containing all possible legal
    moves except for checks *)

val moves_king : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for king (true for white,
    false for black) represented by (from position, to position); does not
    include castling *)

val moves_kingcastle : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal castlings (as long as no piece
    blocking ) represented by (from position, to position) indicating king's
    position (true for white, false for black); is list because 2 possible moves *)

val moves_queen : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for queen (true for white,
    false for black) represented by (from position, to position) *)

val moves_rook : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for rooks (true for white,
    false for black) represented by (from position, to position) of rook; does
    not include castling *)

val moves_knight : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for knights (true for
    white, false for black) represented by (from position, to position)*)

val moves_bishop : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for bishops (true for
    white, false for black) represented by (from position, to position)*)

val moves_pawn_double : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves for pawn 'double hops'
    (true for white, false for black) represented by (from position, to
    position)*)

val moves_pawn_single : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal moves (including regular and EP
    captures) for pawn single move forward except for promotions (true for
    white, false for black) *)

val moves_pawn_attacks : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all squares attacked by pawns represented by
    (position of attacking pawn, position of attacked square) (true for white,
    false for black) *)

val moves_promote_no_cap : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal promotions (true for white,
    false for black) represented by (from position, to position) *)

val moves_promote_cap : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal promotions with capture (true
    for white, false for black) represented by (from position, to position) *)

val enemy_attacks : board_state -> Int64.t
(** Bitboard representation of all squares that are attacked by enemies (if
    board_state.w_turn, then squares attacked by black, and vice versa); used
    for checking move and castling legality *)

val process_square : Command.t -> Int64.t * Int64.t
(** Obtains the starting square the user would like to move from their input 
    command and the ending square they would like to move to, represented as a 
    (Int64.t, Int64.t) double that corresponds to two bit boards. First argument
    has type Command.t *)

val process_piece : _ -> string
(** Obtains the piece that the user would like to move as a string First
    argument has type Command.t *)

val rec_func : board_state -> unit
(** Given board_state, computes all legal moves (and prints message about the
    game ending in this step if that is the case), queries and repeatedly waits
    for command corresponding to legal move, then recurses on BoardState
    corresponding to chosen move *)

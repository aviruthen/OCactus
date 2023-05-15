(** This is the represntation of the chess board as well as the bulk of the game
    functionality. It has the board state representation, the possible moves of
    all chess pieces, processing attacks, check, checkmate, promotion,
    en-passant and more. It also accounts for both white and black pieces. *)

type board_state
(** abstract type of board *)

val init_chess : board_state
(** Initializes a chess board before any moves have been made *)

val get_val : board_state -> Int64.t
(** Converts board state to bitboard*)

val pseudolegal_moves : board_state -> (Int64.t * Int64.t * board_state) list
(** Given board_state, generate all pseudolegal moves (piece isn't blocked and
    stays on board) represented by from, to positions and new BoardState; call
    helpers legal_moves_<piece type> as needed. Disregards checks which may
    invalidate moves. *)

val pseudolegal_moves_pawns :
  board_state -> (Int64.t * Int64.t * board_state) list
(** Finds all pseudolegal pawn moves*)

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
(** Given board_state, generate all LEGAL castlings represented by (from
    position, to position) indicating king's position (true for white, false for
    black). Notably: can't castle out of check, can't castle into check, and
    can't castle thru check *)

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
(** Given board_state, generate all pseudolegal moves (excluding EP captures)
    for pawn single move forward or diagonal except for promotions (true for
    white, false for black) represented by (from position, to position) *)

val moves_ep_captures : board_state -> bool -> (Int64.t * Int64.t) list
(** Given board_state, generate all pseudolegal EP captures (true for white,
    false for black) represented by (from position, to position) *)

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

val print_board : board_state -> unit
(** print chess board*)

val move : board_state -> Command.t -> board_state
(** if move is in the valid list then do so (eat other piece if necessary) ...
    if checkmate then finish *)

val get_turn : board_state -> string
(** gets the current mover of the board ("white" for white's turn, "black" for
    black's turn)*)

val get_fifty : board_state -> int
(** gets the number of moves played towards fifty-move rule *)

val get_prev_boards : board_state -> board_state list
(** gets all the previous board states (including) itself that have been played
    used to determine repeated-move drawing *)

val cmp_boards : board_state -> board_state -> bool
(** Compares if the pieces on two boards are in the same place used to determine
    repeated-move drawing *)

val in_check : board_state -> bool
(** Determines if player is in check *)

val cmp_boards : board_state -> board_state -> bool
(** Compares two boards to see if pieces are placed the same way on board *)

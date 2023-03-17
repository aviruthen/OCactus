(** Tracks the state of the chess game. This includes (but is not limited to)
    the locations of each of the pieces, the number of moves in the game, a data
    structure that stores the moves that have been played, a function that
    determines if there is a check or checkmate, a function that determines if
    En Passant can be played, and a way of tracking the pieces that are on (or
    off) the board *)

type board_state
(** abstract type of board *)


val pseudolegal_moves : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves represented by from, to positions 
   and new BoardState; call helpers legal_moves_<piece type>  as needed. 
   Disregards checks which may invalidate moves. *)

val all_legal_moves : (int * int * board_state) list -> (int * int * board_state) list
(** Given the pseudolegal moves provided by pseudolegal_moves function, returns a pruned version 
   of the list which accounts for checks that may invalidate moves. Requires that board_moves is 
   a list containing all possible legal moves except for checks *) 

val legal_moves_king : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for king represented by from, to positions 
   and new BoardState; does not include castling *)

val legal_moves_kingcastle : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for king castling represented by from, to 
   positions and new BoardState; is list because 2 possible moves *)

val legal_moves_queen : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for queen represented by from, to positions 
   and new BoardState *)

val legal_moves_rook : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for rook represented by from, to positions 
   and new board_state *)

val legal_moves_knight : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for knight represented by from, to positions 
   and new board_state *)

val legal_moves_bishop : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for bishop represented by from, to positions 
   and new board_state *)

val legal_moves_pawn_double : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for pawn ‘double hops’ represented 
   by from, to positions and new BoardState; make sure to set EP bool to true for new BoardState *)

val legal_moves_pawn_single : board_state -> (int * int * board_state) list
(** Given board_state, generate all possible moves for pawn moves (except for ‘double hops’) *)

val enemy_attacks : board_state -> int
(** Bitboard representation of all squares that are attacked by enemy square; 
   used for checking move and castling legality; call enemy_attacks_<piece type> 
   as needed and logical or them all together *)

val enemy_attacks_queen : board_state -> int
(* Bitboard representation of all squares that are attacked by enemy queen; 
   call enemy_attacks_diagonal and enemy_attacks_straight *)

val enemy_attacks_rooks : board_state -> int
(** Bitboard representation of all squares that are attacked by both enemy rooks; 
   call enemy_attacks_straight *)

val enemy_attacks_knights : board_state -> int
(** Bitboard representation of all squares that are attacked by both enemy knights *)

val enemy_attacks_bishops : board_state -> int
(** Bitboard representation of all squares that are attacked by both enemy bishops; 
   call enemy_attacks_diagonal *)

val enemy_attacks_pawns : board_state -> int
(** Bitboard representation of all squares that are attacked by all enemy pawns *)

val enemy_attacks_diagonal : board_state -> int -> int
(** Bitboard representation of all squares that are attacked by piece 
   at square that slides diagonally *)

val enemy_attacks_straight : board_state -> int -> int
(** Bitboard representation of all squares that are attacked by piece at square that 
   slide along rows/files *)

val move : (int * int * board_state) list -> int -> string -> board_state
(* Given all possible boardstates which have been computed in the board_list argument, and 
   given the piece that user would like to move along with the square the user would like to move to,
    this function will return a new boardstate that incorporates the user move. 

    If the move the user suggests is not a valid piece move, raise IllegalMove. 
    If the user tries a valid move, but they were previously in check which invalidates their move, 
    then raise MoveInCheck. 
    If no BoardState is found that corresponds to the move the user intended, raise MoveNotFound *)

val process_square : _ -> int
(** Obtains the square the user would like to move to from their input command represented as 
   an int that corresponds to the bitboard
   First argument has type Command.t *)

val process_piece : _ -> string
(** Obtains the piece that the user would like to move as a string 
First argument has type Command.t *)

val rec_func : board_state -> unit
(** Given board_state, computes all legal moves (and prints message about the game ending 
   in this step if that is the case), queries and repeatedly waits for command corresponding 
   to legal move, then recurses on BoardState corresponding to chosen move *)
(** Tracks the state of the chess game. This includes (but is not limited to)
    the locations of each of the pieces, the number of moves in the game, a data
    structure that stores the moves that have been played, a function that
    determines if there is a check or checkmate, a function that determines if
    En Passant can be played, and a way of tracking the pieces that are on (or
    off) the board *)

type t
(** abstract type of board *)

val location : Piece.t -> (int * int * string) list
(** location returns the squares that piece is located on the board. For
    example, a rook on g4 would be labelled as [(7,4,"rook")]. *)

val init_state : t
(** where we initialize the board *)

val checkmate : t -> bool
(** checkmate move the king in all possible ways and see if it's being attacked *)

val check : t -> bool
(** see if is in check (maybe an arugment is white/back indicating player) *)

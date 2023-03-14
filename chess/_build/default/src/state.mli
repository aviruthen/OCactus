(** Tracks the state of the chess game. This includes (but is not limited to)
    the locations of each of the pieces, the number of moves in the game, a data
    structure that stores the moves that have been played, a function that
    determines if there is a check or checkmate, a function that determines if
    En Passant can be played, and a way of tracking the pieces that are on (or
    off) the board *)

type board_state
(** abstract type of board *)

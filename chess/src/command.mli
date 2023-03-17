type t

(** Given a user or computer input, command.ml translates the input into
    information that can be used to generate a new move in the game. Command
    should check to determine if the move is a valid move, and also should
    correctly process moves with captures, checks, or checkmates (e.g. Qc3+ is
    the same as Qc3 should be processed equally) *)

val parse : string -> string list
(** parse - parses the command so that we get the piece and it's desired move
    (should catch issues of bad command) *)

val valid_actions : _ -> _
(** where given a piece and a location, this will return a list of locations
    where the piece can go - can eat other pieces so that would be a valid move *)

val move : _ -> _
(** if move is in the valid list then do so (eat other piece if necessary) ...
    if checkmate then finish *)

(** Given a user or computer input, command.ml translates the input into
    information that can be used to generate a new move in the game. Command
    should check to determine if the move is a valid move, and also should
    correctly process moves with captures, checks, or checkmates (e.g. Qc3+ is
    the same as Qc3 should be processed equally) *)

type t
(** Abstract type of command *)

val parse : string -> t
(** Parses the command line argument so that we get the piece 
    and it's desired move (should catch issues of bad command) *)

val get_command : t -> string
(** Gets the string version of the command *)

exception MalformedInput
(** Represents all malformed inputs *)

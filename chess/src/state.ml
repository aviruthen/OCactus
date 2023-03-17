type board_state = {b_pawns : int; b_bishops : int; b_knights : int; b_rooks: int;
b_queen: int; b_king : int; w_pawns: int; w_bishops : int; w_knights : int;
  w_rooks : int; w_queen: int; w_king : int; all_whites: int; all_blacks: int; 
  all_pieces : int ; ep: int; b_castle_l: bool; b_castle_r: bool; 
  w_castle_l: bool; w_castle_r: bool; w_turn: bool; in_check_w: bool; 
  in_check_b: bool}

(* Given board_state, generate all possible moves represented by from, to positions 
   and new BoardState; call helpers legal_moves_<piece type>  as needed. 
   Disregards checks which may invalidate moves. *)
let pseudolegal_moves (board_state: board_state) = raise (Failure "Unimplemented")


(* Given the pseudolegal moves provided by pseudolegal_moves function, returns a pruned version 
   of the list which accounts for checks that may invalidate moves. Requires that board_moves is 
   a list containing all possible legal moves except for checks *) 
let all_legal_moves (board_moves : (int * int * board_state) list) =
List.map (fun (a,b,c) -> (not c.w_turn <>  c.in_check_w) || (c.w_turn <>  c.in_check_b)) board_moves


(* Given board_state, generate all possible moves for king represented by from, to positions 
   and new BoardState; does not include castling *)
let legal_moves_king (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for king castling represented by from, to 
   positions and new BoardState; is list because 2 possible moves *)
let legal_moves_kingcastle (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for queen represented by from, to positions 
   and new BoardState *)
let legal_moves_queen (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for rook represented by from, to positions 
   and new board_state *)
let legal_moves_rook (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for knight represented by from, to positions 
   and new board_state *)
let legal_moves_knight (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for bishop represented by from, to positions 
   and new board_state *)
let legal_moves_bishop (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for pawn ‘double hops’ represented 
   by from, to positions and new BoardState; make sure to set EP bool to true for new BoardState *)
let legal_moves_pawn_double (board_state: board_state) = raise (Failure "Unimplemented")


(* Given board_state, generate all possible moves for pawn moves (except for ‘double hops’) *)
let legal_moves_pawn_single (board_state: board_state) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by enemy square; 
   used for checking move and castling legality; call enemy_attacks_<piece type> 
   as needed and logical or them all together *)
let enemy_attacks (board_state: board_state) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by enemy queen; 
   call enemy_attacks_diagonal and enemy_attacks_straight *)
let enemy_attacks_queen (board_state: board_state) = raise (Failure "Unimplemented")



(* Bitboard representation of all squares that are attacked by both enemy rooks; 
   call enemy_attacks_straight *)
let enemy_attacks_rooks (board_state: board_state) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by both enemy knights *)
let enemy_attacks_knights (board_state: board_state) = raise (Failure "Unimplemented")

(* Bitboard representation of all squares that are attacked by both enemy bishops; 
   call enemy_attacks_diagonal *)
let enemy_attacks_bishops (board_state: board_state) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by all enemy pawns *)
let enemy_attacks_pawns (board_state: board_state) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by piece 
   at square that slides diagonally *)
let enemy_attacks_diagonal (board_state: board_state) (square : int) = raise (Failure "Unimplemented")


(* Bitboard representation of all squares that are attacked by piece at square that 
   slide along rows/files *)
let enemy_attacks_straight (board_state: board_state) (square : int) = raise (Failure "Unimplemented")

(* Given all possible boardstates which have been computed in the board_list argument, and 
   given the piece that user would like to move along with the square the user would like to move to,
    this function will return a new boardstate that incorporates the user move. 

    If the move the user suggests is not a valid piece move, raise IllegalMove. 
    If the user tries a valid move, but they were previously in check which invalidates their move, 
    then raise MoveInCheck. 
    If no BoardState is found that corresponds to the move the user intended, raise MoveNotFound *) 
let move (board_list : (int * int * board_state) list) (piece : string) (square : int) = 
                                                        raise (Failure "Unimplemented")


(* Obtains the square the user would like to move to from their input command represented as 
   an int that corresponds to the bitboard 

   cmd has type Command.t *) 
let process_square cmd = raise (Failure "Unimplemented")


(* Obtains the piece that the user would like to move as a string. 
   
cmd has type Command.t *)
let process_piece cmd = raise (Failure "Unimplemented")


(* Given board_state, computes all legal moves (and prints message about the game ending 
   in this step if that is the case), queries and repeatedly waits for command corresponding 
   to legal move, then recurses on BoardState corresponding to chosen move *)
let rec_func (board_state: board_state) = raise (Failure "Unimplemented")




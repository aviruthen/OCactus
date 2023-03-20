open Chess
open State

let rec move_game mv bs = 
  let m = try Command.parse mv with
  | Command.MalformedInput -> Command.parse "a1 g8" in
  match Command.get_command m with 
  | "a1 g8" -> (* this is never a valid move*)
    ANSITerminal.print_string [ ANSITerminal.red ] "(for now we just crash after printing final board state) \n\n";
    (* move_game (read_line ()) bs *)
    bs
  | valid_move -> 
    let nbs = State.move bs (Command.parse valid_move) in 
      Stdlib.print_string "\n\n";
      State.print_board nbs 64;
    if nbs = bs then 
        (Stdlib.print_string "Enter valid move: "; 
        move_game (read_line ()) bs)
        else
          (Stdlib.print_string ">"; 
          move_game (read_line ()) nbs)

let play_game =
  Stdlib.print_string "\n\n";
  State.print_board State.init_chess 64;
  Stdlib.print_string "Enter move: ";
  match read_line () with
  | exception End_of_file -> ()
  | m -> State.print_board (move_game m State.init_chess) 64;

(* let () = play_game () *)

(** let main () = ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to
    Chess.\n"; print_endline "Test...\n"; print_string "> "; (** match read_line
    () with | exception End_of_file -> () | file_name -> play_game *) play_game

    (* let () = main () *) *)

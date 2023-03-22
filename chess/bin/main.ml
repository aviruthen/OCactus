open Chess
open State

let rec move_game mv bs =
  let m =
    try Command.parse mv with Command.MalformedInput -> Command.parse "a1 g8"
  in
  match Command.get_command m with
  | "a1 g8" ->
      (* this is never a valid move*)
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Invalid move format: try a move of the format \n\
        \    'start-square end-square'\n";
      Stdlib.print_string ("Enter a valid move (for " ^ get_turn bs ^ "): ");
      move_game (String.trim (read_line ())) bs
  | "quit game" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Thank you for playing chess!\n\n";
      bs
  | valid_move ->
      let nbs = State.move bs (Command.parse valid_move) in
      Stdlib.print_string "\n\n";
      State.print_board nbs;
      if nbs = bs then (
        ANSITerminal.print_string [ ANSITerminal.red ] "Not a playable move. \n";
        Stdlib.print_string ("Enter a valid move for " ^ get_turn bs ^ ": ");
        move_game (String.trim (read_line ())) bs)
      else (
        Stdlib.print_string ("Nice, enter a move (for " ^ get_turn nbs ^ "): ");
        move_game (String.trim (read_line ())) nbs)

let play_game =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to Ocactus Chess! \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Make moves using the format 'start-square end-square'\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "End the game by typing 'quit'";
  Stdlib.print_string "\n\n";
  State.print_board State.init_chess;
  Stdlib.print_string "Enter move (for white): ";
  match String.trim (read_line ()) with
  | exception End_of_file -> ()
  (* | m -> State.print_board (move_game m State.init_chess) 64; *)
  | m -> ignore (move_game m State.init_chess)

(* let () = play_game () *)

(** let main () = ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to
    Chess.\n"; print_endline "Test...\n"; print_string "> "; (** match read_line
    () with | exception End_of_file -> () | file_name -> play_game *) play_game

    (* let () = main () *) *)

open Chess
open State

let rec play_game =
  Stdlib.print_string "\n\n";
  State.print_board State.init_chess 64;
  Stdlib.print_string "\n\n"

(** let main () = ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to
    Chess.\n"; print_endline "Test...\n"; print_string "> "; (** match read_line
    () with | exception End_of_file -> () | file_name -> play_game *) play_game

    let () = main () *)

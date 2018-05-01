open State
open Command

(* [interactWithTerm state] takes in a state and based on state displays
   to the terminal. It then reads whatthe user writes in the terminal,
   updates state, quits if the user would like to, otherwise calls itself
   with a new state*)
let rec updateTerm state =
  ANSITerminal.(print_string [green](get_msg state));
  print_endline"";
  (* if(score state >= win_score state) then print_endline (winningMessage state); *)
  print_string  "> ";
  let cmd = parse(read_line()) in
  let updatedState = (do' cmd state) in
  updateTerm updatedState
  (* if(quitting updatedState) then () else interactWithTerm updatedState *)

(* [play_game f] plays the game in adventure file [f]. *)
let play_game players =
  let startState = (init_state (int_of_string players) (Yojson.Basic.from_file "dummy.json")) in
  updateTerm startState

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to Risk.\n");
  print_endline "Please enter the total number of human players (Between 2 and 5 inclusive).\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | players -> play_game players

let () = main ()

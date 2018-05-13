open State
open Command
open Gui

(* [interactWithTerm state] takes in a state and based on state displays
   to the terminal. It then reads whatthe user writes in the terminal,
   updates state, quits if the user would like to, otherwise calls itself
   with a new state*)
let rec updateTerm state =
  ANSITerminal.(print_string [green](get_msg state));
  print_endline"";
  (* if(score state >= win_score state) then print_endline (winningMessage state); *)
  print_string ((print_state state)^"\n");
  let player = get_cplayer state in
  print_string ("Player " ^ player ^ ": > ");
  if String.length player > 1
  then updateTerm state
  else
    let cmd = parse(read_line()) in
    let updatedState = (do' cmd state) in
    match cmd with
    |QuitC -> exit(0)
    |_->updateTerm updatedState

(* [play_game f] plays the game in adventure file [f]. *)
let play_game p ai =
  let w = GWindow.window ~title:"Risk" ~border_width:10 () in
  let _ = w#connect#destroy ~callback:GMain.Main.quit in
  let startState = (init_state (int_of_string p) (int_of_string ai) (Yojson.Basic.from_file "map.json")) in
  draw w [] startState
  (* updateTerm startState *)

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  let s = init_gui () in
  let p = String.make 1 (String.get s 0) in
  let ai = String.make 1 (String.get s 2) in
  play_game p ai

let () = main ()

open State
open Command
open Gui

(* [interactWithTerm state] takes in a state and based on state displays
   to the terminal. It then reads whatthe user writes in the terminal,
   updates state, quits if the user would like to, otherwise calls itself
   with a new state. This is no longer used but is left here for testing purposes
  with the repl *)
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

(* [play_game f] hands control over to the gui which runs a loop of taking in
   user actions and updating the gui*)
let play_game p eAI mAI hAI =
  let w = GWindow.window ~position:`CENTER ~title:"Risk" ~border_width:10 () in
  let _ = w#connect#destroy ~callback:GMain.Main.quit in
  let startState = (init_state p eAI mAI hAI (Yojson.Basic.from_file "map.json")) in
  draw w [] startState
  (* updateTerm startState *)

(* [main ()] starts the GUI, which prompts for the number of players
   for humans, ai and the difficulty of the ai if any ai is choosen. *)
let main () =
  let s = init_gui () in
  let p = (int_of_string (String.make 1 (String.get s 0))) in
  let eAI = (int_of_string (String.make 1 (String.get s 2))) in
  let mAI = (int_of_string (String.make 1 (String.get s 4))) in
  let hAI = (int_of_string (String.make 1 (String.get s 6))) in
  play_game p eAI mAI hAI

let () = main ()

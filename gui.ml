open State
open Ai
open GMain
open Gtk

let _ = GMain.Rc.add_default_file ("buttoncolors.rc")
let _ = GtkMain.Main.init ()

let winningScreen box msg w =
  (* let _ = w #connect#destroy ~callback:GMain.Main.quit in *)
  let _ = box#destroy () in
  let main_hbox = GPack.hbox ~packing:w#add ~width:1200 ~height:700 () in
  let gameBoard = GPack.fixed ~packing:main_hbox#add () in
  let _ = GMisc.image ~file: "images/riskEnd.png" ~packing:(gameBoard#put ~x:0 ~y:0) () in
  let _ = GMisc.label ~text:(msg) ~packing:(gameBoard#put ~x:400 ~y:85) () in
  GMain.Main.quit ()

let rec action cl box w st =
  (* let n = string_of_int ((int_of_string label#text) + 1) in
     label#set_text n *)
  let _ = box#destroy () in
  if String.length (get_cplayer st) > 1
  then draw w [] st
  else
    if (List.hd cl) = "END"
    then let st' = do' (EndPhaseC) st in draw w [] st'
    else
      match getPhase st with
      | SetUp -> let st' = do' (ClaimC (List.hd cl)) st in draw w [] st'
      | Game x ->
        match x with
        | Deploy -> let st' = do' (DeployC (1,(List.hd cl))) st in draw w [] st'
        | Attack ->
          if List.length cl = 1 then draw w cl st
          else let st' = do' (AttackC (List.hd (List.rev cl), List.hd cl)) st in draw w [] st'
        | Reinforce ->
        if List.length cl = 1 then draw w cl st
        else let plyr = get_player_of_state st in
          let fst_ctry = (List.hd(List.rev cl)) in
          let num_troops = get_troops fst_ctry plyr in
          let st' = do' (ReinforceC (num_troops - 1, List.hd (List.rev cl), List.hd cl)) st in draw w [] st'
and aiAction box w st =
  let _ = print_string "ai actioning\n" in
  let _ = box#destroy () in
  let cmd = determine_move st in
  if List.length (get_player_list st) = 1 then
    let plyr = get_cplayer st in
    let main_hbox = GPack.hbox ~packing:w#add ~width:1200 ~height:700 () in
    winningScreen main_hbox ("Congratulations Player: " ^ plyr) w
  else
  let st' = do' (cmd) st in
  draw w [] st'
and draw window cl st =
  let _ = window#connect#destroy ~callback:GMain.Main.quit in
  let main_hbox = GPack.hbox ~packing:window#add ~width:1200 ~height:700 () in
  let has_won st =
    if List.length (get_player_list st) = 1
    then let plyr = get_cplayer st in
      winningScreen main_hbox ("Congratulations Player: " ^ plyr) window
    else () in
  let _ = has_won st in
  let gameBoard = GPack.fixed ~packing:main_hbox#add () in
  let _ = GMisc.image ~file: "images/riskmap.png" ~packing:(gameBoard#put ~x:0 ~y:0) () in
  let _ = GMisc.label ~text:(get_cplayer st) ~packing:(gameBoard#put ~x:1090 ~y:15) () in
  let _ = GMisc.label ~text:((get_msg st)^"\n"^(getPhaseString st)^"\n\n"^"Order of Players is:\n"^(printOrder st)) ~packing:(gameBoard#put ~x:1025 ~y:525) () in
  let txt = string_of_int (get_bonus_troops st) in
  let _ = GMisc.label ~text:("Next card bonus: " ^ txt)
      ~packing:(gameBoard#put ~x:800 ~y:35) () in
  let right_buttons = GPack.button_box `VERTICAL ~border_width:0 ~child_width:160 ~child_height:30
      ~spacing:10 ~packing:(gameBoard#put ~x:1020 ~y:40) () in

  let string_phase st =
    match getPhase st with
    | SetUp -> "setup"
    | Game x ->
      match x with
      | Deploy -> "deploy"
      | Attack -> "attack"
      | Reinforce -> "reinforce"
  in
  (* Have to change this. *)
  let setupButton = GButton.button ~label:"Set Up" ~packing:right_buttons#add () in
  let _ = setupButton#connect#clicked ~callback: (fun () -> ()) in
  setupButton#misc#set_name ("setup" ^ string_phase st);
  let deployButton = GButton.button ~label:"Deploy" ~packing:right_buttons#add () in
  let _ = deployButton#connect#clicked ~callback: (fun () -> ()) in
  deployButton#misc#set_name ("deploy" ^ string_phase st);
  let attackButton = GButton.button ~label:"Attack" ~packing:right_buttons#add () in
  let _ = attackButton#connect#clicked ~callback: (fun () -> ()) in
  attackButton#misc#set_name ("attack" ^ string_phase st);
  let reinforceButton = GButton.button ~label:"Reinforce" ~packing:right_buttons#add () in
  let _ = reinforceButton#connect#clicked ~callback: (fun () -> ()) in
  reinforceButton#misc#set_name ("reinforce" ^ string_phase st);

  let finishButton = GButton.button ~label:"Finish" ~packing:(right_buttons#add) () in
  let _ = finishButton#connect#clicked ~callback: (fun () -> action ("END"::cl) main_hbox window st) in

  let player_turn_image =
    match (get_cplayer st) with
    | "1" | "ae1" | "am1" | "ah1" -> (let _ = GMisc.image ~file:
                  "images/green.png" ~packing:(gameBoard#put ~x:1110 ~y:10) () in ())
    | "2" | "ae2" | "am2" | "ah2" -> (let _ = GMisc.image ~file:
                  "images/yellow.png" ~packing:(gameBoard#put ~x:1110 ~y:10) () in ())
    | "3" | "ae3" | "am3" | "ah3" -> (let _ = GMisc.image ~file:
                  "images/orange.png" ~packing:(gameBoard#put ~x:1110 ~y:10) () in ())
    | "4" | "ae4" | "am4" | "ah4" -> (let _ = GMisc.image ~file:
                  "images/red.png" ~packing:(gameBoard#put ~x:1110 ~y:10) () in ())
    | "5" | "ae5" | "am5" | "ah5" -> (let _ = GMisc.image ~file:
                  "images/blue.png" ~packing:(gameBoard#put ~x:1110 ~y:10) () in ())
    | _ -> failwith ((get_cplayer st)) in
  let _ = player_turn_image in

  let countryTroops = getCountryTroops st in

  let rc_file_text label =
    match label with
    | "Casterly\nRock" -> "casterly"
    | "Dragon-\nstone" -> "dragonstone"
    | "Storm's End" -> "storm"
    | "Dothraki Sea" -> "dothraki"
    | "Slaver Bay" -> "slaver"
    | "Unknown Lands" -> "unknown"
    | "Red Waste" -> "redwaste"
    | "Iron Islands" -> "ironislands"
    | _ -> String.lowercase_ascii label
  in

  let button_color_determiner st label =
    let plyr_id = country_owned_by_player st (String.uppercase_ascii label) in
    match plyr_id with
    | "1" | "ae1" | "am1" | "ah1" -> (rc_file_text label)^"G"
    | "2" | "ae2" | "am2" | "ah2" -> (rc_file_text label)^"Y"
    | "3" | "ae3" | "am3" | "ah3" -> (rc_file_text label)^"O"
    | "4" | "ae4" | "am4" | "ah4" -> (rc_file_text label)^"R"
    | "5" | "ae5" | "am5" | "ah5" -> (rc_file_text label)^"B"
    | "Unheld" -> (String.lowercase_ascii label)
    | _ -> failwith ("Impossible") in
  let countryA = GButton.button ~label:"Winter" ~packing:(gameBoard#put ~x:120 ~y:30) () in
  countryA#misc#set_name (button_color_determiner st countryA#label);
  (* let _ = countryA# `WHITE *)
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryA#label) countryTroops)) with
      | _ -> "0")
      ~packing:(gameBoard#put ~x:138 ~y:55) () in
  let _ =countryA#connect#clicked ~callback: (fun () -> action (countryA#label::cl) main_hbox window st) in
  let countryB = GButton.button ~label:"Winterfell" ~packing:(gameBoard#put ~x:140 ~y:120) () in
  countryB#misc#set_name (button_color_determiner st countryB#label);
  let _ = countryB#connect#clicked ~callback: (fun () -> action (countryB#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryB#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:165 ~y:145) () in
  let countryC = GButton.button ~label:"Riverrun" ~packing:(gameBoard#put ~x:180 ~y:180) () in
  countryC#misc#set_name (button_color_determiner st countryC#label);
  let _ = countryC#connect#clicked ~callback: (fun () -> action (countryC#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryC#label) countryTroops)) with
      | _ -> "0")
      ~packing:(gameBoard#put ~x:200 ~y:205) () in
  let countryD = GButton.button ~label:"Vale" ~packing:(gameBoard#put ~x:178 ~y:240) () in
  countryD#misc#set_name (button_color_determiner st countryD#label);
  let _ = countryD#connect#clicked ~callback: (fun () -> action (countryD#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryD#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:192 ~y:265) () in
  let countryE = GButton.button ~label:"Casterly\nRock" ~packing:(gameBoard#put ~x:110 ~y:280) () in
  countryE#misc#set_name (button_color_determiner st countryE#label);
  let _ = countryE#connect#clicked ~callback: (fun () -> action (countryE#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryE#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:129 ~y:320) () in
  let countryF = GButton.button ~label:"Dragon-\nstone" ~packing:(gameBoard#put ~x:203 ~y:325) () in
  countryF#misc#set_name (button_color_determiner st countryF#label);
  let _ = countryF#connect#clicked ~callback: (fun () -> action (countryF#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryF#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:225 ~y:365) () in
  let countryG = GButton.button ~label:"Storm's End" ~packing:(gameBoard#put ~x:120 ~y:360) () in
  countryG#misc#set_name (button_color_determiner st countryG#label);
  let _ = countryG#connect#clicked ~callback: (fun () -> action (countryG#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryG#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:150 ~y:385) () in
  let countryH = GButton.button ~label:"Dorne" ~packing:(gameBoard#put ~x:170 ~y:457) () in
  countryH#misc#set_name (button_color_determiner st countryH#label);
  let _ = countryH#connect#clicked ~callback: (fun () -> action (countryH#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryH#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x:185 ~y:482) () in
  let countryJ = GButton.button ~label:"Pentos" ~packing:(gameBoard#put ~x:455 ~y:320) () in
  countryJ#misc#set_name (button_color_determiner st countryJ#label);
  let _ = countryJ#connect#clicked ~callback: (fun () -> action (countryJ#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryJ#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 470 ~y:345) () in
  let countryK = GButton.button ~label:"Braavos" ~packing:(gameBoard#put ~x:425 ~y:220) () in
  countryK#misc#set_name (button_color_determiner st countryK#label);
  let _ = countryK#connect#clicked ~callback: (fun () -> action (countryK#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryK#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 445 ~y:245) () in
  let countryL = GButton.button ~label:"Tyrosh" ~packing:(gameBoard#put ~x:440 ~y:410) () in
  countryL#misc#set_name (button_color_determiner st countryL#label);
  let _ = countryL#connect#clicked ~callback: (fun () -> action (countryL#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryL#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 457 ~y:435) () in
  let countryM = GButton.button ~label:"Dothraki Sea" ~packing:(gameBoard#put ~x:645 ~y:265) () in
  countryM#misc#set_name (button_color_determiner st countryM#label);
  let _ = countryM#connect#clicked ~callback: (fun () -> action (countryM#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryM#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 680 ~y:290) () in
  let countryN = GButton.button ~label:"Volantis" ~packing:(gameBoard#put ~x:545 ~y:425) () in
  countryN#misc#set_name (button_color_determiner st countryN#label);
  let _ = countryN#connect#clicked ~callback: (fun () -> action (countryN#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryN#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 563 ~y:450) () in
  let countryO = GButton.button ~label:"Slaver Bay" ~packing:(gameBoard#put ~x:646 ~y:395) () in
  countryO#misc#set_name (button_color_determiner st countryO#label);
  let _ = countryO#connect#clicked ~callback: (fun () -> action (countryO#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryO#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 661 ~y:420) () in

  let countryP = GButton.button ~label:"Unknown Lands" ~packing:(gameBoard#put ~x:800 ~y:200) () in
  countryP#misc#set_name (button_color_determiner st countryP#label);
  let _ = countryP#connect#clicked ~callback: (fun () -> action (countryP#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryP#label) countryTroops)) with
      | _ -> "0")
      ~packing:(gameBoard#put ~x: 835 ~y:225) () in
  let countryQ = GButton.button ~label:"Red Waste" ~packing:(gameBoard#put ~x:760 ~y:420) () in
  countryQ#misc#set_name (button_color_determiner st countryQ#label);
  let _ = countryQ#connect#clicked ~callback: (fun () -> action (countryQ#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryQ#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 790 ~y:445) () in
  let countryR = GButton.button ~label:"Skagos" ~packing:(gameBoard#put ~x:280 ~y:100) () in
  countryR#misc#set_name (button_color_determiner st countryR#label);
  let _ = countryR#connect#clicked ~callback: (fun () -> action (countryR#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryR#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 300 ~y:125) () in
  let countryS = GButton.button ~label:"Iron Islands" ~packing:(gameBoard#put ~x:10 ~y:220) () in
  countryS#misc#set_name (button_color_determiner st countryS#label);
  let _ = countryS#connect#clicked ~callback: (fun () -> action (countryS#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryS#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 45 ~y:245) () in
  let countryT = GButton.button ~label:"Highgarden" ~packing:(gameBoard#put ~x:10 ~y:440) () in
  countryT#misc#set_name (button_color_determiner st countryT#label);
  let _ = countryT#connect#clicked ~callback: (fun () -> action (countryT#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryT#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 37 ~y:470) () in
  let countryU = GButton.button ~label:"Qohor" ~packing:(gameBoard#put ~x:520 ~y:240) () in
  countryU#misc#set_name (button_color_determiner st countryU#label);
  let _ = countryU#connect#clicked ~callback: (fun () -> action (countryU#label::cl) main_hbox window st) in
  let _ = GMisc.label ~text:
      (try (string_of_int (List.assoc (String.uppercase_ascii countryU#label) countryTroops)) with
       | _ -> "0")
      ~packing:(gameBoard#put ~x: 538 ~y:265) () in

  let image_match card =
    match card with
    | BannerMan -> "images/BannerCard.png"
    | Lord -> "images/LordCard.png"
    | Dragon -> "images/DragonCard.png"
    | WildCard -> "images/WildCard.png"
  in

  let draw_cards =
    let curr_player = get_player_of_state st in
    let card_lst = (cards_owned curr_player) in
    for i = 0 to List.length (cards_owned curr_player) - 1
    do let img = image_match (List.nth card_lst i) in
      let xcoord = 60 + i * 100 in
      let _ = GMisc.image ~file:img ~packing:(gameBoard#put ~x:xcoord ~y: 590) () in ()
    done
  in let _ = draw_cards in

  let aDice =
    match getAttackDice st with
    | [] ->
      (let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1020 ~y:300) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1080 ~y:300) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1140 ~y:300) () in ())
    | x::[] ->
      (let _ = GMisc.image ~file: ("images/dice"^(string_of_int x)^".png") ~packing:(gameBoard#put ~x:1020 ~y:300) () in
      let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1080 ~y:300) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1140 ~y:300) () in ())
    | x::x'::[] ->
      (let _ = GMisc.image ~file: ("images/dice"^(string_of_int x)^".png") ~packing:(gameBoard#put ~x:1020 ~y:300) () in
      let _ = GMisc.image ~file: ("images/dice"^(string_of_int x')^".png") ~packing:(gameBoard#put ~x:1080 ~y:300) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1140 ~y:300) () in ())
    | x::x'::x''::[] ->
      (let _ = GMisc.image ~file: ("images/dice"^(string_of_int x)^".png") ~packing:(gameBoard#put ~x:1020 ~y:300) () in
      let _ = GMisc.image ~file: ("images/dice"^(string_of_int x')^".png") ~packing:(gameBoard#put ~x:1080 ~y:300) () in
       let _ = GMisc.image ~file: ("images/dice"^(string_of_int x'')^".png") ~packing:(gameBoard#put ~x:1140 ~y:300) () in ())
    | _ -> (failwith "Invalid Roll") in
  let _ = aDice in

  let dDice =
    match getDefendDice st with
    | [] ->
      (let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1040 ~y:400) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1120 ~y:400) () in ())
    | x::[] ->
      (let _ = GMisc.image ~file: ("images/dice"^(string_of_int x)^".png") ~packing:(gameBoard#put ~x:1040 ~y:400) () in
       let _ = GMisc.image ~file: "images/nodice.png" ~packing:(gameBoard#put ~x:1120 ~y:400) () in ())
    | x::x'::[] ->
      (let _ = GMisc.image ~file: ("images/dice"^(string_of_int x)^".png") ~packing:(gameBoard#put ~x:1040 ~y:400) () in
       let _ = GMisc.image ~file: ("images/dice"^(string_of_int x')^".png") ~packing:(gameBoard#put ~x:1120 ~y:400) () in ())
    | _ -> (failwith "Invalid Roll") in
  let _ = dDice in

  let _ = window#show () in
  if String.length (get_cplayer st) > 1
  then aiAction main_hbox window st
  else
    let _ = GMain.Main.main () in ()


let selectPlayers p ai aiD r vbox w=
  let _ = vbox#destroy () in
  let _ = w#destroy () in
  let _ = GMain.Main.quit () in
  match aiD with
  |"e"->r := string_of_int p ^ "/" ^ string_of_int ai ^ "/0/0"
  |"m"->r := string_of_int p ^ "/0/" ^ string_of_int ai ^ "/0"
  |"h"->r := string_of_int p ^ "/0/0/" ^ string_of_int ai
  |_->r := string_of_int p ^ "/0/0/0"



let selectAIDifficulty p ai r vbox window=
  let _ = vbox#destroy () in
  let main_vbox = GPack.vbox ~packing:window#add ~width:600 ~height:700 () in
  let mainBoard = GPack.fixed ~packing:main_vbox#add () in
  let _ = GMisc.image ~file: "images/aiDiff.png" ~packing:(mainBoard#put ~x:0 ~y:0) () in
  let vbox1 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:400 ~child_height:120
      ~spacing:20 ~packing:(mainBoard#put ~x:75 ~y:200)() in
  let p0 = GButton.button ~label:"Easy" ~packing:vbox1#add () in
  let _ = p0#connect#clicked ~callback: (fun () -> selectPlayers p ai "e" r main_vbox window) in
  p0#misc#set_name ("p0");
  let p1 = GButton.button ~label:"Medium" ~packing:vbox1#add () in
  let _ = p1#connect#clicked ~callback: (fun () -> selectPlayers p ai "m" r main_vbox window) in
  p1#misc#set_name ("p0");
  let p2 = GButton.button ~label:"Hard" ~packing:vbox1#add () in
  let _ = p2#connect#clicked ~callback: (fun () -> selectPlayers p ai "h" r main_vbox window) in
  p2#misc#set_name ("p0");
  ()


let selectHumanPlayers num r vbox window =
  let _ = vbox#destroy () in
  let main_vbox = GPack.vbox ~packing:window#add ~width:600 ~height:700 () in
  let mainBoard = GPack.fixed ~packing:main_vbox#add () in
  let _ = GMisc.image ~file: "images/aiNum.png" ~packing:(mainBoard#put ~x:0 ~y:0) () in
  let vbox1 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:150 ~child_height:120
      ~spacing:20 ~packing:(mainBoard#put ~x:100 ~y:200)() in
  let vbox2 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:150 ~child_height:120
      ~spacing:20 ~packing:(mainBoard#put ~x:300 ~y:200)() in
  (* let biggerWindow = GWindow.window ~width:1000 ~height:800 ~title:"Game" ~border_width:10 () in *)
  match num with
  | 1 ->
    let i1 = GButton.button ~label:"0 AI Players (INVALID)" ~packing:vbox1#add () in
    let _ = i1#connect#clicked ~callback: (fun () -> ()) in
    i1#misc#set_name ("i0");
    let p2 = GButton.button ~label:"1 AI Player" ~packing:vbox2#add () in
    let _ = p2#connect#clicked ~callback: (fun () -> selectAIDifficulty 1 1 r main_vbox window) in
    p2#misc#set_name ("p0");
    let p3 = GButton.button ~label:"2 AI Players" ~packing:vbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectAIDifficulty 1 2 r main_vbox window) in
    p3#misc#set_name ("p0");
    let p4 = GButton.button ~label:"3 AI Players" ~packing:vbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectAIDifficulty 1 3 r main_vbox window) in
    p4#misc#set_name ("p0");
    let p5 = GButton.button ~label:"4 AI Players" ~packing:vbox1#add () in
    let _ = p5#connect#clicked ~callback: (fun () -> selectAIDifficulty 1 4 r main_vbox window) in
    p5#misc#set_name ("p0");
    let i2 = GButton.button ~label:"5 AI Players (INVALID)" ~packing:vbox2#add () in
    i2#misc#set_name ("i0");
    let _ = i2#connect#clicked ~callback: (fun () -> ()) in ()
  | 2 ->
    let p2 = GButton.button ~label:"0 AI Players" ~packing:vbox1#add () in
    let _ = p2#connect#clicked ~callback: (fun () -> selectPlayers 2 0 "" r main_vbox window) in
    p2#misc#set_name ("p0");
    let p3 = GButton.button ~label:"1 AI Player" ~packing:vbox2#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectAIDifficulty 2 1 r main_vbox window) in
    p3#misc#set_name ("p0");
    let p4 = GButton.button ~label:"2 AI Players" ~packing:vbox1#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectAIDifficulty 2 2 r main_vbox window) in
    p4#misc#set_name ("p0");
    let p5 = GButton.button ~label:"3 AI Players" ~packing:vbox2#add () in
    let _ = p5#connect#clicked ~callback: (fun () -> selectAIDifficulty 2 3 r main_vbox window)in
    p5#misc#set_name ("p0");
    let i1 = GButton.button ~label:"4 AI Players (INVALID)" ~packing:vbox1#add () in
    let _ = i1#connect#clicked ~callback: (fun () -> ()) in
    i1#misc#set_name ("i0");
    let i2 = GButton.button ~label:"5 AI Players (INVALID)" ~packing:vbox2#add () in
    let _ = i2#connect#clicked ~callback: (fun () -> ()) in
    i2#misc#set_name ("i0");
    ()
  | 3 ->
    let p2 = GButton.button ~label:"0 AI Players" ~packing:vbox1#add () in
    let _ = p2#connect#clicked ~callback: (fun () -> selectPlayers 3 0 "" r main_vbox window) in
    p2#misc#set_name ("p0");
    let p3 = GButton.button ~label:"1 AI Player" ~packing:vbox2#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectAIDifficulty 3 1 r main_vbox window) in
    p3#misc#set_name ("p0");
    let p4 = GButton.button ~label:"2 AI Players" ~packing:vbox1#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectAIDifficulty 3 2 r main_vbox window) in
    p4#misc#set_name ("p0");
    let i0 = GButton.button ~label:"3 AI Players (INVALID)" ~packing:vbox2#add () in
    let _ = i0#connect#clicked ~callback: (fun () -> ()) in
    i0#misc#set_name ("i0");
    let i1 = GButton.button ~label:"4 AI Players (INVALID)" ~packing:vbox1#add () in
    let _ = i1#connect#clicked ~callback: (fun () -> ()) in
    i1#misc#set_name ("i0");
    let i2 = GButton.button ~label:"5 AI Players (INVALID)" ~packing:vbox2#add () in
    let _ = i2#connect#clicked ~callback: (fun () -> ()) in
    i2#misc#set_name ("i0");
     ()
  | 4 ->
    let p2 = GButton.button ~label:"0 AI Players" ~packing:vbox1#add () in
    let _ = p2#connect#clicked ~callback: (fun () -> selectPlayers 4 0 "" r main_vbox window) in
    p2#misc#set_name ("p0");
    let p3 = GButton.button ~label:"1 AI Player" ~packing:vbox2#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectAIDifficulty 4 1 r main_vbox window) in
    p3#misc#set_name ("p0");
    let i4 = GButton.button ~label:"2 AI Players (INVALID)" ~packing:vbox1#add () in
    let _ = i4#connect#clicked ~callback: (fun () -> ()) in
    i4#misc#set_name ("i0");
    let i0 = GButton.button ~label:"3 AI Players (INVALID)" ~packing:vbox2#add () in
    let _ = i0#connect#clicked ~callback: (fun () -> ()) in
    i0#misc#set_name ("i0");
    let i1 = GButton.button ~label:"4 AI Players (INVALID)" ~packing:vbox1#add () in
    let _ = i1#connect#clicked ~callback: (fun () -> ()) in
    i1#misc#set_name ("i0");
    let i2 = GButton.button ~label:"5 AI Players (INVALID)" ~packing:vbox2#add () in
    let _ = i2#connect#clicked ~callback: (fun () -> ()) in
    i2#misc#set_name ("i0");
    ()
  | _ -> failwith ("Cannot be possible!")

let init_gui () =
  let r = ref "" in
  let window = GWindow.window ~position:`CENTER ~title:"Risk" ~border_width:10 () in
  let _ = window #connect#destroy ~callback:GMain.Main.quit in
  let main_vbox = GPack.vbox ~packing:window#add ~width:600 ~height:700 () in
  let mainBoard = GPack.fixed ~packing:main_vbox#add () in
  let _ = GMisc.image ~file: "images/human.png" ~packing:(mainBoard#put ~x:0 ~y:0) () in
  let vbox1 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:150 ~child_height:120
      ~spacing:20 ~packing:(mainBoard#put ~x:100 ~y:200)() in
  let vbox2 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:150 ~child_height:120
      ~spacing:20 ~packing:(mainBoard#put ~x:300 ~y:200)() in
  let vbox3 = GPack.button_box `VERTICAL ~border_width:20 ~child_width:350 ~child_height:120
        ~spacing:20 ~packing:(mainBoard#put ~x:100 ~y:480)() in

  let p1 = GButton.button ~label:"1 Human Players" ~packing:vbox1#add () in
  let _ = p1#connect#clicked ~callback: (fun () -> selectHumanPlayers 1 r main_vbox window) in
  p1#misc#set_name ("p0");
  let p2 = GButton.button ~label:"2 Human Players" ~packing:vbox2#add () in
  let _ = p2#connect#clicked ~callback: (fun () -> selectHumanPlayers 2 r main_vbox window) in
  p2#misc#set_name ("p0");
  let p3 = GButton.button ~label:"3 Human Players" ~packing:vbox1#add () in
  let _ = p3#connect#clicked ~callback: (fun () -> selectHumanPlayers 3 r main_vbox window) in
  p3#misc#set_name ("p0");
  let p4 = GButton.button ~label:"4 Human Players" ~packing:vbox2#add () in
  let _ = p4#connect#clicked ~callback: (fun () -> selectHumanPlayers 4 r main_vbox window) in
  p4#misc#set_name ("p0");
  let p5 = GButton.button ~label:"5 Human Players" ~packing:vbox3#add () in
  let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 5 0 "" r main_vbox window) in
  p5#misc#set_name ("p0");
  let _ = window#show () in
  (* Enter the event loop *)
  let _ = GMain.Main.main () in
  !r

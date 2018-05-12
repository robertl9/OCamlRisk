open State
open GMain
open Gtk

let _ = GtkMain.Main.init ()

let selectPlayers p ai r vbox =
  let _ = vbox#destroy () in
  let _ = GMain.Main.quit () in
  r := string_of_int p ^ "/" ^ string_of_int ai

let rec draw st =
  let window = GWindow.window ~title:"Risk" ~border_width:10 () in
  let _ = window #connect#destroy ~callback:GMain.Main.quit in
  let main_hbox = GPack.hbox ~packing:window#add ~width:1200 ~height:700 () in
  let gameBoard = GPack.fixed ~packing:main_hbox#add () in
  let _ = GMisc.image ~file: "images/riskmap.png" ~packing:(gameBoard#put ~x:0 ~y:0) () in
  let currentPlayer = GMisc.label ~text:(get_cplayer(st)) ~packing:(gameBoard#put ~x:1090 ~y:25) () in
  let consoleMessage = GMisc.label ~text:(get_msg(st)) ~packing:(gameBoard#put ~x:1020 ~y:505) () in
  let right_buttons = GPack.button_box `VERTICAL ~border_width:0 ~child_width:160 ~child_height:30
      ~spacing:10 ~packing:(gameBoard#put ~x:1020 ~y:60) () in

  let deployButton = GButton.button ~label:"Deploy" ~packing:right_buttons#add () in
  let _ = deployButton#connect#clicked ~callback: (fun () -> ()) in
  let attackButton = GButton.button ~label:"Attack" ~packing:right_buttons#add () in
  let _ = attackButton#connect#clicked ~callback: (fun () -> ()) in
  let reinforceButton = GButton.button ~label:"Reinforce" ~packing:right_buttons#add () in
  let _ = reinforceButton#connect#clicked ~callback: (fun () -> ()) in
  let finishButton = GButton.button ~label:"Finish" ~packing:right_buttons#add () in
  let _ = finishButton#connect#clicked ~callback: (fun () -> ()) in

  let winterButton = GButton.button ~label:"Winter" ~packing:(gameBoard#put ~x:120 ~y:30) () in
  let _ =winterButton#connect#clicked ~callback: (fun () -> ()) in
  let winterLabel = GMisc.label ~text:"3" ~packing:(gameBoard#put ~x:138 ~y:55) () in
  let countryB = GButton.button ~label:"Winterfell" ~packing:(gameBoard#put ~x:140 ~y:120) () in
  let _ = countryB#connect#clicked ~callback: (fun () -> ()) in
  let countryBLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:165 ~y:145) () in
  let countryC = GButton.button ~label:"Riverrun" ~packing:(gameBoard#put ~x:180 ~y:180) () in
  let _ = countryC#connect#clicked ~callback: (fun () -> ()) in
  let countryCLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:200 ~y:205) () in
  let countryD = GButton.button ~label:"Vale" ~packing:(gameBoard#put ~x:178 ~y:240) () in
  let _ = countryD#connect#clicked ~callback: (fun () -> ()) in
  let countryDLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:192 ~y:265) () in
  let countryE = GButton.button ~label:"Casterly\nRock" ~packing:(gameBoard#put ~x:110 ~y:280) () in
  let _ = countryE#connect#clicked ~callback: (fun () -> ()) in
  let countryELabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:129 ~y:320) () in
  let countryF = GButton.button ~label:"Dragon-\nstone" ~packing:(gameBoard#put ~x:203 ~y:325) () in
  let _ = countryF#connect#clicked ~callback: (fun () -> ()) in
  let countryFLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:225 ~y:365) () in
  let countryG = GButton.button ~label:"Storm's End" ~packing:(gameBoard#put ~x:120 ~y:360) () in
  let _ = countryG#connect#clicked ~callback: (fun () -> ()) in
  let countryGLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:150 ~y:385) () in
  let countryH = GButton.button ~label:"Dorne" ~packing:(gameBoard#put ~x:170 ~y:457) () in
  let _ = countryH#connect#clicked ~callback: (fun () -> ()) in
  let countryHLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:185 ~y:482) () in
  let countryJ = GButton.button ~label:"Pentos" ~packing:(gameBoard#put ~x:455 ~y:320) () in
  let _ = countryJ#connect#clicked ~callback: (fun () -> ()) in
  let countryJLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 470 ~y:345) () in

  let countryK = GButton.button ~label:"Braavos" ~packing:(gameBoard#put ~x:425 ~y:220) () in
  let _ = countryK#connect#clicked ~callback: (fun () -> ()) in
  let countryKLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 445 ~y:245) () in

  let countryL = GButton.button ~label:"Tyrosh" ~packing:(gameBoard#put ~x:440 ~y:410) () in
  let _ = countryL#connect#clicked ~callback: (fun () -> ()) in
  let countryLLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 457 ~y:435) () in

  let countryM = GButton.button ~label:"Dothraki Sea" ~packing:(gameBoard#put ~x:645 ~y:265) () in
  let _ = countryM#connect#clicked ~callback: (fun () -> ()) in
  let countryMLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 680 ~y:290) () in

  let countryN = GButton.button ~label:"Volantis" ~packing:(gameBoard#put ~x:545 ~y:425) () in
  let _ = countryN#connect#clicked ~callback: (fun () -> ()) in
  let countryNLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 563 ~y:450) () in

  let countryO = GButton.button ~label:"Slaver Bay" ~packing:(gameBoard#put ~x:646 ~y:395) () in
  let _ = countryO#connect#clicked ~callback: (fun () -> ()) in
  let countryOLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 661 ~y:420) () in

  let countryP = GButton.button ~label:"Unknown Lands" ~packing:(gameBoard#put ~x:800 ~y:200) () in
  let _ = countryP#connect#clicked ~callback: (fun () -> ()) in
  let countryPLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 835 ~y:225) () in

  let countryQ = GButton.button ~label:"Red Waste" ~packing:(gameBoard#put ~x:760 ~y:420) () in
  let _ = countryQ#connect#clicked ~callback: (fun () -> ()) in
  let countryQLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 790 ~y:445) () in

  let countryR = GButton.button ~label:"Skagos" ~packing:(gameBoard#put ~x:280 ~y:100) () in
  let _ = countryR#connect#clicked ~callback: (fun () -> ()) in
  let countryRLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 300 ~y:125) () in

  let countryS = GButton.button ~label:"Iron Islands" ~packing:(gameBoard#put ~x:10 ~y:220) () in
  let _ = countryS#connect#clicked ~callback: (fun () -> ()) in
  let countrySLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 45 ~y:245) () in

  let countryT = GButton.button ~label:"HighGarden" ~packing:(gameBoard#put ~x:10 ~y:440) () in
  let _ = countryT#connect#clicked ~callback: (fun () -> ()) in
  let countryTLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 37 ~y:470) () in

  let countryU = GButton.button ~label:"Qohor" ~packing:(gameBoard#put ~x:520 ~y:240) () in
  let _ = countryU#connect#clicked ~callback: (fun () -> ()) in
  let countryULabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 538 ~y:265) () in






  let a_dice1 = GMisc.image ~file: "images/dice1.png" ~packing:(gameBoard#put ~x:1020 ~y:300) () in
  let a_dice2 = GMisc.image ~file: "images/dice5.png" ~packing:(gameBoard#put ~x:1080 ~y:300) () in
  let a_dice3 = GMisc.image ~file: "images/dice6.png" ~packing:(gameBoard#put ~x:1140 ~y:300) () in
  let d_dice1 = GMisc.image ~file: "images/dice1.png" ~packing:(gameBoard#put ~x:1040 ~y:400) () in
  let d_dice2 = GMisc.image ~file: "images/dice3.png" ~packing:(gameBoard#put ~x:1120 ~y:400) () in
  let _ = window#show () in
  let _ = GMain.Main.main () in ()



let selectHumanPlayers num r vbox window =
  let _ = vbox#destroy () in
  let main_vbox = GPack.vbox ~packing:window#add () in
  let instruct = GMisc.label ~text:"Please Select the Number of Human/AI Players" ~packing:main_vbox#add () in
  let vbox = GPack.vbox ~border_width:10 ~packing:main_vbox#add () in
  let hbox1 = GPack.button_box `HORIZONTAL ~border_width:20 ~child_width:100 ~child_height:100
      ~spacing:20 ~packing:vbox#add () in
  let hbox2 = GPack.button_box `HORIZONTAL ~border_width:20 ~child_width:100 ~child_height:100
      ~spacing:20 ~packing:vbox#add () in
  let biggerWindow = GWindow.window ~width:1000 ~height:800 ~title:"Game" ~border_width:10 () in
  match num with
  | 2 ->
    let title = GMisc.label ~text:"2 Players Selected." ~packing:main_vbox#add ~height:50 () in
    let p2 = GButton.button ~label:"2 Human & 0 AI Players" ~packing:hbox1#add () in
    let _ = p2#connect#clicked ~callback: (fun () -> selectPlayers 2 0 r main_vbox) in
    let p3 = GButton.button ~label:"1 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 1 1 r main_vbox) in
    let p4 = GButton.button ~label:"0 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 0 2 r main_vbox) in ()
  | 3 ->
    let title = GMisc.label ~text:"3 Players Selected." ~packing:main_vbox#add ~height:50 () in
    let p2 = GButton.button ~label:"3 Human & 0 AI Players" ~packing:hbox1#add () in
    let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 3 0 r main_vbox) in
    let p3 = GButton.button ~label:"2 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 2 1 r main_vbox) in
    let p4 = GButton.button ~label:"1 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 1 2 r main_vbox) in
    let p5 = GButton.button ~label:"0 Humans & 3 AI Players" ~packing:hbox2#add () in
    let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 0 0 r main_vbox) in ()
  | 4 ->
    let title = GMisc.label ~text:"4 Players Selected." ~packing:main_vbox#add ~height:50 () in
    let p2 = GButton.button ~label:"4 Humans & 0 AI Players" ~packing:hbox1#add () in
    let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 4 0 r main_vbox) in
    let p3 = GButton.button ~label:"3 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 3 1 r main_vbox) in
    let p4 = GButton.button ~label:"2 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 2 2 r main_vbox) in
    let p5 = GButton.button ~label:"1 Humans & 3 AI Players" ~packing:hbox2#add () in
    let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 1 3 r main_vbox) in
    let p6 = GButton.button ~label:"0 Humans & 4 AI Players" ~packing:hbox2#add () in
    let _ =p6#connect#clicked ~callback: (fun () -> selectPlayers 0 4 r main_vbox) in ()
  | _ -> failwith ("Cannot be possible!")

let init_gui () =
  let r = ref "" in
  let window = GWindow.window ~title:"Risk" ~border_width:10 () in
  let _ = window #connect#destroy ~callback:GMain.Main.quit in
  let main_vbox = GPack.vbox ~packing:window#add () in
  let title = GMisc.label ~text:"Risk" ~packing:main_vbox#add ~height:50 () in
  let instruct = GMisc.label ~text:"Please Select the Number of Players" ~packing:main_vbox#add () in
  let vbox = GPack.vbox ~border_width:10 ~packing:main_vbox#add () in
  let hbox1 = GPack.button_box `HORIZONTAL ~border_width:20 ~child_width:100 ~child_height:100
      ~spacing:20 ~packing:vbox#add () in
  let hbox2 = GPack.button_box `HORIZONTAL ~border_width:20 ~child_width:100 ~child_height:100
      ~spacing:20 ~packing:vbox#add () in
  let biggerWindow = GWindow.window ~width:1000 ~height:800 ~title:"Game" ~border_width:10 () in
  let p2 = GButton.button ~label:"2 Players" ~packing:hbox1#add () in
  let _ = p2#connect#clicked ~callback: (fun () -> selectHumanPlayers 2 r main_vbox window) in
  let p3 = GButton.button ~label:"3 Players" ~packing:hbox1#add () in
  let _ = p3#connect#clicked ~callback: (fun () -> selectHumanPlayers 3 r main_vbox window) in
  let p4 = GButton.button ~label:"4 Players" ~packing:hbox2#add () in
  let _ = p4#connect#clicked ~callback: (fun () -> selectHumanPlayers 4 r main_vbox window) in
  let p5 = GButton.button ~label:"5 Players" ~packing:hbox2#add () in
  let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 5 0 r main_vbox) in
  let _ = window#show () in
  (* Enter the event loop *)
  let _ = GMain.Main.main () in
  !r

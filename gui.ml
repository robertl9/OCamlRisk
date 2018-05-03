(* open State *)
let _ = GtkMain.Main.init ()
let rec selectPlayers num vbox window=
  let _ = vbox#destroy () in
  let main_hbox = GPack.hbox ~packing:window#add ~width:1000 ~height:800 () in
  let left_side = GPack.vbox ~packing:main_hbox#add ~width:700 ~height:800 () in
  let right_side = GPack.vbox ~packing:main_hbox#add ~width:300 ~height:800 () in
  let game_board = GPack.vbox ~packing:left_side#add ~width:700 ~height:600 () in
  let left_bottom = GPack.hbox ~packing:left_side#add ~width:700 ~height:200 () in
  let _ = GMisc.label ~text:"Game_board" ~packing:game_board#add ~height:50 () in
  let _ = GMisc.label ~text:"Left_bottom" ~packing:left_bottom#add ~height:50 () in
  let _ = GMisc.label ~text:"Right_side" ~packing:right_side#add ~height:50 () in
  prerr_endline (string_of_int num)

let main () =
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
  let p2 = GButton.button ~label:"2 Players" ~packing:hbox1#add () in
  let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 2 main_vbox window) in
  let p3 = GButton.button ~label:"3 Players" ~packing:hbox1#add () in
  let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 3 main_vbox window) in
  let p4 = GButton.button ~label:"4 Players" ~packing:hbox2#add () in
  let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 4 main_vbox window) in
  let p5 = GButton.button ~label:"5 Players" ~packing:hbox2#add () in
  let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 5 main_vbox window) in
  window#show ();
  (* Enter the event loop *)
  GMain.Main.main ()

let _ =  main ()

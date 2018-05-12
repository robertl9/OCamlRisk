(* open State *)
open GMain
open Gtk
(* open GdkKeysyms *)
open Graphics
open Png
open Jpeg
open Camlimages
open Images
open Gdk.Color


(* open Player *)

(* open Images *)

let _ = GtkMain.Main.init ()

(* let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported" *)

(* let load_image file =
   print_endline "Load as string";
   let buf = Bytes.create (256*256*3) in
   let ic = open_in_bin file in
   really_input ic buf 0 (256*256*3);
   close_in ic;
   buf *)

(* let rgb_at buf x y =
   let offset = (y * 256 + x) * 3 in
   (int_of_char (Bytes.get buf offset),
   int_of_char (Bytes.get buf (offset+1)),
   int_of_char (Bytes.get buf (offset+2))) *)

let rec selectPlayers num_h num_ai vbox window=

  (* let g = Graphic_image.of_image img  *)

  (* let g = Graphic_image.of_image img in *)
  (* Graphics.draw_image img 0 0; *)

  let _ = vbox#destroy () in

  (* let bg = GtkWidget.gtk_image_new_from_file(const gchar *filename); *)

  let main_hbox = GPack.hbox ~packing:window#add ~width:1000 ~height:800 () in
  let game_board_box = GPack.vbox ~packing:main_hbox#add ~width:700 ~height:800 () in
  let gameBoard = GPack.fixed ~packing:game_board_box#add () in
  let _ = GMisc.image ~file: "images/trash.png" ~packing:(gameBoard#put ~x:50 ~y:200) () in
  (* let winterfellButton2 = GButton.color_button ~color: [`NAME "green"] ~title:"A" ~packing:(gameBoard#put ~x:100 ~y:90) () in *)
  let winterfellButton = GButton.button ~label:"Winterfell" ~packing:(gameBoard#put ~x:110 ~y:220) () in
  let _ =winterfellButton#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let winterfellLabel = GMisc.label ~text:"3" ~packing:(gameBoard#put ~x:125 ~y:250) () in
  let countryB = GButton.button ~label:"CountryB" ~packing:(gameBoard#put ~x:110 ~y:300) () in
  let _ = countryB#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryBLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:125 ~y:330) () in
  let countryC = GButton.button ~label:"CountryC" ~packing:(gameBoard#put ~x:160 ~y:340) () in
  let _ = countryC#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryCLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:170 ~y:365) () in
  let countryD = GButton.button ~label:"CountryD" ~packing:(gameBoard#put ~x:170 ~y:400) () in
  let _ = countryD#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryDLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:185 ~y:425) () in
  let countryE = GButton.button ~label:"CountryE" ~packing:(gameBoard#put ~x:90 ~y:430) () in
  let _ = countryE#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryELabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:100 ~y:455) () in

  let countryF = GButton.button ~label:"CountryF" ~packing:(gameBoard#put ~x:175 ~y:465) () in
  let _ = countryF#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryFLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:200 ~y:490) () in
  let countryG = GButton.button ~label:"CountryG" ~packing:(gameBoard#put ~x:115 ~y:495) () in
  let _ = countryG#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryGLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:125 ~y:532) () in

  let countryH = GButton.button ~label:"CountryH" ~packing:(gameBoard#put ~x:55 ~y:532) () in
  let _ = countryH#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryHLabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x:108 ~y:558) () in

  let countryI = GButton.button ~label:"CountryI" ~packing:(gameBoard#put ~x:155 ~y:590) () in
  let _ = countryI#connect#clicked ~callback: (fun () -> selectPlayers 2 0 vbox window) in
  let countryILabel = GMisc.label ~text:"2" ~packing:(gameBoard#put ~x: 215 ~y:595) () in

  let a_dice1 = GMisc.image ~file: "images/dice1.png" ~packing:(gameBoard#put ~x:350 ~y:200) () in
  let a_dice2 = GMisc.image ~file: "images/dice5.png" ~packing:(gameBoard#put ~x:405 ~y:200) () in
  let a_dice3 = GMisc.image ~file: "images/dice6.png" ~packing:(gameBoard#put ~x:460 ~y:200) () in

  let d_dice1 = GMisc.image ~file: "images/dice1.png" ~packing:(gameBoard#put ~x:377 ~y:300) () in
  let d_dice2 = GMisc.image ~file: "images/dice3.png" ~packing:(gameBoard#put ~x:433 ~y:300) () in ()



let selectHumanPlayers num vbox window =
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
    let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 2 0 main_vbox window) in
    let p3 = GButton.button ~label:"1 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 1 1 main_vbox window) in
    let p4 = GButton.button ~label:"0 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 0 2 main_vbox window) in ()
  | 3 ->
    let title = GMisc.label ~text:"3 Players Selected." ~packing:main_vbox#add ~height:50 () in
    let p2 = GButton.button ~label:"3 Human & 0 AI Players" ~packing:hbox1#add () in
    let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 3 0 main_vbox window) in
    let p3 = GButton.button ~label:"2 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 2 1 main_vbox window) in
    let p4 = GButton.button ~label:"1 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 1 2 main_vbox window) in
    let p5 = GButton.button ~label:"0 Humans & 3 AI Players" ~packing:hbox2#add () in
    let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 0 0 main_vbox window) in ()
  | 4 ->
    let title = GMisc.label ~text:"4 Players Selected." ~packing:main_vbox#add ~height:50 () in
    let p2 = GButton.button ~label:"4 Humans & 0 AI Players" ~packing:hbox1#add () in
    let _ =p2#connect#clicked ~callback: (fun () -> selectPlayers 4 0 main_vbox window) in
    let p3 = GButton.button ~label:"3 Human & 1 AI Player" ~packing:hbox1#add () in
    let _ = p3#connect#clicked ~callback: (fun () -> selectPlayers 3 1 main_vbox window) in
    let p4 = GButton.button ~label:"2 Humans & 2 AI Players" ~packing:hbox2#add () in
    let _ = p4#connect#clicked ~callback: (fun () -> selectPlayers 2 2 main_vbox window) in
    let p5 = GButton.button ~label:"1 Humans & 3 AI Players" ~packing:hbox2#add () in
    let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 1 3 main_vbox window) in
    let p6 = GButton.button ~label:"0 Humans & 4 AI Players" ~packing:hbox2#add () in
    let _ =p6#connect#clicked ~callback: (fun () -> selectPlayers 0 4 main_vbox window) in ()
  | _ -> failwith ("Cannot be possible!")

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
  let biggerWindow = GWindow.window ~width:1000 ~height:800 ~title:"Game" ~border_width:10 () in
  let p2 = GButton.button ~label:"2 Players" ~packing:hbox1#add () in
  let _ =p2#connect#clicked ~callback: (fun () -> selectHumanPlayers 2 main_vbox window) in
  let p3 = GButton.button ~label:"3 Players" ~packing:hbox1#add () in
  let _ = p3#connect#clicked ~callback: (fun () -> selectHumanPlayers 3 main_vbox window) in
  let p4 = GButton.button ~label:"4 Players" ~packing:hbox2#add () in
  let _ = p4#connect#clicked ~callback: (fun () -> selectHumanPlayers 4 main_vbox window) in
  let p5 = GButton.button ~label:"5 Players" ~packing:hbox2#add () in
  let _ =p5#connect#clicked ~callback: (fun () -> selectPlayers 5 0 main_vbox window) in

  (* let menubar = GMenu.menu_bar ~packing:main_vbox#pack () in
     let factory = new GMenu.factory menubar in
     let accel_group = factory#accel_group in
     let file_menu = factory#add_submenu "File" in *)

  (* File menu *)
  (* let factory = new GMenu.factory file_menu ~accel_group in
     factory#add_item "Quit" ~key:_Q ~callback: Main.quit;
     window#add_accel_group accel_group; *)
  window#show ();
  (* Enter the event loop *)
  GMain.Main.main ()

let _ =  main ()







(* open Graphics

let white = rgb 255 255 255
let blue  = rgb 30 25 255

(* no function for converting color back to rgb in Graphics *)
let color_to_rgb color =
    let r = (color land 0xFF0000) asr 0x10
    and g = (color land 0x00FF00) asr 0x8
    and b = (color land 0x0000FF)
    in r, g, b

let open_window =
    open_graph " 640x480";
    set_window_title "GraphicsExample"

(* no way of setting background color; resizing shows white *)
let clear_window color =
    let fg = foreground
    in
        set_color color;
        fill_rect 0 0 (size_x ()) (size_y ());
        set_color fg

(* create a gradient of colors from black at 0,0 to white at w-1,h-1 *)
let gradient arr w h =
    for y = 0 to h-1 do
        for x = 0 to w-1 do
            let s = 255 * (x+y) / (w+h-2)
            in arr.(y).(x) <- rgb s s s
        done
    done

let draw_gradient x y w h =
    (* w and h are flipped from perspective of the matrix *)
    let arr = Array.make_matrix h w white
    in
        gradient arr w h;
        draw_image (make_image arr) 0 0

let rec event_loop wx wy =
    (* there's no resize event so polling in required *)
    let _ = wait_next_event [Poll]
    and wx' = size_x () and wy' = size_y ()
    in
        if wx' <> wx || wy' <> wy then
            begin
                clear_window blue;
                draw_gradient 0 0 200 100
            end;
        Unix.sleep 1;
        event_loop wx' wy'

let () =
    open_window;
    let r,g,b = color_to_rgb background
    in
        Printf.printf "Background color: %d %d %d\n" r g b;
        try event_loop 0 0
        with Graphic_failure _ -> print_endline "Exiting..."



(* open State *)
(* open GMain
open Graphics
open Png
open Jpeg
open Camlimages
open Images


(* open Player *)

(* open Images *)

let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
      let w = bitmap.Index8.width
      and h = bitmap.Index8.height
      and colormap = bitmap.Index8.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
      if bitmap.Index8.transparent <> -1 then
        cmap.(bitmap.Index8.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
      let w = bitmap.Index16.width
      and h = bitmap.Index16.height
      and colormap = bitmap.Index16.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
      if bitmap.Index16.transparent <> -1 then
        cmap.(bitmap.Index16.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
      let w = bitmap.Rgb24.width
      and h = bitmap.Rgb24.height in
      Array.init h (fun i ->
        Array.init w (fun j ->
          let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
          rgb r g b))
  | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported"

let rec loop () = loop ()

let main () =
  let image = Png.load_as_rgb24 ("westeros.png") [] in
  let converted = array_of_image image in
  let fin_img = Graphics.make_image converted in
  Graphics.draw_image fin_img 0 0;
  loop () *)

let _ =
  Graphics.open_window
  (* main (); *) *)

(* [init_gui] passes the value of the button chosen by
 * the player in the selectHumanPlayers screen and draws the screen on the
 * window. *)
val init_gui: unit -> string

(* [draw window cl st] draws the entire updated map on the window *)
val draw: GWindow.window -> string list -> State.state -> unit

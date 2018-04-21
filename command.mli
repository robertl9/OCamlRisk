
type country 

(* [command] represents a command input by a player which is taken 
 * from the user interaction with the GUI. There are 3 possible commands 
 * and each involves pressing a country and possibly putting a number of 
 * troops on that location.
*)
type actions = Deploy of (int * country) | 
               Reinforce of (int * country) | Attack of country | End 
type command = actions

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 *   writeup. *)
val parse : string -> command


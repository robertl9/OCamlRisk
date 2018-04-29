
(* [command] represents a command input by a player which is taken 
 * from the user interaction with the GUI. There are 3 possible commands 
 * and each involves pressing a country and possibly putting a number of 
 * troops on that location.
*)
type actions = Deploy of (int * country) | 
               Reinforce of (int * country * country) | Attack of (country*country) | End 
               | Quit | Ally of string | Inv | Error of string


type command = actions

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 * writeup:
 * attack [ctr1] [ctr2]          --> Attack country ctr 
 * deploy [num] [ctr]            --> Deploy num troops on country ctr
 * reinforce [num] [ctr1] [ctr2] --> Place troops from ctr1 on ctr2 
 * ally [plr]                    --> Ally request with player plr
 * inventory                     --> Highlights player's list of cards and alliances
 * quit                          --> Player quits from game 
 *)
let parse_helper str1 str2 str3 str4= 
	match str1 with
	| "attack" -> Attack(str2, str3)
	| "deploy" -> Deploy(int_of_string(str2), str3)
	| "reinforce" -> Reinforce(int_of_string(str2), str3, str4)
	| "ally" -> Ally(str1)
	| "quit"  -> Quit
	| "inv" -> Inv
	|  _ -> Error("Not a valid game action")

let parse str = 
	(*get GUI input here*)
	failwith "Unimplemented"







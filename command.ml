
(* [command] represents a command input by a player which is taken 
 * from the user interaction with the GUI. There are 3 possible commands 
 * and each involves pressing a country and possibly putting a number of 
 * troops on that location.
*)
type actions = Deploy of (int * string) | 
               Reinforce of (int * string * string) | Attack of (string*string) 
               | Quit | Ally of string | Inv | Error of string 
               | Claim of string


type command = actions

(* [parse] takes in a command and creates the corresponding variant
 * requires: [s] is one of the commands forms described in the
 * writeup:
 * attack [ctr1] [ctr2]          --> Attack ctr2 using ctr1
 * deploy [num] [ctr]            --> Deploy num troops on country ctr
 * reinforce [num] [ctr1] [ctr2] --> Place troops from ctr1 on ctr2 
 * ally [plr]                    --> Ally request with player plr
 * inv                           --> Highlights player's list of cards and alliances
 * quit                          --> Player quits from game 
 * claim [ctr]                   --> Current player claims ctr
 *)
let parse str = 
	(*get GUI input here*)
	let str_list = String.split_on_char (Char.chr 32) str in 
	match str_list with
	| a::b::c::d::e -> 
		if String.lowercase_ascii(a) = "reinforce" then Reinforce(int_of_string(b), c, d)
		else Error("Invalid game action")
	| a::b::c::d -> 
		if String.lowercase_ascii(a) = "attack" then Attack(b, c)
		else if String.lowercase_ascii(a) = "deploy" then Deploy(int_of_string(b),c)
		else Error("Invalid game action")
	| a::b::c ->
		if String.lowercase_ascii(a) = "claim" then Claim(b)
		else Error("Invalid game action")
	| a::b -> 
		if String.lowercase_ascii(a) = "inv" then Inv
		else if  String.lowercase_ascii(a) = "quit" then Quit
		else Error("Invalid game action")
	| _ -> Error("No action given")





(* [command] represents a command input by a player which is taken
 * from the user interaction with the GUI. There are 3 possible commands
 * and each involves pressing a country and possibly putting a number of
 * troops on that location.
*)

type actions = | DeployC of int * string | TradeC of string * string * string|
                   ReinforceC of int * string * string
               | AttackC of string * string | QuitC | AllyC of string
               | ErrorC of string | ClaimC of string | EndPhaseC

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
		if String.lowercase_ascii(a) = "reinforce" then ReinforceC(int_of_string(b), c, d)
		else ErrorC("Invalid game action")
	| a::b::c::d ->
		if String.lowercase_ascii(a) = "attack" then AttackC(b, c)
		else if String.lowercase_ascii(a) = "deploy" then DeployC(int_of_string(b),c)
		else ErrorC("Invalid game action")
	| a::b::c ->
   if String.lowercase_ascii(a) = "claim" then ClaimC(b)
   else if String.lowercase_ascii(a) = "end" then EndPhaseC
   else ErrorC("Invalid game action")
	| a::b ->
   if  String.lowercase_ascii(a) = "quit" then QuitC
   else if String.lowercase_ascii(a) = "end" then EndPhaseC
   else ErrorC("Invalid game action")
	| _ -> ErrorC("No action given")

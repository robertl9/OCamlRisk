
(* [command] represents a command input by a player which is taken
 * from the user interaction with the GUI. There are 3 possible commands
 * and each involves pressing a country and possibly putting a number of
 * troops on that location.
*)

type card = BannerMan | Lord | Dragon | WildCard

type actions = | DeployC of int * string | Trade of card * card * card |
                 ReinforceC of int * string * string
               | AttackC of string * string | QuitC | AllyC of string
               | ErrorC of string | ClaimC of string | EndPhaseC

type command = actions

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 *   writeup. *)
val parse : string -> command

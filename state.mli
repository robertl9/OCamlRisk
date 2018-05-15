

(* [state] is an abstract type representing the state of the Risk game*)
type state

(* [country] is an abstract type representing country within the game*)
type country

(* [player] is an abstract type representing a player in the game*)
type player

(* [card] is an abstract type representing a card in the game*)
type card = BannerMan | Lord | Dragon | WildCard

type continent

(* [t_phase] is an abstract type representing the phase during the game portion*)
type t_phase = Deploy | Attack | Reinforce

(* [phase] is an abstract type representing the phase in a game*)
type phase = SetUp | Game of t_phase

(* [taken s p] returns a list representing the countries in s
 * that are held by a player p
*)
val taken_by: state -> player -> string list

(* [available s] is a list of countries
 * that are not taken by a player, on the board
*)
val available: state -> string list

(* [continents] list of continent names*)
(* val continents: state -> string list *)

(* [countries] list of country names*)
(* val countries: state -> string list *)

(* [exits c] returns a list of the names of countries that neighbor c *)
val exits: country -> string list

(* [num_troops s p] *)
(* val num_troops: state -> int *)

(* [turns_by p] is a number representing the number of turns taken by a player.
 * A turn consists of three stages: p deploying their troops, p declaring an attack
 * on a country held by another player, and p reinforcing their troops
*)
(* val turns_by: player -> int *)

(* [win s] returns a state in which the player who owns all of the
 * countries in the state wins the game. If no such player exists, the
 * same state is returned.
*)
val win: state -> bool

(* [cards_owned p] returns a list of cards representing cards owned by p*)
val cards_owned: player -> card list

val get_unclaimed: state -> string list

val get_neighbors: country -> string list

val get_num_deploy: player -> int

val get_player_countries: player -> (string * int) list

val get_country_id: country -> string

val get_player_list: state -> player list

val get_country_content: country -> string

(* [getPhase st] returns the current phase
 * requires: st is a state
*)
val getPhase: state -> phase

(* [getPhaseString st] returns the current phase as a string
 * requires: st is a state
*)
val getPhaseString: state -> string

val get_continent_id: continent -> string

val get_continents: player -> continent list

(* [getCountryTroops st] returns association list of country id and troops
 * requires: st is a state
*)
val getCountryTroops: state -> (string * int) list

(* [getAttackDice st] returns a list of numbers rolled on attack dice
 * requires: st is a state
*)
val getAttackDice: state -> int list

(* [getDefendDice st] returns a list of numbers rolled on defend dice
 * requires: st is a state
*)
val getDefendDice: state -> int list

val get_countries: state -> country list

(* [printOrder st] returns string or the players order
 * requires: st is a state
*)
val printOrder: state -> string

val calc_troops: player -> int

(* [remove_card s c] returns a state that has a
 * a card list that does not contain c
*)
(* val remove_card: state -> card -> state *)

(* [cards_free s] returns a list of cards not held by any player
*)
val cards_free: state -> card array

(* [get_cplayer s] returns the string id of the current player
 * requires: s is a state
*)
val get_cplayer: state -> string

(* [get_player_of_state s] returns the player record of the current player
 * requires: s is a state
*)
val get_player_of_state: state -> player

(* [get_player_by_id s p] returns the player record associated with the id p
 * requires: s is a state
             p is a string
*)
val get_player_by_id: state -> string -> player

(* [print_state s] returns a string of various information to print
 * requires: s is a state
*)
val print_state: state -> string

(* [do' cmd s] returns a new state after executing actions associated with cmd
 * requires: cmd is a Command.command
             s is a state
*)
val do': Command.command -> state -> state

(* [get_msg s] returns the msg in the record s
 * requires: s is a state
*)
val get_msg: state -> string

(* [init_state p aie aim aih json] returns an initialized state
 * requires: p is an int of human players
             aie is an int of easy ai players
             aim is an int of medium ai players
             aih is an int of hard ai players
             json is a valid json
*)
val init_state: int -> int -> int -> int -> Yojson.Basic.json -> state

val get_country: string -> country list -> country

val country_owned_by_player: state -> string -> string

(* [get_win_msg s] returns the win message in the record s
 * requires: s is a state
*)
val get_win_msg: state -> string

val get_player_by_id: state -> string -> player

(* [get_player_id p] returns id associated with player p
 * requires: p is a player
*)
val get_player_id:  player -> string

(* [get_troops s p] returns num troops in country id s owned by player p
 * requires: s is a string representing a country id
             p is a player
*)
val get_troops: string -> player -> int

val get_conts_on: player -> country list -> string list

val find_owner: string -> state -> player

(* [reinforcable c1 c2 neighbors cl st] returns boolean if c1 and c2 are
                                         connected by a path owned by the player
 * requires: st is a state
             c1 is a country id
             c2 is a country id
             neighbors is a country list
             cl is a country list
*)
val reinforcable: string ->string ->
           country list -> country list -> country list -> state -> bool

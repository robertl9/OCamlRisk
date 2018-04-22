open Yojson.Basic.Util
open Hashtbl
open Random

type country

type continent

type character = JonSnow | DaenerysTargareyan | NightKing | Bran

type card = BannerMan | Lord | Dragon | WildCard

type phase = Reinforce | Deploy | Attack

type player = {
  character: character;
  continents: continent list;
  countries: (country * int) list;
  cards: card list;
}

type state = {
  players: player list;
  c_player: player;
  c_phase: phase;
  continents: (continent * (country * player list)) list;
  card_l: card list;
}

let roll n =
  let rl = [] in
  for x=1 to n do (((Random.int 5) + 1)::rl) done

(* [taken s p] returns a list representing the countries in s
 * that are held by a player p
 *)
val taken_by: state -> player -> string list

(* [available s] is a list of countries
 * that are not taken by a player, on teh board
 *)
val available: state -> string list

(* [continents] list of continent names*)
val continents: state -> string list

(* [countries] list of country names*)
val countries: state -> string list

(* [exits c] returns a list of the names of countries that neighbor c *)
val exits: country -> string list

(* [num_troops s p] *)
val num_troops: state -> int

(* [turns_by p] is a number representing the number of turns taken by a player.
 * A turn consists of three stages: p deploying their troops, p declaring an attack
 * on a country held by another player, and p reinforcing their troops
 *)
val turns_by: player -> int

(* [win s] returns a state in which the player who owns all of the
 * countries in the state wins the game. If no such player exists, the
 * same state is returned.
 *)
val win: state -> state

(* [cards_owned p] returns a list of strings representing cards owned by p*)
val cards_owned: player -> string list

(* [remove_card s c] returns a state that has a
 * a card list that does not contain c
 *)
val remove_card: state -> card -> state

(* [cards_free s] returns a list of cards not held by any player
 *)
val cards_free: state -> string list

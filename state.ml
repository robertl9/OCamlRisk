open Yojson.Basic.Util

type country = {
  country_id: string ;
  exits: string list;
  c_id: string ;
}

type continent = {
  continent_id : string ;
  continent_list: string list;
  bonus_troops: int
}

type character = JonSnow | DaenerysTargareyan | NightKing | Bran

type card = BannerMan | Lord | Dragon | WildCard

type phase = Start | Reinforce | Deploy | Attack

type player = {
  character: character;
  continents: continent list;
  countries_held: (country * int) list;
  cards: card list;
}

type state = {
  players_list: player list;
  (* characters_list: character list; *)
  current_turn: string;               (* string representing id of character's turn *)
  c_phase: phase;
  continents: continent list;
  countries: country list;
  card_l: card array;
  (* num_troops_left: int; *)
  (* phase: phase ; *)
  fog_of_war: bool;
  w_msg: string;
}

let roll n =
  let rl = [] in
  let rec roll_n n l =
    if (n = 0) then (l) else (roll_n (n-1) ((Random.int 5 + 1)::l))
  in roll_n n rl

let roll_m n =
  let rl = Array.make n 0 in
  for i = 0 to n-1 do rl.(i)<-(Random.int 5 + 1) done

let init_characters () =
  [Bran; NightKing; DaenerysTargareyan; JonSnow]

let init_cards () =
  let bm = Array.make 15 BannerMan in
  let l = Array.make 15 Lord in
  let d = Array.make 15 Dragon in
  let wc = Array.make 2 WildCard in
  let bml = Array.append bm l in
  let bmld = Array.append bml d in
  let deck = Array.append bmld wc in
  deck

let init_state j =
  let continents = j|> member "continents" |> to_list in
  let countries = j|> member "countries" |> to_list in
  let win = j|> member "win_message" |> to_string in
  {players_list = []; current_turn = ""; c_phase = Start;
   continents = continents; countries = countries; card_l = init_cards ();
   fog_of_war = false; w_msg = win;}



(* [taken s p] returns a list representing the countries in s
 * that are held by a player p
 *)
(* val taken_by: state -> player -> string list *)

(* [available s] is a list of countries
 * that are not taken by a player, on teh board
 *)
(* val available: state -> string list *)

(* [continents] list of continent names*)
(* val continents: state -> string list *)

(* [countries] list of country names*)
(* val countries: state -> string list *)

(* [exits c] returns a list of the names of countries that neighbor c *)
(* val exits: country -> string list *)

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
(* val win: state -> state *)

(* [cards_owned p] returns a list of strings representing cards owned by p*)
(* val cards_owned: player -> string list *)

(* [remove_card s c] returns a state that has a
 * a card list that does not contain c
 *)
(* val remove_card: state -> card -> state *)

(* [cards_free s] returns a list of cards not held by any player
 *)
(* val cards_free: state -> string list *)

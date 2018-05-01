open Yojson.Basic.Util

type country = {
  country_id: string ;
  neighbors: string list;
  c_id: string ;
}

type continent = {
  continent_id : string ;
  country_list: string list;
  bonus_troops: int
}

type character = JonSnow | DaenerysTargareyan | NightKing | Bran

type card = BannerMan | Lord | Dragon | WildCard

type phase = Start | Reinforce | Deploy | Attack

type player = {
  id: string;
  character: character;
  continents: continent list;
  mutable countries_held: (country * int) list;
  mutable cards: card list;
}

type state = {
  mutable players_list: player list;
  mutable c_turn: string;
  c_phase: phase;
  continents: continent list;
  countries: country list;
  mutable card_l: card array;
  fog_of_war: bool;
  mutable repl_msg: string;
  w_msg: string;
}

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
  {players_list = []; c_turn = ""; c_phase = Start;
   continents = continents; countries = countries; card_l = init_cards ();
   fog_of_war = false; w_msg = win; repl_msg = "Welcome to Risk!"}

let add_player id character st =
  let player = {id = id; character = character; continents = [];
                countries_held = []; cards = [];} in
  let _ = st.players_list <- player::st.players_list in st

let rec get_player p pl npl =
  match pl with
  | [] -> failwith "Invalid Player"
  | h::t -> if h.id = p then h, t@npl else get_player p t (h::npl)

let get_country str_country state = 
  List.find (fun x -> x = str_country) state.countries

let rec get_defender c pl npl =
  let f x (k,v) = if c = k then true else x || false in
  match pl with
  | [] -> failwith "Invalid Country"
  | h::t -> let boolean = List.fold_left f false h.countries_held in
    if boolean then h, t@npl else get_defender c t (h::npl)

(* let roll n =
  let rl = [] in
  let rec roll_n n l =
    if (n = 0) then (l) else (roll_n (n-1) ((Random.int 5 + 1)::l))
  in roll_n n rl *)

let roll n =
  let rl = Array.make n 0 in
  for i = 0 to n-1 do rl.(i)<-(Random.int 5 + 1) done; rl

let get_troops c p =
  let f x (k,v) = if c = k then v else x + 0 in
  List.fold_left f 0 p.countries_held

let inc_troop c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v+1)::t else inc c t
  in let _ = player.countries_held <- (inc c player.countries_held) in
  let _ = st.repl_message <- c2.id ^ " has gained " ^ "one troop!" in 
  let _ = st.players_list <- (player::pl) in st

let dec_troop c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v-1)::t else inc c t
  in let _ = player.countries_held <- (inc c player.countries_held) in
  let _ = st.repl_message <- c.id ^ " has lost" ^ " one troop!" in 
  let _ = st.players_list <- (player::pl) in st

let draw_card st =
  let len = Array.length st.card_l in
  let c = st.card_l.(Random.int len) in
  (* Remove Card *)
  let player, pl = get_player st.c_turn st.players_list [] in
  let _ = player.cards <- c::player.cards in
  let _ = st.players_list <- (player::pl) in st

let rec max_e l =
  match l with
  | [] -> failwith "Invalid Roll"
  | h::[] -> h
  | h::t -> max h (max_e t)

let conquer a d pl c t st =
  let f (k,v) = k <> c in
  let _ = a.countries_held <- ((c,t)::a.countries_held) in
  let _ = d.countries_held <- List.filter f d.countries_held in
  let _ = st.repl_message <- a.id ^ " has conquered " ^ c.country_id "!" in 
  let _ = st.players_list <- (a::d::pl) in st

let attack c c2 st =
  let attacker, pl = get_player st.c_turn st.players_list [] in
  let defender, pl = get_defender c2 pl [] in
  let a_troops = get_troops c attacker in
  let d_troops = get_troops c2 defender in
  let a_roll = if a_troops > 3 then roll 3 else
    if a_troops = 1 then failwith "Insufficient Troops" else roll (a_troops-1) in
  let d_roll = if d_troops >= 2 then roll 2 else roll d_troops in
  match a_roll, d_roll with
  | [|x|], [|x2|] -> if x > x2
    then if d_troops = 1
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop c2 st (* Decrement Defense Troop *)
    else dec_troop c st (* Decerement Attack Troop *)
  | [|x; x'|], [|x2|] -> let max_x = max_e [x;x'] in
    if max_x > x2
    then if d_troops = 1
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop c2 st (* Decrement Defense Troop *)
    else dec_troop c st (* Decerement Attack Troop *)
  | [|x; x'; x''|], [|x2|] -> let max_x = max_e [x;x';x''] in
    if max_x > x2
    then if d_troops = 1
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop c2 st (* Decrement Defense Troop *)
    else dec_troop c st (* Decerement Attack Troop *)
  | [|x|], [|x2; x2'|] -> let max_x = max_e [x2;x2'] in
    if x > max_x
    then dec_troop c2 st (* Decrement Defense Troop *)
    else dec_troop c st (* Decerement Attack Troop *)
  | [|x; x'|], [|x2; x2'|] ->
  | [|x; x'; x''|], [|x2; x2'|] ->
  | _, _ -> failwith "Program Failure"

let deploy str_country state = 
  let current_plyr = fst (get_player state.c_turn state.players_list []) in 
  let ctry = get_country str_country state in 
 
  let _ = state.repl_msg <- state.c_turn ^ " deploys 1 troop on " ^ str_country in
  (* check to see if current player already has country 
   * if not, add that country to countries held
   *)
   if List.mem_assoc ctry current_plyr.countries_held 
   then inc_troop ctry state
   else current_plyr.countries_held <- (ctry, 1)::(current_plyr.countries_held)

let reinforce num country_dec country_inc state = 

  let current_plyr = fst (get_player state.c_turn state.players_list []) in 

  (* checks to see if a link exists between two countries *)
  let rec check_links visited frontier = 
    if frontier = [] then false 
    else
      let popped, frontier, new_visited = 
        (* pops country off of frontier*)
        match frontier with 
        | (ctr, num)::b -> ctr, b, ctr.country_id::visited
        (* methord would return false in match below 
          before calling check_links with empty frontier *)
        | _ -> failwith "Program failure" in

      (* adding neighboring countries to frontier that are not already in frontier, 
       * that are held by current player, and that have not been visited yet*)
      let new_frontier = (List.filter (fun x -> 
        (List.mem_assoc x current_plyr.countries_held) && 
        not (List.mem (fst x).country_id visited) 
        || (List.mem (fst x) frontier)) popped.neighbors)@frontier in 
      if popped.country_id = country_inc.country_id then true 
      else check_links new_visited new_frontier in

  (* if there's a link, reinforce, otherwise return error message*)
  if check_links [] [country_dec] then 
    let str_troops = if num = 1 then "troop" else "troops" in 
    let _ = state.repl_msg <- state.c_turn ^ " moved " 
      ^ string_of_int(num) ^ " " ^ str_troops ^ " from " ^ str_country_dec 
      ^ " to " ^ str_country_inc in
    let _ = for x = num downto 0 do inc_troop ctry_inc state done in 
    let _ = for x = num downto 0 do dec_troop ctry_dec state done 
  else 
    let _ = state.repl_msg <- "Cannot reinforce using these two countries. Try again!"

(* return list of owned countries to player (most likely not needed for GUI) *)
let inv_helper state =
  let current_plyr = fst (get_player state.c_turn state.players_list []) in 
  let country_ids = List.map (fun x -> (fst x).country_id) current_plyr.countries_held in
  let _ = state.repl_msg <- List.fold_left (^) "You own the following countries: " country_ids


(* remove player from game, re-distribute their countries to remaining player*)
let quit_helper state = 
  let current_plyr = fst (get_player state.c_turn state.players_list []) in 

  (*free players properties, re-deploy to remaining players*)
  
  let _ = state.players_list <- List.remove current_plyr 

(*changes the game state based on the GUI input*)
let do' act state = 
  match act with
  | Attack(ctr1, ctr2) -> attack get_country(ctr1 state) get_country(ctr2 state) state
  | Deploy(num, ctr) -> deploy get_country(ctr state) state
  | Reinforce(num, ctr1, ctr2) -> 
    reinforce num get_country(ctr1 state) get_country(ctr2 state) state
  (* | Ally(str) -> ally str state *)
  | Quit -> quit_helper state 
  | Inv -> inv_helper state  
  | _ -> failwith "Action not implemented"


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








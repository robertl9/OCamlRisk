open Command
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
  mutable countries_held: (string * int) list;
  mutable cards: card list;
}

type state = {
  mutable players_list: player list;
  mutable c_turn: string;
  mutable turns: string array;
  mutable turn: int;
  mutable c_phase: phase;
  continents: continent list;
  countries: country list;
  mutable unclaimed: string list;
  mutable card_l: card array;
  fog_of_war: string;
  mutable repl_msg: string;
  w_msg: string;
}

(* num of initial amount of troops allowed on board
 * subject to change
 *)
let init_troops = 30

let get_msg st =
  st.repl_msg

let head_of_lst lst =
  match lst with
  | [] -> []
  | h::t -> h

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

(* take json element a and create continent record*)
let to_continents a =
  let id = a |> member "id" |> to_string in
  let bonus = a |> member "bonus" |> to_string |> int_of_string in
  let c_list = [] in (*no JSON element for country list*)
  {continent_id = id; bonus_troops = bonus; country_list = c_list}

(* take json element a and create country record*)
let to_countries a =
  let id = a |> member "id" |> to_string in
  let neighbor_lst = a |> member "neighbors" |> to_list |> List.map to_string in
  let cont_id = a |> member "continent" |> to_string in
  {country_id = id; neighbors = neighbor_lst; c_id = cont_id}

let rec players n l =
  if n == 0 then l
  else let player = {id = string_of_int n; character = JonSnow; continents = [];
                     countries_held = []; cards = [];} in
    let npl = player::l in
    players (n-1) npl

let order n =
  let ol = Array.make n "" in
  for i = 0 to n-1 do ol.(i)<-(string_of_int (i+1)) done; ol

let init_state n j =
  let continents = j|> member "continents" |> to_list |> List.map to_continents in
  let countries = j|> member "countries" |> to_list |> List.map to_countries in
  let u_countries = let f x = (String.uppercase_ascii x.country_id) in List.map f countries in
  let repl_msg = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!" in
  let fog_of_war = j |> member "fog_of_war" |> to_string in
  let win = j|> member "win_message" |> to_string in
  {players_list = players n []; c_turn = "1"; turns = order n; turn = 0; c_phase = Start;
   continents = continents; countries = countries; unclaimed = u_countries;
   card_l = init_cards (); fog_of_war = fog_of_war; w_msg = win; repl_msg = repl_msg}
(* "Welcome to Risk!" *)

let add_player id character st =
  let player = {id = id; character = character; continents = [];
                countries_held = []; cards = [];} in
  let _ = st.players_list <- player::st.players_list in st

let rec get_player p pl npl =
  match pl with
  | [] -> failwith "Invalid Player"
  | h::t -> if h.id = p then h, t@npl else get_player p t (h::npl)

let get_cplayer st =
  st.c_turn

let rec remove_l c l =
  match l with
  | [] -> l
  | h::t -> if (h = c) then t else h::(remove_l c t)

(* let rec continent countryl contnentl = *)

let pick_country c st =
  if (List.mem c st.unclaimed) then
    let player, pl = get_player st.c_turn st.players_list [] in
    let _ = player.countries_held <- (c, 1)::player.countries_held in
    (* let _ = add continent if feasible *)
    let _ = st.unclaimed <- (remove_l c st.unclaimed) in
    let _ = st.turn <- st.turn + 1 in
    let _ = st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)) in
    let _ = st.repl_msg <- player.id ^ " has claimed " ^ c in
    let _ = st.players_list <- (player::pl) in st
  else let _ = st.repl_msg <- "Invalid Country/Country Taken" in st

let get_country str_country state =
    List.find (fun x -> x.country_id = str_country) state.countries

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

(*returns sum of all troops held by a player*)
let sum_troops p =
  List.fold_left (+) 0 (List.map (fun x -> snd x) p.countries_held)

let inc_troop c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v+1)::t else inc c t
  in let _ = player.countries_held <- (inc c player.countries_held) in
  let _ = st.repl_msg <- c ^ " has gained " ^ "one troop!" in
  let _ = st.players_list <- (player::pl) in st

let dec_troop c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v-1)::t else inc c t
  in let _ = player.countries_held <- (inc c player.countries_held) in
  let _ = st.repl_msg <- c ^ " has lost" ^ " one troop!" in
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
  let _ = st.repl_msg <- a.id ^ " has conquered " ^ c ^ "!" in
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
  (* | [|x; x'|], [|x2; x2'|] ->
  | [|x; x'; x''|], [|x2; x2'|] -> *)
  | _, _ -> let _ = print_int (Array.length a_roll) in
    let _ = print_int (Array.length d_roll) in
    failwith "Program Failure"

(* let deploy ctry st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let _ = st.repl_msg <- st.c_turn ^ " deploys 1 troop on " ^ ctry.country_id in
    (* check to see if current player already has country
     * if not, add that country to countries held *)
  if List.mem_assoc ctry player.countries_held
  then let st = inc_troop ctry st in
    let _ = st.c_phase <- Attack in st
  else let _ = player.countries_held <- (ctry, 1)::(player.countries_held) in
    let _ = st.repl_msg <- player.id ^ " has claimed " ^ ctry.country_id in
    let _ = st.players_list <- (player::pl) in
    let _ = st.c_phase <- Attack in st *)

(* let claim c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  if List.mem_assoc c player.countries_held then
    let st = inc_troop c st in
    (*check num troop remaining, for now max initial troops is 30*)
    let troops_on_board = List.fold_left (+) 0 (List.map sum_troops st.players_list) in
    if troops_on_board = init_troops then
      let _ = st.c_phase <- Deploy in st
      (*let _ = st.c_turn <- "next person's player id" in *)
    else st
  else
    let st = deploy c st in
    let troops_on_board = List.fold_left (+) 0 (List.map sum_troops st.players_list) in
    if troops_on_board = init_troops then
      let _ = st.c_phase <- Deploy in
      let _ = st.unclaimed <- List.filter (fun x -> x != c) st.unclaimed in st
      (*let _ = st.c_turn <- "next person's player id" in *)
    else st *)

(* let reinforce num country_dec country_inc st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let keep_in x =
    (List.mem x (List.map (fun x -> (fst x).country_id) player.countries_held))
  in
  let rec update_troops str ctry num st =
    if num = 0 then st
    else
     if str = "dec" then
      let new_st = dec_troop ctry st in
      update_troops str ctry (num - 1) new_st
     else
      let new_st = inc_troop ctry st in
      update_troops str ctry (num - 1) new_st in
    (* checks to see if a link exists between two countries *)
    let rec check_links visited frontier =
      if frontier = [] then false
      else
        let popped, frontier, new_visited =
          (* pops country off of frontier*)
          match frontier with
          | ctry_id::b -> (get_country ctry_id st), b, ctry_id::visited
          (* method would return false in match above
            before calling check_links with empty frontier *)
          | _ -> failwith "Program failure" in

        (* adding neighboring countries to frontier that are not already in frontier,
         * that are held by current player, and that have not been visited yet*)
        let new_frontier =
          (List.filter (fun x -> (keep_in x)) popped.neighbors)@frontier in
        if popped.country_id = country_inc.country_id then true
        else check_links new_visited new_frontier in *)

  (* if there's a link, reinforce, otherwise return error message*)
  (* if check_links [] [country_dec.country_id] then
  let st = update_troops "inc" country_inc num (update_troops "dec" country_dec num st) in
  let str_troops = if num = 1 then "troop" else "troops" in
  let _ = st.repl_msg <- st.c_turn ^ " moved "
                         ^ string_of_int(num) ^ " " ^ str_troops ^ " from " ^ country_dec.country_id
                         ^ " to " ^ country_inc.country_id in
  let _ = st.c_phase <- Deploy (*change c_turn*) in st
  else
    let _ = st.repl_msg <- "Cannot reinforce using these two countries. Try again!" in st *)

(* return list of owned countries to player (most likely not needed for GUI) *)
(* let inv_helper state =
  let current_plyr, pl = get_player state.c_turn state.players_list [] in
  let country_ids = List.map (fun x -> (fst x).country_id) current_plyr.countries_held in
  let _ = state.repl_msg <- List.fold_left (^) "You own the following countries: " country_ids in
  state *)

(* remove player from game, re-distribute their countries to remaining player*)
let quit_helper st =
  let player, pl = get_player st.c_turn st.players_list [] in
  (*free players properties, re-deploy to remaining players*)
  let _ = st.players_list <- List.filter (fun x -> x != player) st.players_list
  in st

let rec string_of_dict d s =
  match d with
  | [] -> s
  | (k,v)::t -> string_of_dict t ((string_of_int v)^" troops in "^k^"\n")

let rec string_of_list l s =
  match l with
  | [] -> s
  | h::t -> string_of_list t (s^h^"\n")

let print_state st =
  let s = ref "Players are:" in
  for i = 0 to (Array.length st.turns)-1 do s := !s ^ " " ^ (st.turns.(i)) done;
  s := !s ^ ". It is currently " ^ st.c_turn ^ "\'s turn.\n";
  let p, _ = get_player st.c_turn st.players_list [] in
  s := !s ^ "Player has:\n" ^ (string_of_dict (p.countries_held) "");
  s := !s ^ "Countries availble:\n" ^ (string_of_list st.unclaimed "");!s

(*changes the game state based on the GUI input*)
let do' act state =
  match act with
  | AttackC (ctr1, ctr2) -> attack (String.uppercase_ascii ctr1) (String.uppercase_ascii ctr2) state
  | DeployC (num, ctr) -> for i = 0 to num-1 do inc_troop (String.uppercase_ascii ctr) state done;
    let _ = state.repl_msg <- (String.uppercase_ascii ctr) ^ " has gained " ^ string_of_int num ^ " troop!" in state
  (* | Reinforce (num, ctr1, ctr2) ->
     reinforce num (get_country ctr1 state) (get_country ctr2 state) state *)
  (* | Ally(str) -> ally str state *)
  | QuitC -> quit_helper state
  | ClaimC (ctr) -> pick_country (String.uppercase_ascii ctr) state
  (* | InvC -> inv_helper state *)
  | _ -> let _ = state.repl_msg <- "Command Currently Unavailable" in state

let taken_by state plyr =
  List.map (fun (a,_) -> a) plyr.countries_held

let available state =
  state.unclaimed

let exits cntry =
  cntry.neighbors

let win state =
  let plyrs = state.players_list in
  let total_countries = List.length state.countries in
  let rec helper players =
    match players with
    | [] -> false
    | h::t -> if List.length h.countries_held = total_countries then true
      else helper t
  in helper plyrs

let cards_owned plyr = plyr.cards

let cards_free state = state.card_l

let get_player_of_state s =
  let player_id = s.c_turn in
  let rec helper plyrs =
    match plyrs with
    | [] -> failwith ("Player id not in list of players!")
    | h::t -> if h.id = player_id then h
      else helper t
  in helper s.players_list

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

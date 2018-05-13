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

type t_phase = Deploy | Attack | Reinforce

type phase = SetUp | Game of t_phase

type die_roll = One|Two|Three|Four|Five|Six|None

type player = {
  id: string;
  character: character;
  mutable deploy: int;
  mutable continents: continent list;
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

let get_country_content ct =
  ct.c_id

let get_player_list st =
  st.players_list

let get_country_id ct =
  ct.country_id

let get_num_deploy pl =
  pl.deploy

let get_player_countries pl =
  pl.countries_held

let get_neighbors ct =
  ct.neighbors

let get_unclaimed st =
  st.unclaimed

let init_troops = 30

let get_msg st =
  st.repl_msg

let head_of_lst lst =
  match lst with
  | [] -> []
  | h::t -> h

let init_characters () = [Bran; NightKing; DaenerysTargareyan; JonSnow]


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
  let c_list = a |> member "countries" |> to_list in
  let c_list' = List.map (fun x -> String.uppercase_ascii (to_string x)) c_list in
  {continent_id = id; bonus_troops = bonus; country_list = c_list'}

(* take json element a and create country record*)
let to_countries a =
  let id = a |> member "id" |> to_string in
  let neighbor_lst = a |> member "neighbors" |> to_list |> List.map to_string in
  let cont_id = a |> member "continent" |> to_string in
  {country_id = id; neighbors = neighbor_lst; c_id = cont_id}

let rec players n l =
  if n == 0 then l
  else let player = {id = string_of_int n; character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = [];} in
    let npl = player::l in
    players (n-1) npl

let rec ai p n l =
  if n == 0 then l
  else let player = {id = "a"^string_of_int (p+n); character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = [];} in
    let npl = player::l in
    players (n-1) npl

let order n p ai =
  let ol = Array.make n "" in
  for i = 0 to p-1 do ol.(i)<-(string_of_int (i+1)) done;
  for i = p to n-1 do ol.(i)<-("a"^string_of_int (i+1)) done; ol

let init_state p ai_p j =
  let continents = j|> member "continents" |> to_list |> List.map to_continents in
  let countries = j|> member "countries" |> to_list |> List.map to_countries in
  let u_countries = let f x = (String.uppercase_ascii x.country_id) in List.map f countries in
  let repl_msg = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!" in
  let fog_of_war = j |> member "fog_of_war" |> to_string in
  let win = j|> member "win_message" |> to_string in
  {players_list = (players p []) @ (ai p ai_p []); c_turn = "1"; turns = order (p+ai_p) p ai_p; turn = 0; c_phase = SetUp;
   continents = continents; countries = countries; unclaimed = u_countries;
   card_l = init_cards (); fog_of_war = fog_of_war; w_msg = win; repl_msg = repl_msg}

let add_player id character st =
  let player = {id = id; character = character; deploy = 0; continents = [];
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

let rec conq_continent countryl continentl cl =
  match continentl with
  | [] -> cl
  | h::t ->
    let rec contains_all l =
      match l with
      | [] -> conq_continent countryl t (h::cl)
      | h'::t' -> let _ = print_string h' in
        if List.mem h' countryl
        then contains_all t'
        else conq_continent countryl t cl in
    contains_all h.country_list

let pick_country c st =
  if (List.mem c st.unclaimed) then
    let player, pl = get_player st.c_turn st.players_list [] in
    let _ = player.countries_held <- (c, 1)::player.countries_held in
    let pc = List.map (fun (k,v) -> k) player.countries_held in
    let _ = player.continents <- conq_continent pc st.continents [] in
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

let roll n =
  let rl = Array.make n 0 in
  for i = 0 to n-1 do rl.(i)<-(Random.int 5 + 1) done; rl

let get_troops c p =
  let f x (k,v) = if c = k then v else x + 0 in
  List.fold_left f 0 p.countries_held

(*returns sum of all troops held by a player*)
let sum_troops p =
  List.fold_left (+) 0 (List.map (fun x -> snd x) p.countries_held)

let inc_troop n c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v+n)::t else inc c t
  in let _ = player.countries_held <- (inc c player.countries_held) in
  let _ = st.repl_msg <- c ^ " has gained " ^ (string_of_int n) ^ " troop!" in
  let _ = st.players_list <- (player::pl) in st

let dec_troop n c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec dec c cl =
    match cl with
    | [] -> failwith "Invalid Country"
    | (k,v)::t -> if k = c then (k,v-n)::t else dec c t
  in let _ = player.countries_held <- (dec c player.countries_held) in
  let _ = st.repl_msg <- c ^ " has lost" ^ (string_of_int n) ^ " troop!" in
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

let rec max2_e l =
  let _ = Array.fast_sort compare l in
  l.(Array.length l - 2)

let conquer a d pl c t st =
  let f (k,v) = k <> c in
  let _ = a.countries_held <- ((c,t)::a.countries_held) in
  let _ = d.countries_held <- List.filter f d.countries_held in
  let pc = List.map (fun (k,v) -> k) a.countries_held in
  let _ = a.continents <- conq_continent pc st.continents [] in
  let pc2 = List.map (fun (k,v) -> k) d.countries_held in
  let _ = d.continents <- conq_continent pc2 st.continents [] in
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
      else dec_troop 1 c2 st (* Decrement Defense Troop *)
    else dec_troop 1 c st (* Decerement Attack Troop *)
  | [|x; x'|], [|x2|] -> let max_x = max_e [x;x'] in
    if max_x > x2
    then if d_troops = 1
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop 1 c2 st (* Decrement Defense Troop *)
    else dec_troop 1 c st (* Decerement Attack Troop *)
  | [|x; x'; x''|], [|x2|] -> let max_x = max_e [x;x';x''] in
    if max_x > x2
    then if d_troops = 1
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop 1 c2 st (* Decrement Defense Troop *)
    else dec_troop 1 c st (* Decerement Attack Troop *)
  | [|x|], [|x2; x2'|] -> let max_x = max_e [x2;x2'] in
    if x > max_x
    then dec_troop 1 c2 st (* Decrement Defense Troop *)
    else dec_troop 1 c st (* Decerement Attack Troop *)
  | [|x; x'|], [|x2; x2'|] ->
    let max_x = max_e [x;x'] in
    let max_x2 = max_e [x2;x2'] in
    let max2_x = max2_e [|x;x'|] in
    let max2_x2 = max2_e [|x2;x2'|] in
    if max_x > max_x2 && max2_x > max2_x2
    then if d_troops = 2
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop 2 c2 st (* Decrement Defense Troop *)
    else if max_x > max_x2 || max2_x > max2_x2
    then let st' = dec_troop 2 c st in dec_troop 2 c2 st'
    else dec_troop 2 c st (* Decerement Attack Troop *)
  | [|x; x'; x''|], [|x2; x2'|] ->
    let max_x = max_e [x;x'] in
    let max_x2 = max_e [x2;x2'] in
    let max2_x = max2_e [|x;x'|] in
    let max2_x2 = max2_e [|x2;x2'|] in
    if max_x > max_x2 && max2_x > max2_x2
    then if d_troops = 2
      then conquer attacker defender pl c2 (Array.length a_roll) st (* Conquer *)
      else dec_troop 2 c2 st (* Decrement Defense Troop *)
    else if max_x > max_x2 || max2_x > max2_x2
    then let st' = dec_troop 2 c st in dec_troop 2 c2 st'
    else dec_troop 2 c st (* Decerement Attack Troop *)
  | _, _ -> let _ = print_int (Array.length a_roll) in
    let _ = print_int (Array.length d_roll) in
    failwith "Program Failure"

let rec to_country s cl =
  match cl with
  | [] -> failwith "Invalid Country!"
  | h::t -> if h.country_id = s then h else to_country s t

let rec reinforcable current_c dest neighbors cl visited st =
  match neighbors with
  | [] -> false
  | h::t ->
    if List.mem h visited = false && List.mem h cl
    then
      if h.country_id = dest
      then true
      else
        let to_c = fun x -> to_country x st.countries in
        let new_n = List.map to_c h.neighbors in
        reinforcable h.country_id dest new_n cl (h::visited) st || reinforcable current_c dest t cl (h::visited) st
    else reinforcable current_c dest t cl visited st

let reinforce n c1 c2 st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let cl = (List.map (fun (k,v) -> k) player.countries_held) in
  let c1b = List.mem c1 cl in
  let c2b = List.mem c2 cl in
  let rec suff l =
    match l with
    | [] -> false
    | (k,v)::t -> if k = c1 then n < v else suff t in
  let suffb = suff player.countries_held in
  if c1b && c2b then
    if suffb then let _ = st.repl_msg <- "Insufficent troops!" in st
    else
      let to_c = fun x -> to_country x st.countries in
      let countryl = List.map to_c cl in
      let country1 = to_c c1 in
      let neighbors = List.map to_c country1.neighbors in
      let rb = reinforcable c1 c2 neighbors countryl [] st in
      if rb then
        let st' = dec_troop n c1 st in
        let st'' = inc_troop n c2 st' in st''
      else let _ = st.repl_msg <- "Countries must be connected!" in st
  else let _ = st.repl_msg <- "Must own both countries to reinforce!" in st

(* remove player from game, re-distribute their countries to remaining player*)
let quit_helper st =
  let player, pl = get_player st.c_turn st.players_list [] in
  (*free players properties, re-deploy to remaining players*)
  let _ = st.players_list <- List.filter (fun x -> x != player) st.players_list
  in st

let getPhase st =
  st.c_phase

let rec string_of_dict d s =
  match d with
  | [] -> s
  | (k,v)::t -> string_of_dict t (s ^ (string_of_int v) ^ " troops in "^k^"\n")

let rec string_of_list l s =
  match l with
  | [] -> s
  | h::t -> string_of_list t (s^h^"\n")

let rec string_of_continent l s =
  match l with
  | [] -> s
  | h::t -> string_of_continent t (s^(h.continent_id)^"\n")

let print_state st =
  let s = ref "Players are:" in
  for i = 0 to (Array.length st.turns)-1 do s := !s ^ " " ^ (st.turns.(i)) done;
  s := !s ^ ". It is currently " ^ st.c_turn ^ "\'s turn.\n";
  let p, _ = get_player st.c_turn st.players_list [] in
  s := !s ^ "Player has:\n Countries:\n" ^ (string_of_dict (p.countries_held) "");
  s := !s ^ "Continents:\n" ^ (string_of_continent (p.continents) "");
  s := !s ^ "Countries availble:\n" ^ (string_of_list st.unclaimed "");
  let s' = match st.c_phase with
    | SetUp -> "SetUp!"
    | Game x -> match x with
      | Deploy -> "Deploy!"
      | Attack -> "Attack!"
      | Reinforce -> "Reinforce!" in
  s := !s ^ "Current Phase is " ^ s'; !s

(*changes the game state based on the GUI input*)
let do' cmd st =
  match st.c_phase with
  | SetUp -> (match cmd with
      | ClaimC (c) when st.unclaimed != [] -> let st2 = pick_country (String.uppercase_ascii c) st in
        let _ = if st2.turn = (Array.length st2.turns)*4 then st2.c_phase <- Game (Deploy) else st2.c_phase <- st2.c_phase in st2
      | DeployC (n,c) when n == 1 ->
        let st' = inc_troop 1 (String.uppercase_ascii c) st in
        let _ = st'.turn <- st'.turn + 1 in
        let _ = st'.c_turn <- st'.turns.(st'.turn mod (Array.length st'.turns)) in
        let _ = if st'.turn = (Array.length st'.turns)*4 then st'.c_phase <- Game (Deploy) else st'.c_phase <- st'.c_phase in
        let _ = st'.repl_msg <- (String.uppercase_ascii c) ^ " has gained one troop!" in st'
      | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
  | Game x -> (match x with
      | Deploy -> (match cmd with
          | DeployC (n,c) ->
            (let player, pl = get_player st.c_turn st.players_list [] in
             if player.deploy = 0 then
               let troops = (if ((List.length player.countries_held)/3 <= 3)
                             then 3
                             else (List.length player.countries_held)/3) +
                            List.fold_left (fun x y -> x + y.bonus_troops) 0 player.continents in
               let _ = player.deploy <- troops in
               let _ = st.players_list <- (player::pl) in
               if n <= player.deploy then
                 let _ = player.deploy <- player.deploy-n in
                 let _ = if player.deploy = 0
                   then (st.c_phase <- Game (Attack); st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)))
                   else st.c_phase <- Game (Deploy) in
                 let _ = st.players_list <- (player::pl) in
                 let _ = st.repl_msg <- (String.uppercase_ascii c) ^ " has gained " ^ string_of_int n ^ " troop!" in
                 let st' = inc_troop n (String.uppercase_ascii c) st in st'
               else let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st
             else
             if n <= player.deploy then
               let _ = player.deploy <- player.deploy-n in
               let _ = if player.deploy = 0
                 then (st.c_phase <- Game (Attack); st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)))
                 else st.c_phase <- Game (Deploy) in
               let _ = st.players_list <- (player::pl) in
               let _ = st.repl_msg <- (String.uppercase_ascii c) ^ " has gained " ^ string_of_int n ^ " troop!" in
               let st' = inc_troop n (String.uppercase_ascii c) st in st'
             else let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st)
          | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
      | Attack -> (match cmd with
          | AttackC (c1, c2) -> attack (String.uppercase_ascii c1) (String.uppercase_ascii c2) st
          | EndPhaseC -> let _  = st.c_phase <- Game (Reinforce) in st
          | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
      | Reinforce -> (match cmd with
          | ReinforceC (n, c1, c2) ->
            let st' = reinforce n (String.uppercase_ascii c1) (String.uppercase_ascii c2) st in
            let _ = st'.turn <- st'.turn + 1 in
            let _ = st'.c_turn <- st'.turns.(st'.turn mod (Array.length st'.turns)) in
            let _  = st'.c_phase <- Game (Deploy) in st'
          | EndPhaseC ->
            let _ = st.turn <- st.turn + 1 in
            let _ = st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)) in
            let _  = st.c_phase <- Game (Reinforce) in st
          | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st))

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

let get_player_by_id s id =
  let rec helper plyrs =
    match plyrs with
    | [] -> failwith ("Player id not in list of players!")
    | h::t -> if h.id = id then h
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

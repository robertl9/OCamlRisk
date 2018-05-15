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

type die_roll = One | Two | Three | Four | Five | Six | None

type player = {
  id: string;
  character: character;
  mutable deploy: int;
  mutable continents: continent list;
  mutable countries_held: (string * int) list;
  mutable cards: card list;
  mutable flag: bool
}

let troop_bonus_list= [4;6;8;10;12;15]

let get_continents pl =
  pl.continents

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
  mutable attackDice: int list;
  mutable defendDice: int list;
  fog_of_war: string;
  mutable repl_msg: string;
  mutable card_bonus_index: int;
  mutable card_bonus: int;
  w_msg: string;
}


let get_countries st =
  st.countries

(* [getCountryTroops st] returns association list of country id and troops
 * requires: st is a state
*)
let getCountryTroops st =
  let rec append pl l =
    match pl with
    | [] -> l
    | h::t -> append t (h.countries_held@l) in
  append st.players_list []

(* [getAttackDice st] returns a list of numbers rolled on attack dice
 * requires: st is a state
*)
let getAttackDice st =
  st.attackDice

(* [getDefendDice st] returns a list of numbers rolled on defend dice
 * requires: st is a state
*)
let getDefendDice st =
  st.defendDice

let get_continent_id cont =
  cont.continent_id

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

let get_player_of_state s =
  let player_id = s.c_turn in
  let rec helper plyrs =
    match plyrs with
    | [] -> failwith ("Player id not in list of players!")
    | h::t -> if h.id = player_id then h
      else helper t
  in helper s.players_list

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

(* [players n l] returns n human players in a list
 * requires: n is an int
             l is a players list
*)
let rec players n l =
  if n == 0 then l
  else let player = {id = string_of_int n; character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = []; flag = false} in
    let npl = player::l in
    players (n-1) npl

(* [aiE p n l] returns n easy ai players in a list
 * requires: p is an int
             n is an int
             l is a players list
*)
let rec aiE p n l =
  if n == 0 then l
  else let player = {id = "ae"^string_of_int (p+n); character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = []; flag = false} in
    let npl = player::l in
    aiE p (n-1) npl

(* [aiM p n l] returns n medium ai players in a list
 * requires: p is an int
             n is an int
             l is a players list
*)
let rec aiM p n l =
  if n == 0 then l
  else let player = {id = "am"^string_of_int (p+n); character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = [];flag = false} in
    let _ = print_int n in
    let _ = print_string (player.id^"\n") in
    let npl = player::l in
    aiM p (n-1) npl

(* [aiH p n l] returns n hard ai players in a list
 * requires: p is an int
             n is an int
             l is a players list
*)
let rec aiH p n l =
  if n == 0 then l
  else let player = {id = "ah"^string_of_int (p+n); character = JonSnow; deploy = 0;
                     continents = []; countries_held = []; cards = []; flag = false} in
    let npl = player::l in
    aiH p (n-1) npl

(* [order n p eAI mAI hAI] returns an array of the order of players
 * requires: p is an int
             n is an int
             eAi is an int
             mAi is an int
             hAi is an int
*)
let order n p eAI mAI hAI =
  let ol = Array.make n "" in
  for i = 0 to p-1 do ol.(i)<-(string_of_int (i+1)) done;
  for i = p to p+eAI-1 do ol.(i)<-("ae"^string_of_int (i+1)) done;
  for i = p+eAI to p+eAI+mAI-1 do ol.(i)<-("am"^string_of_int (i+1)) done;
  for i = p+eAI+mAI to p+eAI+mAI+hAI-1 do ol.(i)<-("ah"^string_of_int (i+1)) done; ol

let init_state p eAI mAI hAI j =
  let continents = j|> member "continents" |> to_list |> List.map to_continents in
  let countries = j|> member "countries" |> to_list |> List.map to_countries in
  let u_countries = let f x = (String.uppercase_ascii x.country_id) in List.map f countries in
  let repl_msg = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!" in
  let fog_of_war = j |> member "fog_of_war" |> to_string in
  let win = j|> member "win_message" |> to_string in
  let orderl = order (p+eAI+mAI+hAI) p eAI mAI hAI in
  {players_list = (players p []) @ (aiE p eAI []) @ (aiM p mAI []) @ (aiH p hAI []);
   c_turn = orderl.(0); turns = orderl; turn = 0; c_phase = SetUp;
   continents = continents; countries = countries; unclaimed = u_countries;
   card_l = init_cards (); attackDice = []; defendDice = [];
   fog_of_war = fog_of_war; w_msg = win; repl_msg = repl_msg;
   card_bonus = 4; card_bonus_index = 0}

(* [get_player p pl npl] returns a tuple of player associated with id p and
                         list of remaining players
 * requires: p is a player id
             pl is a players list
             npl is a players list
*)
let rec get_player p pl npl =
  match pl with
  | [] -> failwith "Invalid Player"
  | h::t -> if h.id = p then h, t@npl else get_player p t (h::npl)

let get_cplayer st =
  st.c_turn

(* [remove_l c l] returns an 'a list with c removed from l
 * requires: c is an 'a'
             l is an 'a list
*)
let rec remove_l c l =
  match l with
  | [] -> l
  | h::t -> if (h = c) then t else h::(remove_l c t)

(* [conq_continent countryl continentl cl] returns a continent list of the continents
                                     the player owns
 * requires: countryl is a coutry list
             continentl is a continent list
             cl is a continent list
*)
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

(* [pick_country c st] returns a state with country c claimed
 * requires: c is an country id
             st is a state
 * raise: "Invalid Country/Country Taken" if country does not exist
*)
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

let get_player_id pl =
  pl.id

let rec get_country str_country lst =
  match lst with
  | [] -> failwith "Country not found"
  | h::t -> if String.uppercase_ascii h.country_id =
    String.uppercase_ascii str_country then h else get_country str_country t

let get_conts_on player st=
  List.map (fun x -> (String.lowercase_ascii x.c_id))
  (List.map (fun x -> get_country (fst x) st) player.countries_held)

(* [get_defender c pl npl] returns a tuple of player owning country c and
                           remaining players list
 * requires: c is an country id
             pl is a players list
             npl is a players list
 * raise: "Invalid Country" if country does not exist
*)
let rec get_defender c pl npl =
  let f x (k,v) = if String.uppercase_ascii c = String.uppercase_ascii k then true else x || false in
  match pl with
  | [] -> failwith "Invalid Country"
  | h::[] -> h, npl
  | h::t -> let boolean = List.fold_left f false h.countries_held in
    if boolean then h, t@npl else get_defender c t (h::npl)

(* [roll n] returns an array with n randonly generated ints
 * requires: n is an int
*)
let roll n =
  let rl = Array.make n 0 in
  for i = 0 to n-1 do rl.(i)<-(Random.int 5 + 1) done; rl

let get_troops c p =
  let f x (k,v) = if String.uppercase_ascii c = String.uppercase_ascii k then v else x + 0 in
  List.fold_left f 0 p.countries_held

let find_owner c st =
  let c_held_lists = List.flatten (List.map (fun x -> (x.countries_held)) st.players_list) in
  let rec get_p tup lst =
    match lst with
    | [] -> failwith "Country not owned by a player, not possible outside of SetUp"
    | h::t -> if List.mem tup h.countries_held then h else get_p tup t in
  let rec make_lst assocs =
    match assocs with
    | [] -> []
    | (k,v)::t -> ((get_p (k,v) st.players_list), k, v)::(make_lst t) in
  (*list of (player, country name, num troops*)
  let lst = make_lst c_held_lists in
  let rec find_p lst =
    match lst with
    | [] -> failwith "Invalid country, no owner found"
    | (p,k,v)::t -> if String.uppercase_ascii k = String.uppercase_ascii c then p else find_p t in
  find_p lst

let calc_troops player =
  if player.deploy = 0 then
    (if ((List.length player.countries_held)/3 <= 3)
    then 3 else (List.length player.countries_held)/3) +
    List.fold_left (fun x y -> x + y.bonus_troops) 0 player.continents
  else player.deploy

(*returns sum of all troops held by a player*)
let sum_troops p =
  List.fold_left (+) 0 (List.map (fun x -> snd x) p.countries_held)

(* [inc_troop n c st] returns a state with country c incremented by n troops
 * requires: n is an int
             c is a country id
             st is a state
*)
let inc_troop n c st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let rec inc c cl =
    match cl with
    | [] -> []
    | (k,v)::t -> if k = c then (k,v+n)::t else (k,v)::(inc c t)
  in let _ =
    if List.mem_assoc c player.countries_held
    then
      let _ = player.countries_held <- (inc c player.countries_held) in
      st.repl_msg <- c ^ " has gained" ^ (string_of_int n) ^ " troop!"
    else st.repl_msg <- st.c_turn ^ " does not own this country!" in
  let _ = st.players_list <- (player::pl) in st

(* [dec_troop n c st] returns a state with country c decremented by n troops
 * requires: n is an int
             c is a country id
             st is a state
*)
let dec_troop n c st =
  let player, pl = get_defender c st.players_list [] in
  let rec dec c cl =
    match cl with
    | [] -> []
    | (k,v)::t -> if k = c then (k,v-n)::t else (k,v)::(dec c t)
  in let _ = player.countries_held <- (dec c player.countries_held) in
  (* let _ = st.repl_msg <- c ^ " has lost" ^ (string_of_int n) ^ " troop!" in *)
  let _ = st.players_list <- (player::pl) in st

let rec find_index a el index =
  if Array.length a = 0 then failwith ("Impossible!")
  else if el = a.(0) then index
  else let new_array = Array.sub a 1 (Array.length a - 1) in
    find_index new_array el (index+1)

let remove_card arr elem =
  let index = find_index arr elem 0 in
  let arr1 = Array.sub arr 0 index in
  let arr2 = Array.sub arr (index+1) (Array.length arr - 1 - index) in
  Array.append arr1 arr2

(* [draw_card st] returns a state with a card added to the current player
 * requires: st is a state
*)
let draw_card st =
  if Array.length st.card_l > 0 then
    let len = Array.length st.card_l in
    let c = st.card_l.(Random.int len) in
    let _ = st.card_l <- remove_card st.card_l c in
    (* Remove Card *)
    let player, pl = get_player st.c_turn st.players_list [] in
    let _ = player.cards <- c::player.cards in
    let _ = st.players_list <- (player::pl) in st
  else st

(* [max_e l] returns the max element in the list l
 * requires: l is a list
*)
let rec max_e l =
  match l with
  | [] -> failwith "Invalid Roll"
  | h::[] -> h
  | h::t -> max h (max_e t)

(* [max2_e l] returns the second largest element in the list l
 * requires: l is a list
*)
let rec max2_e l =
  let _ = Array.fast_sort compare l in
  l.(Array.length l - 2)

(* [to_country s cl] returns the country associated with id s
 * requires: s is a country id
             cl is a country list
 * raises: s^" is a Invalid Country!" if s is not a valid country id
*)
let rec to_country s cl =
  match cl with
  | [] -> failwith (s^" is a Invalid Country!")
  | h::t -> if String.uppercase_ascii h.country_id = String.uppercase_ascii s then h else to_country s t

(* [conquer a d pl c t st] returns a state where a conquers country c from d
 * requires: a is a player
             d is a player
             pl is a players list
             c is country id
             t is sn int
             st is a state
*)
let conquer a d pl c t st =
  let _ = if a.flag = false then
      let _ = draw_card st in
      let plyr, nonplyrs = get_player st.c_turn st.players_list [] in
      let _ = plyr.flag <- true in
      let _ = st.players_list <- plyr::nonplyrs in st
    else st in
  let f (k,v) = k <> c in
  let _ = a.countries_held <- ((c,t)::a.countries_held) in
  let _ = d.countries_held <- List.filter f d.countries_held in
  (* Remove player from Playing List *)

  let pc = List.map (fun (k,v) -> k) a.countries_held in
  let _ = a.continents <- conq_continent pc st.continents [] in
  let pc2 = List.map (fun (k,v) -> k) d.countries_held in
  let _ = st.repl_msg <- string_of_int (List.length d.countries_held) in
  (* print_int (List.length d.countries_held) ;  *)
  if List.length d.countries_held = 0 then
    let _ = a.cards <- a.cards @ d.cards in
    let _ = st.repl_msg <- a.id ^ " has conquered " ^ c ^ "and "
                           ^ a.id^" has defeated " ^ d.id ^ "!" in
    print_string ("in loop") ;
    let index = find_index st.turns a.id 0 in
    let _ = st.turns <- remove_card st.turns d.id in
    let _ = st.turn <- index in
    let _ = st.players_list <- (a::pl) in st
  else
    let _ = d.continents <- conq_continent pc2 st.continents [] in
    let _ = st.repl_msg <- a.id ^ " has conquered " ^ c ^ "!" in
    let _ = st.players_list <- (a::d::pl) in st

(* [mem c l] returns true if c is l case insensitive
 * requires: c is a country id
             l is a country id list
*)
let rec mem c l =
  match l with
  | [] -> true
  | h::t -> if String.uppercase_ascii h = c then false else mem c t

(* [attack c c2 st] returns new state with outcome of the attack from c to c2
 * requires: c is a country id
             c2 is a country id
             st is a state
 * raises: "Invalid Rolls!" if s is not a valid roll
*)
let attack c c2 st =
  let attacker, pl = get_defender c st.players_list [] in
  if attacker.id <> st.c_turn
  then let _ = st.repl_msg <- "Player must own attacking country!" in st
  else
    let defender, pl = get_defender c2 pl [] in
    let to_c = fun x -> to_country x st.countries in
    let country1 = to_c c in
    let neighbors = country1.neighbors in
    let borderBool = mem c2 neighbors in
    let a_troops = get_troops c attacker in
    let d_troops = get_troops c2 defender in
    let a_roll = if a_troops > 3 then roll 3 else
      if a_troops = 1 then [||] else roll (a_troops-1) in
    let d_roll = if d_troops >= 2 then roll 2 else roll d_troops in
    if a_roll = [||] || attacker.id = defender.id || borderBool
    then
      if a_roll = [||]
      then let _ = st.repl_msg <- "Insufficient Troops!" in st
      else
        if attacker.id = defender.id
        then let _ = st.repl_msg <- "Player owns both countries!" in st
        else let _ = st.repl_msg <- "Countries do not border each other!" in st
    else
      let _ = st.attackDice <- Array.to_list a_roll in
      let _ = st.defendDice <- Array.to_list d_roll in
      match a_roll, d_roll with
      | [|x|], [|x2|] -> if x > x2
        then if d_troops = 1
          then
            let st' = conquer attacker defender pl c2 (a_troops - 1) st in (* Conquer *)
            dec_troop (a_troops - 1) c st'
          else dec_troop 1 c2 st (* Decrement Defense Troop *)
        else dec_troop 1 c st (* Decerement Attack Troop *)
      | [|x; x'|], [|x2|] -> let max_x = max_e [x;x'] in
        if max_x > x2
        then if d_troops = 1
          then
            let st' = conquer attacker defender pl c2 (a_troops - 1) st in (* Conquer *)
            dec_troop (a_troops - 1) c st'
          else dec_troop 1 c2 st (* Decrement Defense Troop *)
        else dec_troop 1 c st (* Decerement Attack Troop *)
      | [|x; x'; x''|], [|x2|] -> let max_x = max_e [x;x';x''] in
        if max_x > x2
        then if d_troops = 1
          then
            let st' = conquer attacker defender pl c2 (a_troops - 1) st in (* Conquer *)
            dec_troop (a_troops - 1) c st'
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
          then
            let st' = conquer attacker defender pl c2 (a_troops - 1) st in (* Conquer *)
            dec_troop (a_troops - 1) c st'
          else dec_troop 2 c2 st (* Decrement Defense Troop *)
        else
        if max_x2 >= max_x && max2_x2 >= max2_x
        then dec_troop 2 c st
        else let st' = dec_troop 1 c st in dec_troop 1 c2 st' (* Decerement Attack Troop *)
      | [|x; x'; x''|], [|x2; x2'|] ->
        let max_x = max_e [x;x';x''] in
        let max_x2 = max_e [x2;x2'] in
        let max2_x = max2_e [|x;x';x''|] in
        let max2_x2 = max2_e [|x2;x2'|] in
        if max_x > max_x2 && max2_x > max2_x2
        then if d_troops = 2
          then
            let st' = conquer attacker defender pl c2 (a_troops - 1) st in (* Conquer *)
            dec_troop (a_troops - 1) c st'
          else dec_troop 2 c2 st (* Decrement Defense Troop *)
        else
          if max_x2 >= max_x && max2_x2 >= max2_x
          then dec_troop 2 c st
          else let st' = dec_troop 1 c2 st in let st'' = dec_troop 1 c st' in
            let _ = st''.repl_msg <- "1and1" in st''            (* Decerement Attack Troop *)
      | _, _ -> (let _ = st.repl_msg <- "Invalid Rolls" in st)

let rec reinforcable current_c dest neighbors cl visited st =
  match neighbors with
  | [] -> false
  | h::t ->
    if List.mem h visited = false && List.mem h cl
    then
      if String.uppercase_ascii h.country_id = dest
      then true
      else
        let to_c = fun x -> to_country x st.countries in
        let new_n = List.map to_c h.neighbors in
        reinforcable h.country_id dest new_n cl (h::visited) st || reinforcable current_c dest t cl (h::visited) st
    else reinforcable current_c dest t cl visited st

(* [reinforce n c c2 st] returns new state with n trooops moved from c1 to c2
 * requires: n is an int
             c1 is a country id
             c2 is a country id
             st is a state
*)
let reinforce n c1 c2 st =
  let player, pl = get_player st.c_turn st.players_list [] in
  let cl = (List.map (fun (k,v) -> k) player.countries_held) in
  let c1b = List.mem c1 cl in
  let c2b = List.mem c2 cl in
  let rec suff l =
    match l with
    | [] -> true
    | (k,v)::t -> if k = c1 then n >= v else suff t in
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

let getPhaseString st =
  match st.c_phase with
    | SetUp -> "SetUp!"
    | Game x -> match x with
      | Deploy -> "Deploy!"
      | Attack -> "Attack!"
      | Reinforce -> "Reinforce!"

(* [string_of_dict d s] returns string of dict elements
 * requires: d is an association list
             s is a string
*)
let rec string_of_dict d s =
  match d with
  | [] -> s
  | (k,v)::t -> string_of_dict t (s ^ (string_of_int v) ^ " troops in "^k^"\n")

(* [string_of_list l s] returns string of list elements
 * requires: l is an list
             s is a string
*)
let rec string_of_list l s =
  match l with
  | [] -> s
  | h::t -> string_of_list t (s^h^"\n")

let printOrder st =
  string_of_list (Array.to_list st.turns) ""

(* [string_of_continent l s] returns string of continent list
 * requires: l is a continent list
             s is a string
*)
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

let rec count_cards lst card card_acc wild_acc =
  match lst with
  | [] -> if card_acc + wild_acc >= 3 then (true, card_acc, wild_acc)
    else (false, card_acc, wild_acc)
  | h::t -> if h = card then count_cards t card (1+card_acc) wild_acc
    else if h = WildCard then count_cards t card card_acc (1+wild_acc)
    else count_cards t card card_acc wild_acc

let rec remove_elem elem lst =
  match lst with
  | [] -> []
  | h::t -> if h = elem then t
    else h::(remove_elem elem t)

let helper card1 card2 card3 st  =
  let c_turn_after = st.turns.((st.turn+1) mod (Array.length st.turns)) in
  let plyr, nonplyrs = get_player c_turn_after st.players_list [] in
  let bonus = st.card_bonus in
  let _ = st.repl_msg <- "" ^ string_of_int (bonus) ^ "troops gained!" in
  let new_lst = remove_elem card3(remove_elem card2(remove_elem card1 plyr.cards)) in
  let _ = plyr.cards <- new_lst in
  let _ = plyr.deploy <- (plyr.deploy + bonus) in
  let _ = st.players_list <- (plyr::nonplyrs) in
  if st.card_bonus_index < 5
  then
    let _ = st.card_bonus <- (List.nth troop_bonus_list (st.card_bonus_index + 1)) in
    let _ = st.card_bonus_index <- (st.card_bonus_index + 1) in st
  else
    let _ = st.card_bonus <- (15 + (st.card_bonus_index+1 - 5) * 5) in
    let _ = st.card_bonus_index <- (st.card_bonus_index + 1) in st

let rec trade st =
  let c_turn_after = st.turns.((st.turn+1) mod (Array.length st.turns)) in
  let plyr, nplyrs = get_player c_turn_after st.players_list [] in
  if List.length plyr.cards < 3 then st
  else
    let (x1,y1,z) = count_cards plyr.cards BannerMan 0 0 in
    let (x2,y2,_) = count_cards plyr.cards Lord 0 0  in
    let (x3,y3,_) = count_cards plyr.cards Dragon 0 0  in
    if x1 then let new_st =
                 helper BannerMan BannerMan BannerMan st in trade new_st
    else if x2 then let new_st =
                 helper Lord Lord Lord st in trade new_st
    else if x3 then let new_st =
                      helper Dragon Dragon Dragon st in trade new_st
    else if z = 0 && (y1 > 0 && y2 > 0 && y3 > 0) then let new_st =
                      helper BannerMan Dragon Lord st in trade new_st
    else if z > 0 && (y1 >= 2) then let new_st =
                      helper BannerMan BannerMan WildCard st in trade new_st
    else if z > 0 && (y2 >= 2) then let new_st =
                      helper Lord Lord WildCard st in trade new_st
    else if z > 0 && (y3 >= 2) then let new_st =
                      helper Dragon Dragon WildCard st in trade new_st
    else if z > 0 && (y1 = 1 && y2 = 1 && y3 = 0) then let new_st =
                      helper BannerMan Lord WildCard st in trade new_st
    else if z > 0 && (y1 = 1 && y2 = 0 && y3 = 1) then let new_st =
                      helper BannerMan Dragon WildCard st in trade new_st
    else if z > 0 && (y1 = 0 && y2 = 1 && y3 = 1) then let new_st =
                      helper Lord Dragon WildCard st in trade new_st
    else st


(*changes the game state based on the GUI input*)
let do' cmd st =
  let _ = st.attackDice <- [] in
  let _ = st.defendDice <- [] in
  match st.c_phase with
  | SetUp -> (match cmd with
      | ClaimC (c) when st.unclaimed != [] -> let st' = pick_country (String.uppercase_ascii c) st in
        let _ = if st'.turn = (Array.length st'.turns)*10 then st'.c_phase <- Game (Deploy) else st'.c_phase <- st'.c_phase in st'
      | ClaimC (c) when st.unclaimed = [] ->
        let st' = inc_troop 1 (String.uppercase_ascii c) st in
        let _ =
          if st'.repl_msg = st.c_turn ^ " does not own this country!"
          then ()
          else
            let _ = st'.turn <- st'.turn + 1 in
            let _ = st'.c_turn <- st'.turns.(st'.turn mod (Array.length st'.turns)) in () in
        let _ = if st'.turn = (Array.length st'.turns)*10 then st'.c_phase <- Game (Deploy) else st'.c_phase <- st'.c_phase in st'
      | DeployC (n,c) when n == 1 ->
        let st' = inc_troop 1 (String.uppercase_ascii c) st in
        let _ =
          if st'.repl_msg = st.c_turn ^ " does not own this country!"
          then ()
          else
            let _ = st'.turn <- st'.turn + 1 in
            let _ = st'.c_turn <- st'.turns.(st'.turn mod (Array.length st'.turns)) in () in
        let _ = if st'.turn = (Array.length st'.turns)*10 then st'.c_phase <- Game (Deploy) else st'.c_phase <- st'.c_phase in st'
      | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
  | Game x -> (match x with
      | Deploy -> (match cmd with
          | DeployC (n,c) ->
            (let player, pl = get_defender (String.uppercase_ascii c) st.players_list [] in
             if player.deploy = 0
             then
               let troops = (if ((List.length player.countries_held)/3 <= 3)
                             then 3
                             else (List.length player.countries_held)/3) +
                            List.fold_left (fun x y -> x + y.bonus_troops) 0 player.continents in
               let _ = player.deploy <- troops in
               let _ = st.players_list <- (player::pl) in
               if n <= player.deploy && player.id = st.c_turn then
                 let _ = player.deploy <- player.deploy-n in
                 let _ = if player.deploy = 0
                   then (st.c_phase <- Game (Attack); st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)))
                   else st.c_phase <- Game (Deploy) in
                 let _ = st.players_list <- (player::pl) in
                 let st' = inc_troop n (String.uppercase_ascii c) st in
                 let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st'
               else let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st
             else
               if n <= player.deploy && player.id = st.c_turn then
                 let _ = player.deploy <- player.deploy-n in
                 let _ = if player.deploy = 0
                   then (st.c_phase <- Game (Attack); st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)))
                   else st.c_phase <- Game (Deploy) in
                 let _ = st.players_list <- (player::pl) in
                 let st' = inc_troop n (String.uppercase_ascii c) st in
                 let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st'
               else let _ = st.repl_msg <- (string_of_int player.deploy) ^ " avaliable troops to deploy." in st)
          | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
      | Attack -> (match cmd with
          | AttackC (c1, c2) -> attack (String.uppercase_ascii c1) (String.uppercase_ascii c2) st
          | EndPhaseC -> let plyr, nonplyrs = get_player st.c_turn st.players_list [] in
            let _ = plyr.flag <- false in
            let _ = st.players_list <- plyr::nonplyrs in
            let _  = st.c_phase <- Game (Reinforce) in
            let _ = st.repl_msg <- "EndPhase now reinforce" in st
          | _ -> let _ = st.repl_msg <- "Command Currently Unavailable" in st)
      | Reinforce -> (match cmd with
          | ReinforceC (n, c1, c2) ->
            let st' = reinforce n (String.uppercase_ascii c1) (String.uppercase_ascii c2) st in
            let _ = st'.turn <- st'.turn + 1 in
            let _ = st'.c_turn <- st'.turns.(st'.turn mod (Array.length st'.turns)) in
            let _  = st'.c_phase <- Game (Deploy) in st'
          | EndPhaseC ->
            let _ = trade st in
            let _ = st.turn <- st.turn + 1 in
            let _ = st.c_turn <- st.turns.(st.turn mod (Array.length st.turns)) in
            let _  = st.c_phase <- Game (Deploy) in
            let _ = st.repl_msg <- "EndPhase now deploy" in st
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

let get_player_by_id s id =
  let _ = print_string "rob's fault" in
  let rec helper plyrs =
    match plyrs with
    | [] -> failwith ("Player id not in list of players!")
    | h::t -> if h.id = id then h
      else helper t
  in helper s.players_list

let rec iterate_through_countries lst id =
  match lst with
  | [] -> false
  | h::t -> if h = id then true
    else iterate_through_countries t id

let country_owned_by_player s country_id =
  let rec helper plyrs =
    match plyrs with
    | [] -> "Unheld"
    | h::t -> let country_ids = (List.map (fun (k,v) -> k) h.countries_held) in
      if iterate_through_countries country_ids country_id
      then h.id else helper t
  in helper s.players_list

let get_win_msg s =
  s.w_msg

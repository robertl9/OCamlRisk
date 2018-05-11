open State
open Command

  (* checks difficulty of player *)
  let is_beg p =
    match p with 
    | Artificial(d) -> d = Beginner
    | _ -> false 

  (* returns an assoc lst of all countries in game and the number of troops on them*)
  let get_troops_lst st = 
	let country_troop_lst = 
	List.flatten (List.fold_left (@) (List.map (fun x-> x.countries_held) st.players_list) []) in

  (*get country that is gaining a troop in deploy command*)
  let get_deployed com = 
	match com with 
	| DeployC(_, ctry) -> ctry 
	| _ -> failwith "Not a deploy command"

  let inc_deployed com = 
	match com with 
	| DeployC(num, _) -> DeployC(num + 1, _) 
	| _ -> failwith "Not a deploy command"

	(* returns a state after calling each command in lst, 
	 * in order from head to tail, on st*)
	let rec do_com_lst st lst = 
		match lst = 
		| [] -> st
		| a::b -> do_com_lst (do a st) lst (* print message here too? *)

  let dep_rank c1 player lst= 
 	let enemy_neighbors = List.filter (fun x-> not List.mem x player.countries_held) c1.neighbors in
 	(* diff between troops on this countyr and troops on all other countries should count a bit *)
 	(* calc sum of the differences between troops on c1 and 
     * neighboring countries not owned by player
 	 *)
  	let c1_troop_diffs = List.map (fun x -> (List.assoc x.country_id !lst) 
  		- (List.assoc c1.country_id !lst)) enemy_neighbor in

  	(* if high, greater need for country to gian more troops*)
  	let c1_enemy_factor = List.fold_left (+) c1_troop_diffs [] in 


  (*Note: may need to add cases for adding bonus troops*)
  let smart_claim st =

    let player, pl = get_player st.c_turn st.players_list [] in

    let troop_ctry_lst = ref get_troops_lst st in 

    (* ranking countries to claim*)
    let claim_rank c1 = 

      (*get amount of other countries player has on same continent*)
      let c1_com_conts = List.length (List.filter (fun x -> x.c_id = c1.c_id) player.countries_held) in 

      (*see how many countries held have current country as a neighbor*)
      let c1_neighbors = List.length (List.filter (fun x -> List.mem c1 x.neighbors) player.countries_held) in 

      let c1_score = c1_com_conts + c1_neighbors in

    (* used to sort the list *)
    let comp_c_rank c1 c2 = 
      let c1_score = claim_rank(c1) in
      let c2_score = claim_rank(c2) in

      if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in

    (* for deploying extra troops during claim*)
	let dep_claim_sort c1 c2 =
	  let c1_score = dep_rank c1 player troop_ctry_lst in 
	  let c2_score = dep_rank c2 player troop_ctry_lst in 
	  if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in

    let simp_claim st = 
      let ctry = List.nth st.unclaimed (Random.int (List.length st.unclaimed)) in

    (* list of continents that players has a country in *)
    let owned_conts = List.filter (fun x -> x.continent_id) player.continents in

    (* get list of countries available that are in continents that the player also has a country in*)
    let opt_countries = List.filter (fun x -> (List.mem x.c_id owned_conts)) st.unclaimed in

    let ctry = 
      (* choose random country if beginner, or there exists no 
       * unavailable countries on continents that player owns
       *)
      if (opt_countries = [] || is_beg player) && st.unclaimed != []
      then simp_claim st
   	  else if st.unclaimed != [] then List.hd (List.sort dep_claim_sort player.countries_held)
      else List.hd (List.sort claim_rank opt_countries) in 
    do st ClaimC(ctry.country_id) 

(* takes in state*)
let smart_deploy st = 
  let player, pl = get_player st.c_turn st.players_list [] in

  (*MUST CHANGE*)
  let num_deploy = st.bonus_troops in 

  let troop_ctry_lst = ref get_troops_lst st in 

  let dep_rank_sort c1 c2 =
    let c1_score = dep_rank c1 player troop_ctry_lst in 
  	let c2_score = dep_rank c2 player troop_ctry_lst in 
    if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in

  let rec dep_helper num acc= 
  	if num = 0 then acc
  	else 
  	  (*list sorted by countries that need troops the most*)
  	  let sorted_lst = List.sort dep_rank_sort player.countries_held in 
  	  let ctry_to_dep = (List.hd (sorted_lst)).country_id in 
  	  let deployed_countries = List.map (fun x -> get_deployed x) sorted_lst in 
  	  let acc = 
  	    (*i.e. if we already deployed a troop to this country*)
  	    if List.mem ctry_to_dep deployed_countries 
  		(*update acc so that repetead countyr gets num + 1 for troops*)
  		then 
  		  let temp_acc = List.filter (fun x -> (get_deployed x) != ctry_to_dep) acc in 
  		  let new_elt = List.find (fun x -> (get_deployed x) != ctry_to_dep) acc in 
  		  (* increment num troops on country *)
  		  (inc_deployed new_elt)::temp_acc 
  		else Deploy(ctry_to_dep, 1)::acc in 
  	  (*edit troops_in_st list accordingly to reflect update to acc*) 
  	  let () = troop_ctry_lst := List.map (fun x -> 
  	  	if (fst x).country_id = ctry_to_dep then (fst x), ((snd x) + 1) else x)
  	    !troop_ctry_lst in 
  	  dep_helper (num - 1) acc in 

   let dep_lst = de_helper num_deploy [] in
   do_com_lst st dep_lst 

let rec smart_attack st = 
  let player, pl = get_player st.c_turn st.players_list [] in
  let troop_ctry_lst = ref get_troops_lst st in 

  let defend_sort_helper def attacker= 
  	(*higher, bigger pos difference between attacking and defending country*)
  	let troop_diff_factor = (List.assoc attacker.country_id !troop_ctry_lst) 
  		- (List.assoc def.country_id !troop_ctry_lst) in 
  	let cont_factor = if def.c_id = attacker.c_id 
  		(*factor in number of other countries owned in def's continent*)
  		then let addit_ctrys = List.length (List.filter (fun x -> x = def.c_id) 
  			(List.map (fun x -> fst x) player.countries_held)) in 
  		2 * troop_diff_factor + addit_ctrys else 0 in 
		troop_diff_factor + cont_factor in 

  (* returns best country for given country to attack *)
  let attack_sort_helper tuple = 
  	(*tuple is an attacker, and list of potential defenders*)
  	let att = fst tuple in 
  	let lst = snd tuple in 
  	 let defend_sort def1 def2 =
  	  let def1_score = defend_sort_helper def1 att in 
  	  let def2_score = defend_sort_helper def2 att in 
  	  if def1_score = def2_score then 0 
  	  else if def1_score > def2_score then 1 else -1 in
  	(*most optimum country to attack for each attacker*)
  	let def = List.hd (List.sort defend_sort lst) in 
  	let troop_dif = (List.assoc attacker.country_id !troop_ctry_lst) 
  		- (List.assoc def.country_id !troop_ctry_lst) in 
  	let cont_factor = if def.c_id = attacker.c_id 
  		(*factor in number of other countries owned in def's continent*)
  		then let addit_ctrys = List.length (List.filter (fun x -> x = def.c_id) 
  			(List.map (fun x -> fst x) player.countries_held)) in 
  		2 * troop_diff_factor + addit_ctrys else 0 in 
		troop_diff_factor + cont_factor in 

  let attack_sort tup1 tup2 = 
  	let tup1_score = attack_rank_helper tup1 in 
  	let tup2_score = attack_rank_helper tup2 in 
  	if tup1_score = tup2_score then 0 
  	else if tup1_score > tup2_score then 1 else -1 in


  (*returns tuple of attacker, defender
   * tuple_lst is a list of (countyr, defender list) tuples
   *)
  let get_attack tuple_lst =
  	(*gets tuple containing most optimum attacker and their list of enemy neighbors*)
  	let attacker_tup = List.head (List.sort attack_sort tuple_lst) in 
  	let att = fst attacker_tup in 
  	let def_lst = fst attacker_tup in (*list of possible defenders*)

  	let def = 
  		let def_rank def1 def2 = 
	  		let def1_score = defend_sort_helper def1 att in 
	  	  let def2_score = defend_sort_helper def2 att in 
	  	  if def1_score = def2_score then 0 
	  	  else if def1_score > def2_score then 1 else -1 in
	  	List.hd (List.sort def_rank def_lst) in 
	  (att, deff) in

	(*gets strings of countries player owns*)
	let ctry_strings = List.map (fun x -> fst x) player.countries_held in 
	let plyr_ctries = List.filter (fun x -> List.mem x.country_id ctry_strings) st.countries in

	(*extracts enemy neighbors from a country's neighbor list*)
	let get_enemies neigh_lst = 
		List.filter (fun x -> not List.mem x.country_id ctry_strings) st.countries in 

	let tuple_lst = List.map (fun x -> (x, (get_enemies x.neighbors))) plyr_ctries in 

	(*most optimal attack, defender pairing*)
	let att_def_tup = get_attack tuple_lst in 
	let attacker = fst att_def_tup in 
	let defender = snd att_def_tup in 
	let troop_diff = (snd (List.find (fun x -> x = attacker.country_id) !troop_ctry_lst)) -
		(snd (List.find (fun x -> x = defender.country_id) !troop_ctry_lst)) in 

	(* depending on troop_diff, choose whether or not to attack *)
	let new_st = if troop_diff > 1 
		then smart_attack (do st Attack(attacker.country_id, defender.country_id)) 
		else st in 
	new_st

	(* check difference between troops...if above certain threshold, attack 
	 * and call smart attack on new state...if not, then do not attack*)


  	(*check to see if difference is big enough to warrant attack (above...1)*)
  	(*if above 1, call attack on state, print message, call smart_attack on that state*)
  	(*if not above 1, do not attack-->return state*)

   
let rec smart_reinforce st = 


(* 

let ai_mid st depth = 
  failwith "Unimplemented"


let determine_move 
   need to check phase--> if Start then claim

  (*else deploy, attack, reinforce*)



 (*  *)

let ai_stronger st depth = 
  failwith "Unimplemented"

let ai_weak st depth = 
  failwith "Unimplemented" *)


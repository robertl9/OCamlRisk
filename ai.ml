open State
open Command

  (* returns an assoc lst of all countries in game and the number of troops on them*)
  let get_troops_lst st =
    List.map (fun x -> ((String.uppercase_ascii (fst x)), snd x))
      (List.flatten (List.fold_left (@) (List.map (fun x-> get_player_countries x) (get_player_list st)) []))

  (*get country that is gaining a troop in deploy command*)
  let get_deployed com =
  match com with
  | DeployC(_, ctry) -> ctry
  | _ -> failwith "Not a deploy command"

  let inc_deployed com =
  match com with
  | DeployC(num, ctry) -> DeployC(num + 1, ctry)
  | _ -> failwith "Not a deploy command"

  let dep_rank c1 player st=
    let ai_diff = String.sub (get_cplayer st) 1 1 in
    let enemy_neighbors = List.filter (fun x-> not (List.mem_assoc x (get_player_countries player))) (get_neighbors c1) in
    (* diff between troops on this country and troops on all other countries should count a bit *)
    (* calc sum of the differences between troops on c1 and
     * neighboring countries not owned by player
    *)
    let rec get_diffs init acc lst = 
      match lst with
      | [] -> acc
      | h::t -> get_diffs init (acc + ((get_troops h (find_owner h st) - init))) t in 

   (*difference between enemy neighbors and country's troops...higher means greater need to deploy*)
   let enemy_factor = if (List.length(enemy_neighbors)) > 0 then (Random.int (List.length(enemy_neighbors))) else 0 in 
   if ai_diff = "m" then (List.length (enemy_neighbors)) - enemy_factor
   else (List.length (enemy_neighbors)) + 3 * (get_diffs (get_troops (get_country_id c1) player) 0 enemy_neighbors)

let get_rand_item lst = List.nth lst (Random.int (List.length lst))

(* takes in state*)
let ai_deploy st =
    let player = get_player_by_id st (get_cplayer st) in
    let _ = print_string (get_cplayer st) in
    let _ = print_string (getPhaseString st) in
    let _ = print_string "got player\n" in
    let num_deploy = calc_troops player in
    let _ = print_string "calculatinng troops\n" in
    let ai_diff = String.sub (get_cplayer st) 1 1 in
    let dep_rank_sort c1 c2 =
      let c1_score = dep_rank c1 player st in
      let c2_score = dep_rank c2 player st in
      (*lower score, want to deploy there*)
      if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in
    (* deploys troops to random countries*)
    let simp_deploy st =
      let _ = print_string "ai simpdep\n" in
      if getPhase st = SetUp then ClaimC((fst (get_rand_item (get_player_countries player))))
      else DeployC(1, (fst (get_rand_item (get_player_countries player)))) in
    (* deploys all bonus troops ot the country in most need*)
    let mid_deploy st =
      let _ = print_string "ai middep\n" in
      let sorted_lst = List.rev (List.sort dep_rank_sort
        (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player))) in
      if List.length sorted_lst = 0 then simp_deploy st 
      else
        let ctry_to_dep = get_country_id (List.hd (sorted_lst)) in
        if getPhase st = SetUp then ClaimC(ctry_to_dep)
        else DeployC(num_deploy, ctry_to_dep) in
    (*deploys 1 troop to country in need, then recalculates need*)
    let smart_deploy st=
      let sorted_lst = List.rev (List.sort dep_rank_sort
        (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player))) in
      if List.length sorted_lst = 0 then simp_deploy st 
      else
        let ctry_to_dep = get_country_id (List.hd (sorted_lst)) in
        if getPhase st = SetUp then ClaimC(ctry_to_dep)
        else DeployC(1, ctry_to_dep) in

  (* if ai is easy, do simp_deploy, else if mid, do mid_deploy, else (hard) do below*)
    (* simp_deploy st *)
    if ai_diff = "e" then simp_deploy st
    else if ai_diff = "m" then mid_deploy st
    else smart_deploy st

let ai_claim st =
  if List.length (get_unclaimed st) = 0 then ai_deploy st
  else
    let _ = print_string "ai claiminng\n" in
    let player = get_player_by_id st (get_cplayer st) in
    let _ = print_string "ai playing\n" in
    let ai_diff = String.sub (get_cplayer st) 1 1 in
    let _ = print_string "ai diffing\n" in

    (* ranking countries to claim*)
    let claim_rank c1 =
      (*get amount of other countries player has on same continent*)
      let c1_com_conts = if List.length (get_player_countries player) = 0 then 0 else 
        List.length (List.filter (fun x -> 
        String.uppercase_ascii(get_country_content x)= String.uppercase_ascii(get_country_content c1))
        (List.map (fun y-> get_country y (get_countries st)) (List.map (fun z -> fst z) (get_player_countries player)))) in

      (*see how many countries held have current country as a neighbor*)
      let c1_neighbors = if List.length (get_player_countries player) = 0 then 0 else 
        List.length (List.filter (fun x -> List.mem (get_country_id c1)
        (List.map (fun x -> String.uppercase_ascii x) (get_neighbors (get_country (fst x) (get_countries st)))))
        (get_player_countries player)) in

      let neigh_strings = List.map (fun x -> (String.uppercase_ascii x)) (get_neighbors c1) in

      (*extracts enemy neighbors*)
      let my_enemies =
        List.filter (fun x ->  ( (List.mem (String.uppercase_ascii (get_country_id x)) neigh_strings)) 
          && (not (List.mem (get_country_id x) (get_unclaimed st)) ) 
          && (not (List.mem (get_country_id x) (List.map (fun z -> fst z) (get_player_countries player))))) (get_countries st) in

      (*extracts friendlys neighbors*)
      let my_allies =
        List.filter (fun x ->  ( (List.mem (String.uppercase_ascii (get_country_id x)) neigh_strings)) 
          && (not (List.mem (get_country_id x) (get_unclaimed st)) ) 
          && ((List.mem (get_country_id x) (List.map (fun z -> fst z) (get_player_countries player))))) (get_countries st) in

        (3 * c1_com_conts + 2 * c1_neighbors - (List.length my_enemies)) + (List.length my_allies) in

    (* used to sort the list *)
    let comp_c_rank c1 c2 =
      let c1_score = claim_rank(c1) in
      let c2_score = claim_rank(c2) in
      if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in
    let simp_claim st =
      get_rand_item (get_unclaimed st) in
    (* list of continents that player has a country in *)
    let owned_conts = get_conts_on player (get_countries st) in

    (* get list of countries available that are in continents that the player also has a country in*)
    let opt_countries =
      List.filter (fun x ->
        (List.mem (String.uppercase_ascii (get_country_content (get_country x (get_countries st)))) owned_conts)) 
        (List.map (fun x -> String.uppercase_ascii x) (get_unclaimed st))  in
    let ctry =
      (* choose random country if beginner, or there exists no
       * unavailable countries on continents that player owns
       *)
      if ai_diff = "e" then simp_claim st
      else if List.length opt_countries = 0 then get_country_id 
        (List.hd (List.rev (List.sort comp_c_rank (List.map (fun x -> (get_country x (get_countries st))) (get_unclaimed st)))))
      else get_country_id (List.hd (List.rev 
        (List.sort comp_c_rank (List.map (fun x -> (get_country x (get_countries st))) opt_countries))))in
    ClaimC(ctry)

let rec all_ai lst = 
  match lst with
  | [] -> true
  | h::t-> if String.sub h 0 1 = "a" then all_ai t else false

let rec ai_attack st = 
  let player = get_player_by_id st (get_cplayer st) in
  let ai_diff = String.sub (get_cplayer st) 1 1 in
  (*gets strings of countries player owns*)
  let ctry_strings = List.map (fun x ->  (String.uppercase_ascii (fst x))) (get_player_countries player) in
  let plyr_ctries = List.filter (fun x -> List.mem (String.uppercase_ascii (get_country_id x)) ctry_strings) 
    (get_countries st) in

  (*extracts enemy neighbors from a country's neighbor list*)
  let get_enemies neigh_lst =
      List.filter (fun x -> (List.mem (String.uppercase_ascii (get_country_id x)) (List.map (fun x ->String.uppercase_ascii x) neigh_lst)) && 
      (not (List.mem (String.uppercase_ascii (get_country_id x)) ctry_strings))) (get_countries st) in
      let tuple_lst = List.map (fun x -> (x, (get_enemies (get_neighbors x)))) plyr_ctries in

  let rec get_valid_attacker lst = 
    match lst with
    | [] -> failwith "Player owns all countries; cannot be in attack phase"
    | h::t -> if (List.length (get_enemies (get_neighbors h))) = 0 then get_valid_attacker t else h in

  (* used when only ai players exist *)
  let rec get_ai_attacker lst = 
    match lst with
    | [] -> failwith "AI won"
    | h::t -> if (List.length (get_enemies (get_neighbors h))) = 0 then get_ai_attacker t else h in

  if all_ai (List.map (fun x -> get_player_id x) (get_player_list st)) then 
    let poss_att_lst = List.filter (fun x -> (get_troops (get_country_id x) player) > 1)
    (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player)) in 
    if List.length poss_att_lst = 0 then EndPhaseC
    else if List.length (get_player_list st) = 1 then EndPhaseC
    else 
      let att = get_ai_attacker poss_att_lst in 
      let def = get_rand_item (List.map (fun x -> get_country (get_country_id x) (get_countries st)) (get_enemies (get_neighbors att))) in 
      AttackC(get_country_id att, get_country_id def)
  else
      let defend_sort_helper def attacker=
        (*higher, bigger pos difference between attacking and defending country*)
        let troop_diff_factor = get_troops (get_country_id attacker) player - 
            get_troops (get_country_id def) (find_owner (get_country_id def) st) in
        let cont_factor = if (String.uppercase_ascii (get_country_content def)) = (String.uppercase_ascii (get_country_content attacker))
          (*factor in number of other countries owned in def's continent*)
          then let addit_ctrys = List.length (List.filter (fun x -> x = (get_country_content def))
            (List.map (fun x -> fst x) (get_player_countries player))) in
          2 * troop_diff_factor + addit_ctrys else 0 in
        (*change ranking factor based on difficulty*)
        if ai_diff = "h" then troop_diff_factor + cont_factor else troop_diff_factor in
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
        if List.length lst = 0 then 0 else
          let def = List.hd (List.rev (List.rev (List.sort defend_sort lst))) in
          let troop_diff_factor = get_troops (get_country_id att) player - 
            get_troops (get_country_id def) (find_owner (get_country_id def) st)  in
          let cont_factor = if get_country_content def = get_country_content att
            (*factor in number of other countries owned in def's continent*)

            then let addit_ctrys = List.length (List.filter (fun x -> (get_country_content x) = (get_country_content def))
              (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player))) in
            2 * troop_diff_factor + addit_ctrys else 0 in

          (*change ranking factor based on difficulty*)
          if ai_diff = "h" then troop_diff_factor + cont_factor else troop_diff_factor in
      let attack_sort tup1 tup2 =
        let tup1_score = attack_sort_helper tup1 in
        let tup2_score = attack_sort_helper tup2 in
        if tup1_score = tup2_score then 0
        else if tup1_score > tup2_score then 1 else -1 in

      (*returns tuple of attacker, defender
       * tuple_lst is a list of (countyr, defender list) tuples
       *)
      let get_attack tuple_lst =
        (*gets tuple containing most optimum attacker and their list of enemy neighbors*)
        if tuple_lst = [] then failwith "Player has no countries" else
          let attacker_tup = List.hd (List.rev (List.sort attack_sort tuple_lst)) in
          let att = if List.length (get_neighbors (fst attacker_tup)) = 0 
            then get_valid_attacker (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player))
            else fst attacker_tup in
          let def_lst = snd attacker_tup in (*list of possible defenders*)
          let def =
            let def_rank def1 def2 =
              let def1_score = defend_sort_helper def1 att in
              let def2_score = defend_sort_helper def2 att in
              if def1_score = def2_score then 0
              else if def1_score > def2_score then 1 else -1 in
            if def_lst = [] then 
              if List.length (get_enemies (get_neighbors att)) = 0 then att
              else get_rand_item (get_enemies (get_neighbors att))  
            else List.hd (List.rev (List.sort def_rank def_lst)) in
              (att, def) in

      (*most optimal attack, defender pairing*)
      let att_def_tup = get_attack tuple_lst in
      let attacker = if ai_diff = "e" then  get_valid_attacker (List.map (fun x -> get_country (fst x) (get_countries st)) (get_player_countries player)) 
          else fst att_def_tup in  

      let defender = if ai_diff = "e" then get_rand_item (get_enemies (get_neighbors attacker))
          else snd att_def_tup in

      let troop_diff = get_troops (get_country_id attacker) player - 
        get_troops (get_country_id defender) (find_owner (get_country_id defender) st) in

      (*cannot attack self*)
      if attacker = defender then EndPhaseC
      else if get_troops (get_country_id attacker) player < 2 then EndPhaseC
      (*depending on troop_diff, difficulty choose whether or not to attack*)    
      else if ai_diff = "e" then
        if troop_diff > -2 then
        AttackC(get_country_id attacker, get_country_id defender) else EndPhaseC
      else 
        if troop_diff > -2 then
        AttackC(get_country_id attacker, get_country_id defender) else EndPhaseC





let ai_rein st =
  let player = get_player_by_id st (get_cplayer st) in

 let ctry_strings = List.map (fun x ->  (String.uppercase_ascii (fst x))) (get_player_countries player) in
  let plyr_ctries = List.filter (fun x -> List.mem (String.uppercase_ascii (get_country_id x)) ctry_strings) 
    (get_countries st) in

  let ai_diff = String.sub (get_cplayer st) 1 1 in
  (* get total difference between a country's troops
   * and its enemy neighbor troops
   * if the country has no neighbors owned by player, give score of 0*)
  let rein_sort_helper c1 =
    (*number of good neighbors*)
    let neigh_strings = List.map (fun x -> (String.uppercase_ascii x)) (get_neighbors c1) in

   (*extracts friendlys neighbors*)
    let my_allies =
      List.filter (fun x ->  ( (List.mem (String.uppercase_ascii (get_country_id x)) neigh_strings)) 
      && ((List.mem (get_country_id x) (List.map (fun z -> fst z) (get_player_countries player))))) (get_countries st) in

    let num_allies = List.length my_allies in 

    if num_allies = 0 then 0
      else (dep_rank c1 player st) in

  let rein_sort ctry1 ctry2 =
    let rein1_score = rein_sort_helper ctry1 in
    let rein2_score = rein_sort_helper ctry2 in
    if rein1_score = rein2_score then 0
      else if rein1_score > rein2_score then 1 else -1 in
  if List.length plyr_ctries = 0 then EndPhaseC else
    let ctry_to_rein = List.hd(List.rev(List.sort rein_sort plyr_ctries)) in
    (* check number of friendly neighbors...
     * in the case that it's 0, cannot reinforce, return end *)
    let num_rein_neighs = List.length(List.filter (fun x -> List.mem x ctry_strings) (get_neighbors ctry_to_rein)) in
    let com = if num_rein_neighs = 0 then EndPhaseC
      else
        (*find country to be used to reinforce, *)
        let take_rank c1 =
          let enemy_neighbors = List.filter (fun x-> not (List.mem_assoc x (get_player_countries player))) (get_neighbors c1) in

          let c1_troop_diffs = List.map (fun x -> 
            ((get_troops x (find_owner x st)) - (get_troops (get_country_id c1) player))) 
                enemy_neighbors in

          (* if high, country has troops to spare*)
          List.fold_left (+) 0 c1_troop_diffs in

        let take_sort ctry1 ctry2=
          let take1_score = take_rank ctry1 in
          let take2_score = take_rank ctry2 in
          if take1_score = take2_score then 0
            else if take1_score > take2_score then 1 else -1 in
        (*returns list of string of countries linked with *)
        let rec get_links lst acc =
          (* make sure added elements are not already on frontier, have not been visited,
           * and are countries that the player owns*)
          match lst with
          | [] -> acc
          | h::t -> 
            let cl = (List.map (fun (k,v) -> get_country k (get_countries st)) (get_player_countries player)) in
            let new_n = List.map (fun x -> get_country x (get_countries st)) (get_neighbors h) in
            if reinforcable (get_country_id h) (get_country_id ctry_to_rein) new_n cl [] st
            then get_links t (h::acc) else get_links t acc in 

        (*potential links*)
        let pot_links = List.map (fun x-> get_country (fst x) (get_countries st)) (get_player_countries player) in

        (* get countries that are possible to take from*)
        let get_take_lst = get_links pot_links [] in

        if List.length get_take_lst = 0 then EndPhaseC else 
          let ctry_to_take = List.hd (List.rev (List.sort take_sort (get_take_lst))) in

          (*number of troops on country to take from, country to put on*)
          let num_on_take = get_troops (get_country_id ctry_to_take) player in
          let num_on_rein = get_troops (get_country_id ctry_to_rein) player in
          (*measure of strength of enemies near ctrys*)
          let rein_enemy_score = dep_rank ctry_to_rein player st in
          let take_enemy_score = dep_rank ctry_to_rein player st in
          let step_com =
            (*easy ai path*)
            if ai_diff = "e" then
              (*getting neighbors of country to reinforce that have more than one troop*)
              if rein_enemy_score > 0 || (rein_enemy_score < 0 && take_enemy_score < 0) then EndPhaseC
              else let valid_neighs = List.filter (fun x -> get_troops (get_country_id x) player > 1) get_take_lst in
                if List.length valid_neighs = 0 then EndPhaseC
                else let ctry_to_take = get_rand_item valid_neighs in
                  let num_on_take = get_troops (get_country_id ctry_to_take) player in
                  ReinforceC(num_on_take/2, get_country_id ctry_to_rein, get_country_id ctry_to_take)
            (*mid ai path*)
            else if ai_diff = "m" then
              if num_on_take = 1 || (rein_enemy_score > 0) || (rein_enemy_score < 0 && take_enemy_score < 0) then EndPhaseC
              else ReinforceC(num_on_take/2, get_country_id ctry_to_rein, get_country_id ctry_to_take)
            (*if hard ai path*)
            else
              if num_on_take = 1 || (rein_enemy_score > 0) || (rein_enemy_score < 0 && take_enemy_score < 0) then EndPhaseC
              else if num_on_rein > num_on_take
              (*take half of troops from ctry_to_take, put them on ctry_to_rein *)
              then ReinforceC(num_on_take/2, get_country_id ctry_to_rein, get_country_id ctry_to_take)
              (*num_rein reresents number of troops to place*)
              else let num_rein= if num_on_take <= num_on_rein then num_on_take/2
                else num_on_take - 1 in
              ReinforceC(num_rein, get_country_id ctry_to_rein, get_country_id ctry_to_take)
          in step_com in com

let determine_move st =
  let _ = print_string "ai movinng\n" in
  let _ = print_string (string_of_int(List.length(get_player_list st))) in
  match getPhase st with
  | SetUp -> ai_claim st
  | Game(p)->
    match p with
    |Deploy -> ai_deploy st
    |Attack -> ai_attack st
    |Reinforce -> ai_rein st

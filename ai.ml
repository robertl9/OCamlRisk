open State
open Command

  (* checks difficulty of player *)
  let is_beg p =
    match p.real with
    | AI(d) -> d = Beginner
    | _ -> false

  (* returns an assoc lst of all countries in game and the number of troops on them*)
  let get_troops_lst st =
    List.flatten (List.fold_left (@) (List.map (fun x-> get_player_countries x) (get_player_list st)) [])

  (*get country that is gaining a troop in deploy command*)
  let get_deployed com =
  match com with
  | DeployC(_, ctry) -> ctry
  | _ -> failwith "Not a deploy command"

  let inc_deployed com =
  match com with
  | DeployC(num, ctry) -> DeployC(num + 1, ctry)
  | _ -> failwith "Not a deploy command"

  (* returns a state after calling each command in lst,
   * in order from head to tail, on st*)
  let rec do_com_lst st lst =
    match lst with
    | [] -> st
    | a::b -> do_com_lst (do' a st) lst (* print message here too? *)

  let dep_rank c1 player lst=
    let enemy_neighbors = List.filter (fun x-> not (List.mem_assoc x (get_player_countries player))) (get_neighbors c1) in
    (* diff between troops on this country and troops on all other countries should count a bit *)
    (* calc sum of the differences between troops on c1 and
     * neighboring countries not owned by player
    *)
    let c1_troop_diffs = List.map (fun x -> (List.assoc x !lst)
      - (List.assoc (get_country_id c1) !lst)) enemy_neighbors in

    (* if high, greater need for country to gian more troops*)
    List.fold_left (+) 0 c1_troop_diffs


  (* takes in state*)
  let smart_deploy st =
    let player = get_player_by_id st (get_cplayer st) in
    let num_deploy = get_num_deploy player in
    let troop_ctry_lst = ref (get_troops_lst st) in

    let dep_rank_sort c1 c2 =
      let c1_score = dep_rank c1 player troop_ctry_lst in
      let c2_score = dep_rank c2 player troop_ctry_lst in
      if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in

    let rec dep_helper num acc=
      if num = 0 then acc
      else
        (*list sorted by countries that need troops the most*)
        let sorted_lst = List.sort dep_rank_sort
            (List.map (fun x -> get_country (fst x) st) (get_player_countries player)) in
        let ctry_to_dep = get_country_id (List.hd (sorted_lst)) in
        let deployed_countries = List.map (fun x -> get_deployed x) acc in
        let acc =
          (*i.e. if we already deployed a troop to this country*)
          if List.mem ctry_to_dep deployed_countries
        (*update acc so that repetead countyr gets num + 1 for troops*)
        then
          let temp_acc = List.filter (fun x -> (get_deployed x) != ctry_to_dep) acc in
          let new_elt = List.find (fun x -> (get_deployed x) != ctry_to_dep) acc in
          (* increment num troops on country *)
          (inc_deployed new_elt)::temp_acc
        else DeployC(1, ctry_to_dep)::acc in
        (*edit troops_in_st list accordingly to reflect update to acc*)
        let () = troop_ctry_lst := List.map (fun x ->
          if (fst x) = ctry_to_dep then ((fst x), (snd x) + 1) else x)
          !troop_ctry_lst in
        dep_helper (num - 1) acc in

     let dep_lst = dep_helper num_deploy [] in
     do_com_lst st dep_lst


  let smart_claim st =
    let player = get_player_by_id st (get_cplayer st) in
    (* ranking countries to claim*)
    let claim_rank c1 =
      (*get amount of other countries player has on same continent*)
      let c1_com_conts = List.length (List.filter (fun x -> (fst x) = get_country_content c1) (get_player_countries player)) in
      (*see how many countries held have current country as a neighbor*)
      let c1_neighbors = List.length (List.filter
        (fun x -> List.mem (get_country_id c1) (get_neighbors (get_country (fst x) st))) (get_player_countries player)) in
      c1_com_conts + c1_neighbors in

    (* used to sort the list *)
    let comp_c_rank c1 c2 =
      let c1_score = claim_rank(c1) in
      let c2_score = claim_rank(c2) in
      if c1_score = c2_score then 0 else if c1_score > c2_score then 1 else -1 in

    let simp_claim st =
      List.nth (get_unclaimed st) (Random.int (List.length (get_unclaimed st))) in

    (* list of continents that player has a country in *)
    let owned_conts = List.map (fun x -> get_continent_id x) (get_continents player) in

    (* get list of countries available that are in continents that the player also has a country in*)
    let opt_countries = List.filter (fun x ->
        (List.mem (get_country_content (get_country x st)) owned_conts)) (get_unclaimed(st))  in

    let ctry =
      (* choose random country if beginner, or there exists no
       * unavailable countries on continents that player owns
       *)
      if (opt_countries = [] || is_beg player) && get_unclaimed st != []
      then simp_claim st
      else get_country_id (List.hd (List.sort comp_c_rank (List.map (fun x -> (get_country x st)) opt_countries)))in
    if get_unclaimed st!= [] then do' (ClaimC(ctry)) st
    else smart_deploy st


let rec smart_attack st =
  let player = get_player_by_id st (get_cplayer st) in
  let troop_ctry_lst = ref (get_troops_lst st) in

  let defend_sort_helper def attacker=
    (*higher, bigger pos difference between attacking and defending country*)
    let troop_diff_factor = (List.assoc (get_country_id attacker) !troop_ctry_lst)
                            - (List.assoc (get_country_id def) !troop_ctry_lst) in
    let cont_factor = if (get_country_content def) = (get_country_content attacker)
      (*factor in number of other countries owned in def's continent*)
      then let addit_ctrys = List.length (List.filter (fun x -> x = (get_country_content def))
        (List.map (fun x -> fst x) (get_player_countries player))) in
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
    let troop_diff_factor = (List.assoc (get_country_id att) !troop_ctry_lst)
      - (List.assoc (get_country_id def) !troop_ctry_lst) in
    let cont_factor = if get_country_content def = get_country_content att
      (*factor in number of other countries owned in def's continent*)
      then let addit_ctrys = List.length (List.filter (fun x -> x = (get_country_content def))
        (List.map (fun x -> fst x) (get_player_countries player))) in
      2 * troop_diff_factor + addit_ctrys else 0 in
    troop_diff_factor + cont_factor in

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
    let attacker_tup = List.hd (List.sort attack_sort tuple_lst) in
    let att = fst attacker_tup in
    let def_lst = snd attacker_tup in (*list of possible defenders*)

    let def =
      let def_rank def1 def2 =
        let def1_score = defend_sort_helper def1 att in
        let def2_score = defend_sort_helper def2 att in
        if def1_score = def2_score then 0
        else if def1_score > def2_score then 1 else -1 in
      List.hd (List.sort def_rank def_lst) in
    (att, def) in

  (*gets strings of countries player owns*)
  let ctry_strings = List.map (fun x -> fst x) (get_player_countries player) in
  let plyr_ctries = List.filter (fun x -> List.mem (get_country_id x) ctry_strings) (get_countries st) in
  (*extracts enemy neighbors from a country's neighbor list*)
  let get_enemies neigh_lst =
    List.filter (fun x -> not (List.mem (get_country_id x) ctry_strings)) (get_countries st) in
  let tuple_lst = List.map (fun x -> (x, (get_enemies (get_neighbors x)))) plyr_ctries in
  (*most optimal attack, defender pairing*)
  let att_def_tup = get_attack tuple_lst in
  let attacker = fst att_def_tup in
  let defender = snd att_def_tup in
  let troop_diff = (snd (List.find (fun x -> (fst x) = (get_country_id attacker)) !troop_ctry_lst)) -
    (snd (List.find (fun x -> (fst x) = (get_country_id defender)) !troop_ctry_lst)) in

  (* depending on troop_diff, choose whether or not to attack *)
  let new_st = if troop_diff > 1
    then smart_attack (do' (AttackC(get_country_id attacker, get_country_id defender)) st)
    else do' EndPhaseC st in
  new_st


let rec smart_rein st =
  let player = get_player_by_id st (get_cplayer st) in
  let troop_ctry_lst = ref (get_troops_lst st) in
  let ctry_strings = List.map (fun x -> (fst x)) (get_player_countries player) in
  let plyr_ctries = List.filter (fun x -> (List.mem (get_country_id x) ctry_strings)) (get_countries st) in

  (* get total difference between a country's troops
   * and its enemy neighbor troops
   * if the country has no neighbors owned by player, give score of 0*)
  let rein_sort_helper c1 =
    (*number of good neighbors*)
    let num_own_neighs = List.length(List.filter (fun x -> List.mem x ctry_strings) (get_neighbors c1)) in
    if num_own_neighs = 0 then 0
      else (dep_rank c1 player troop_ctry_lst) in

  let rein_sort ctry1 ctry2=
    let rein1_score = rein_sort_helper ctry1 in
    let rein2_score = rein_sort_helper ctry2 in
    if rein1_score = rein2_score then 0
      else if rein1_score > rein2_score then 1 else -1 in

  let ctry_to_rein = List.hd(List.sort rein_sort plyr_ctries) in

  (* check number of friendly neighbors...
   * in the case that it's 0, cannot reinforce, return end *)
  let num_rein_neighs = List.length(List.filter (fun x -> List.mem x ctry_strings) (get_neighbors ctry_to_rein)) in

  let new_st = if num_rein_neighs = 0 then do' EndPhaseC st
    else
      (*find country to be used to reinforce, *)
      let take_rank c1 =
        let enemy_neighbors = List.filter (fun x-> not (List.mem_assoc x (get_player_countries player))) (get_neighbors c1) in
        let c1_troop_diffs = List.map (fun x -> (List.assoc (get_country_id c1) !troop_ctry_lst)
        - (List.assoc x !troop_ctry_lst)) enemy_neighbors in
        (* if high, country has troops to spare*)
        List.fold_left (+) 0 c1_troop_diffs in

      let take_sort ctry1 ctry2=
        let take1_score = take_rank ctry1 in
        let take2_score = take_rank ctry2 in
        if take1_score = take2_score then 0
          else if take1_score > take2_score then 1 else -1 in

      (*returns list of string of countries linked with *)
      let rec get_links visited frontier =
        (* make sure added elements are not already on frontier, have not been visited,
         * and are countries that the player owns*)
        let keep_in x =
          (List.mem x (List.map (fun x -> (fst x)) (get_player_countries player)))
          && (not ((List.mem x frontier) || (List.mem x visited)) ) in

        if frontier = [] then visited
        else
          let popped, frontier, new_visited =
            (* pops country off of frontier*)
            match frontier with
            | ctry_id::b -> ctry_id, b, ctry_id::visited
            (* method would return false in match above
              before calling check_links with empty frontier *)
            | _ -> failwith "Program failure" in

          (* adding neighboring countries to frontier that are not already in frontier,
           * that are held by current player, and that have not been visited yet*)
          let new_frontier =
            (List.filter (fun x -> (keep_in x)) (get_neighbors (get_country popped st)))
            @frontier in
         get_links new_visited new_frontier in
        (*initial fronties consists of country's neighbors owned by player*)
      let init_rein_links = (List.filter (fun x-> (List.mem x ctry_strings)) (get_neighbors ctry_to_rein)) in
      let get_take_lst = get_links [] init_rein_links in

      let ctry_to_take = List.hd(List.sort take_sort
        (List.map (fun x -> (get_country x st)) (get_links [] get_take_lst))) in

      (*number of troops on country to take from, country to put on*)
      let num_on_take = (snd (List.find (fun x -> (fst x) = get_country_id ctry_to_take) !troop_ctry_lst)) in
      let num_on_rein = (snd (List.find (fun x -> (fst x) = get_country_id ctry_to_rein) !troop_ctry_lst)) in

      (*measure of strength of enemies near ctrys*)
      let rein_enemy_score = dep_rank ctry_to_rein player troop_ctry_lst in
      let take_enemy_score = dep_rank ctry_to_rein player troop_ctry_lst in

      let step_st = if num_on_rein = 1 || (rein_enemy_score > 0) then do' EndPhaseC st
        else if num_on_rein > num_on_take
        (*take half of troops from ctry_to_take, put them on ctry_to_rein *)
        then smart_rein(do' (ReinforceC(num_on_take/2, get_country_id ctry_to_rein, get_country_id ctry_to_take)) st)
        (*num_rein reresents number of troops to place*)
        else let num_rein= if num_on_take <= num_on_rein then num_on_take/2
          else num_on_take - 1 in
        smart_rein(do' (ReinforceC(num_rein, get_country_id ctry_to_rein, get_country_id ctry_to_take)) st) in
        step_st in new_st

let rec determine_move st =
  match st.c_phase with
  | SetUp ->  determine_move (smart_claim st)
  | Game(p)->
        match p with
        |Deploy -> determine_move (smart_deploy st)
        |Attack -> determine_move (smart_attack st)
        |Reinforce -> smart_rein st

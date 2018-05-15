open OUnit2
open State
open Command
open Ai

let file = Yojson.Basic.from_file "dummy.json"

let state1 = init_state 3 0 0 0 file
let state2 = init_state 3 0 0 0 file
let player1 = get_player_of_state state1
let string1 = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!"

let command2 = parse "claim china"
let state2 = do' command2 state2
let player1' = get_player_by_id state2 "1"
let player2 = get_player_by_id state2 "2"

let command3 = parse "claim india"
let state3 = init_state 3 0 0 0 file
let state3 = do' command2 state3
let state3 = do' command3 state3
let player1'' = get_player_by_id state3 "1"
let player2' = get_player_by_id state3 "2"
let player3 = get_player_by_id state3 "3"

let command4 = parse "claim japan"
let state4 = init_state 3 0 0 0 file
let state4 = do' command2 state4
let state4 = do' command3 state4
let state4 = do' command4 state4
let player13 = get_player_by_id state4 "1"
let player22 = get_player_by_id state4 "2"
let player31 = get_player_by_id state4 "3"

let command5 = parse "deploy 5 china"
let state5 = init_state 3 0 0 0 file
let state5 = do' command2 state5
let state5 = do' command3 state5
let state5 = do' command4 state5
let state5 = do' command5 state5
let player14 = get_player_by_id state5 "1"
let player23 = get_player_by_id state5 "2"
let player32 = get_player_by_id state5 "3"

let command6 = parse "deploy 23 china"
let state6 = init_state 3 0 0 0 file
let state6 = do' command2 state6
let state6 = do' command3 state6
let state6 = do' command4 state6
let state6 = do' command5 state6
let state6 = do' command6 state6
let player15 = get_player_by_id state6 "1"
let player24 = get_player_by_id state6 "2"
let player33 = get_player_by_id state6 "3"

let command7 = parse "attack china japan"
let state7 = init_state 3 0 0 0 file
let state7 = do' command2 state7
let state7 = do' command3 state7
let state7 = do' command4 state7
let state7 = do' command5 state7
let state7 = do' command6 state7
let state7 = do' command7 state7
let player16 = get_player_by_id state7 "1"
let player25 = get_player_by_id state7 "2"
let player34 = get_player_by_id state7 "3"



let rec remove lst elem =
  match lst with
  | [] -> []
  | h::t -> if h = elem then t else h::(remove t elem)

let rec compare_item lst1 lst2 =
  match lst1 with
  | [] -> true
  | h::t -> let removed = remove lst2 h in
    if List.mem h lst2 then compare_item t removed else false

let disregard_order lst1 lst2 =
  if List.length lst1 = List.length lst2 then compare_item lst1 lst2
  else false

let tests = [
  "takenby1" >:: (fun _ -> assert_equal [] (taken_by state1 player1));
  (* "available1" >:: (fun _ -> assert_equal ["Japan"; "China"; "India"] (available state1)); *)
  "win1" >:: (fun _ -> assert_equal false (win state1));
  "cards_owned_1" >:: (fun _ -> assert_equal [] (cards_owned player1));
  "getcplayer1" >:: (fun _ -> assert_equal "1" (get_cplayer state1));
  "get_msg" >:: (fun _ -> assert_equal string1 (get_msg state1));

  "takenby1'" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state2 player1'));
  "takenby2" >:: (fun _ -> assert_equal [] (taken_by state2 player2));
  "win2" >:: (fun _ -> assert_equal false (win state2));
  "cards_owned_2" >:: (fun _ -> assert_equal [] (cards_owned player1));
  "getcplayer2" >:: (fun _ -> assert_equal "2" (get_cplayer state2));
  "get_msg2" >:: (fun _ -> assert_equal "1 has claimed CHINA" (get_msg state2));

  "takenby1''" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state3 player1''));
  "takenby2'" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state3 player2'));
  "win3" >:: (fun _ -> assert_equal false (win state3));
  "cards_owned_3" >:: (fun _ -> assert_equal [] (cards_owned player2));
  "getcplayer3" >:: (fun _ -> assert_equal "3" (get_cplayer state3));
  "get_msg3" >:: (fun _ -> assert_equal "2 has claimed INDIA" (get_msg state3));

  "takenby13" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state4 player13));
  "takenby22" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state4 player22));
  "takenby3" >:: (fun _ -> assert_equal ["JAPAN"] (taken_by state4 player31));
  "win4" >:: (fun _ -> assert_equal false (win state4));
  "cards_owned_4" >:: (fun _ -> assert_equal [] (cards_owned player3));
  "get_msg4" >:: (fun _ -> assert_equal "3 has claimed JAPAN" (get_msg state4));

  "takenby14" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state5 player14));
  "takenby23" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state5 player23));
  "takenby31" >:: (fun _ -> assert_equal ["JAPAN"] (taken_by state5 player32));
  "win5" >:: (fun _ -> assert_equal false (win state5));
  "takenby15" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state6 player15));
  "takenby24" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state6 player24));
  "takenby32" >:: (fun _ -> assert_equal ["JAPAN"] (taken_by state6 player33));
  "win6" >:: (fun _ -> assert_equal false (win state6));

  "takenby25" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state7 player25));
  "win7" >:: (fun _ -> assert_equal false (win state7));
]

let file1 = Yojson.Basic.from_file "map.json"
let full1 = ref (init_state 5 0 0 0 file1)

let full_tests = [

  (*Begin test cases to see if claiming works with only human players Checking
    to make sure that only claimed countries can be claimed and countries can be
    claimed until there are no longer any free countries. Also ensuring that each
    player is getting countries added to their list of countries*)
  "correctPlayers" >:: (fun _ -> assert_equal 5 (List.length (get_player_list !full1)));

  "noclaim" >:: (fun _ -> assert_equal 20 (List.length (get_unclaimed !full1)));
  "correctPlayer1" >:: (fun _ -> assert_equal "1" (get_cplayer !full1));
  "cpc1" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim1" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Winterfell")) !full1)));
  "claimed1" >:: (fun _ -> assert_equal 19 (List.length (get_unclaimed !full1)));
  "cpc1,1" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "1"))));
  "correctPlayer2" >:: (fun _ -> assert_equal "2" (get_cplayer !full1));
  "cpc2" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim2" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Skagos")) !full1)));
  "claimed2" >:: (fun _ -> assert_equal 18 (List.length (get_unclaimed !full1)));
  "cp1,2" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "2"))));
  "correctPlayer3" >:: (fun _ -> assert_equal "3" (get_cplayer !full1));
  "cpc3" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim3" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Iron Islands")) !full1)));
  "claimed3" >:: (fun _ -> assert_equal 17 (List.length (get_unclaimed !full1)));
  "cp1,3" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "3"))));
  "correctPlayer4" >:: (fun _ -> assert_equal "4" (get_cplayer !full1));
  "cpc3" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim4" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Casterly\nRock")) !full1)));
  "claimed4" >:: (fun _ -> assert_equal 16 (List.length (get_unclaimed !full1)));
  "cp1,4" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "4"))));
  "correctPlayer5" >:: (fun _ -> assert_equal "5" (get_cplayer !full1));
  "cpc4" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim5" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Vale")) !full1)));
  "claimed5" >:: (fun _ -> assert_equal 15 (List.length (get_unclaimed !full1)));
  "cp1,5" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "5"))));
  "correctPlayer6" >:: (fun _ -> assert_equal "1" (get_cplayer !full1));
  "cpc5" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim6" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Winter")) !full1)));
  "claimed6" >:: (fun _ -> assert_equal 14 (List.length (get_unclaimed !full1)));
  "cp2,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "1"))));
  "correctPlayer7" >:: (fun _ -> assert_equal "2" (get_cplayer !full1));
  "cpc6" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim7" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Riverrun")) !full1)));
  "claimed7" >:: (fun _ -> assert_equal 13 (List.length (get_unclaimed !full1)));
  "cp3,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "2"))));
  "correctPlayer8" >:: (fun _ -> assert_equal "3" (get_cplayer !full1));
  "cpc7" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim8" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Storm's End")) !full1)));
  "claimed8" >:: (fun _ -> assert_equal 12 (List.length (get_unclaimed !full1)));
  "cp4,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "3"))));
  "correctPlayer9" >:: (fun _ -> assert_equal "4" (get_cplayer !full1));
  "cpc8" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim9" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Dragon-\nstone")) !full1)));
  "claimed9" >:: (fun _ -> assert_equal 11 (List.length (get_unclaimed !full1)));
  "cp5,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "4"))));
  "correctPlayer10" >:: (fun _ -> assert_equal "5" (get_cplayer !full1));
  "cpc9" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim11" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Highgarden")) !full1)));
  "claimed11" >:: (fun _ -> assert_equal 10 (List.length (get_unclaimed !full1)));
  "cp11" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "5"))));
  "correctPlayer11" >:: (fun _ -> assert_equal "1" (get_cplayer !full1));
  "cpc11" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim12" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Dorne")) !full1)));
  "claimed12" >:: (fun _ -> assert_equal 9 (List.length (get_unclaimed !full1)));
  "cp12" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "1"))));
  "correctPlayer12" >:: (fun _ -> assert_equal "2" (get_cplayer !full1));
  "cpc12" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim13" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Braavos")) !full1)));
  "claimed13" >:: (fun _ -> assert_equal 8 (List.length (get_unclaimed !full1)));
  "cp13" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "2"))));
  "correctPlayer13" >:: (fun _ -> assert_equal "3" (get_cplayer !full1));
  "cpc13" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim14" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Pentos")) !full1)));
  "claimed14" >:: (fun _ -> assert_equal 7 (List.length (get_unclaimed !full1)));
  "cp14" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "3"))));
  "correctPlayer14" >:: (fun _ -> assert_equal "4" (get_cplayer !full1));
  "cpc14" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim15" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Tyrosh")) !full1)));
  "claimed15" >:: (fun _ -> assert_equal 6 (List.length (get_unclaimed !full1)));
  "cp15" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "4"))));
  "correctPlayer15" >:: (fun _ -> assert_equal "5" (get_cplayer !full1));
  "cpc15" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim16" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Qohor")) !full1)));
  "claimed16" >:: (fun _ -> assert_equal 5 (List.length (get_unclaimed !full1)));
  "cp16" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "5"))));
  "correctPlayer16" >:: (fun _ -> assert_equal "1" (get_cplayer !full1));
  "cpc16" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim17" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Volantis")) !full1)));
  "claimed17" >:: (fun _ -> assert_equal 4 (List.length (get_unclaimed !full1)));
  "cp17" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "1"))));
  "correctPlayer17" >:: (fun _ -> assert_equal "2" (get_cplayer !full1));
  "cpc17" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim18" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Dothraki Sea")) !full1)));
  "claimed18" >:: (fun _ -> assert_equal 3 (List.length (get_unclaimed !full1)));
  "cp18" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "2"))));
  "correctPlayer18" >:: (fun _ -> assert_equal "3" (get_cplayer !full1));
  "cpc18" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim19" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Slaver Bay")) !full1)));
  "claimed19" >:: (fun _ -> assert_equal 2 (List.length (get_unclaimed !full1)));
  "cp19" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "3"))));
  "correctPlayer19" >:: (fun _ -> assert_equal "4" (get_cplayer !full1));
  "cpc19" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim20" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Red Waste")) !full1)));
  "claimed20" >:: (fun _ -> assert_equal 1 (List.length (get_unclaimed !full1)));
  "cp20" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "4"))));
  "correctPlayer20" >:: (fun _ -> assert_equal "5" (get_cplayer !full1));
  "cpc20" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "claim21" >:: (fun _ -> assert_equal () (full1:= (do' (ClaimC("Unknown Lands")) !full1)));
  "claimed21" >:: (fun _ -> assert_equal 0 (List.length (get_unclaimed !full1)));
  "cp21" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "5"))));
  "correctPlayer21" >:: (fun _ -> assert_equal "1" (get_cplayer !full1));
  "cpc21" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_of_state !full1))));
  (*End test cases to see if claiming works with only human players*)
]


let full1 = ref (init_state 0 5 0 0 file1)

let full_tests_AI = [
  "aecorrectPlayers" >:: (fun _ -> assert_equal 5 (List.length (get_player_list !full1)));

  "aenoclaim" >:: (fun _ -> assert_equal 20 (List.length (get_unclaimed !full1)));
  "aecorrectPlayer1" >:: (fun _ -> assert_equal "ae1" (get_cplayer !full1));
  "aecpc1" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim1" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed1" >:: (fun _ -> assert_equal 19 (List.length (get_unclaimed !full1)));
  "aecpc1,1" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "ae1"))));
  "aecorrectPlayer2" >:: (fun _ -> assert_equal "ae2" (get_cplayer !full1));
  "aecpc2" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim2" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed2" >:: (fun _ -> assert_equal 18 (List.length (get_unclaimed !full1)));
  "aecp1,2" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "ae2"))));
  "aecorrectPlayer3" >:: (fun _ -> assert_equal "ae3" (get_cplayer !full1));
  "aecpc3" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim3" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed3" >:: (fun _ -> assert_equal 17 (List.length (get_unclaimed !full1)));
  "aecp1,3" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "ae3"))));
  "aecorrectPlayer4" >:: (fun _ -> assert_equal "ae4" (get_cplayer !full1));
  "aecpc3" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim4" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed4" >:: (fun _ -> assert_equal 16 (List.length (get_unclaimed !full1)));
  "aecp1,4" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "ae4"))));
  "aecorrectPlayer5" >:: (fun _ -> assert_equal "ae5" (get_cplayer !full1));
  "aecpc4" >:: (fun _ -> assert_equal 0 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim5" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed5" >:: (fun _ -> assert_equal 15 (List.length (get_unclaimed !full1)));
  "aecp1,5" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_by_id !full1 "ae5"))));
  "aecorrectPlayer6" >:: (fun _ -> assert_equal "ae1" (get_cplayer !full1));
  "aecpc5" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim6" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed6" >:: (fun _ -> assert_equal 14 (List.length (get_unclaimed !full1)));
  "aecp2,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "ae1"))));
  "aecorrectPlayer7" >:: (fun _ -> assert_equal "ae2" (get_cplayer !full1));
  "aecpc6" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim7" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed7" >:: (fun _ -> assert_equal 13 (List.length (get_unclaimed !full1)));
  "aecp3,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "ae2"))));
  "aecorrectPlayer8" >:: (fun _ -> assert_equal "ae3" (get_cplayer !full1));
  "aecpc7" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim8" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed8" >:: (fun _ -> assert_equal 12 (List.length (get_unclaimed !full1)));
  "aecp4,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "ae3"))));
  "aecorrectPlayer9" >:: (fun _ -> assert_equal "ae4" (get_cplayer !full1));
  "aecpc8" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim9" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed9" >:: (fun _ -> assert_equal 11 (List.length (get_unclaimed !full1)));
  "aecp5,1" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "ae4"))));
  "aecorrectPlayer10" >:: (fun _ -> assert_equal "ae5" (get_cplayer !full1));
  "aecpc9" >:: (fun _ -> assert_equal 1 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim11" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed11" >:: (fun _ -> assert_equal 10 (List.length (get_unclaimed !full1)));
  "aecp11" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_by_id !full1 "ae5"))));
  "aecorrectPlayer11" >:: (fun _ -> assert_equal "ae1" (get_cplayer !full1));
  "aecpc11" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim12" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed12" >:: (fun _ -> assert_equal 9 (List.length (get_unclaimed !full1)));
  "aecp12" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "ae1"))));
  "aecorrectPlayer12" >:: (fun _ -> assert_equal "ae2" (get_cplayer !full1));
  "aecpc12" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim13" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed13" >:: (fun _ -> assert_equal 8 (List.length (get_unclaimed !full1)));
  "aecp13" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "ae2"))));
  "aecorrectPlayer13" >:: (fun _ -> assert_equal "ae3" (get_cplayer !full1));
  "aecpc13" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim14" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed14" >:: (fun _ -> assert_equal 7 (List.length (get_unclaimed !full1)));
  "aecp14" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "ae3"))));
  "aecorrectPlayer14" >:: (fun _ -> assert_equal "ae4" (get_cplayer !full1));
  "aecpc14" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim15" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed15" >:: (fun _ -> assert_equal 6 (List.length (get_unclaimed !full1)));
  "aecp15" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "ae4"))));
  "aecorrectPlayer15" >:: (fun _ -> assert_equal "ae5" (get_cplayer !full1));
  "aecpc15" >:: (fun _ -> assert_equal 2 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim16" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed16" >:: (fun _ -> assert_equal 5 (List.length (get_unclaimed !full1)));
  "aecp16" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_by_id !full1 "ae5"))));
  "aecorrectPlayer16" >:: (fun _ -> assert_equal "ae1" (get_cplayer !full1));
  "aecpc16" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim17" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed17" >:: (fun _ -> assert_equal 4 (List.length (get_unclaimed !full1)));
  "aecp17" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "ae1"))));
  "aecorrectPlayer17" >:: (fun _ -> assert_equal "ae2" (get_cplayer !full1));
  "aecpc17" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim18" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed18" >:: (fun _ -> assert_equal 3 (List.length (get_unclaimed !full1)));
  "aecp18" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "ae2"))));
  "aecorrectPlayer18" >:: (fun _ -> assert_equal "ae3" (get_cplayer !full1));
  "aecpc18" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim19" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed19" >:: (fun _ -> assert_equal 2 (List.length (get_unclaimed !full1)));
  "aecp19" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "ae3"))));
  "aecorrectPlayer19" >:: (fun _ -> assert_equal "ae4" (get_cplayer !full1));
  "aecpc19" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim20" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed20" >:: (fun _ -> assert_equal 1 (List.length (get_unclaimed !full1)));
  "aecp20" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "ae4"))));
  "aecorrectPlayer20" >:: (fun _ -> assert_equal "ae5" (get_cplayer !full1));
  "aecpc20" >:: (fun _ -> assert_equal 3 (List.length (get_player_countries (get_player_of_state !full1))));

  "aeclaim21" >:: (fun _ -> assert_equal () (full1:= (do' (determine_move !full1) !full1)));
  "aeclaimed21" >:: (fun _ -> assert_equal 0 (List.length (get_unclaimed !full1)));
  "aecp21" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_by_id !full1 "ae5"))));
  "aecorrectPlayer21" >:: (fun _ -> assert_equal "ae1" (get_cplayer !full1));
  "aecpc21" >:: (fun _ -> assert_equal 4 (List.length (get_player_countries (get_player_of_state !full1))));
]








let suite =
  "OCamlRisk test suite"
  >::: List.flatten [tests;full_tests;full_tests_AI]

let _ = run_test_tt_main suite

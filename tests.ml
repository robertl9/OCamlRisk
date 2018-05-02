open OUnit2
open State
open Command

let file = Yojson.Basic.from_file "dummy.json"

let state1 = init_state 3 file
let state2 = init_state 3 file
let player1 = get_player_of_state state1
let string1 = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!"

let command2 = parse "claim china"
let state2 = do' command2 state2
let player1' = get_player_by_id state2 "1"
let player2 = get_player_by_id state2 "2"

let command3 = parse "claim india"
let state3 = init_state 3 file
let state3 = do' command2 state3
let state3 = do' command3 state3
let player1'' = get_player_by_id state3 "1"
let player2' = get_player_by_id state3 "2"
let player3 = get_player_by_id state3 "3"

let command4 = parse "claim japan"
let state4 = init_state 3 file
let state4 = do' command2 state4
let state4 = do' command3 state4
let state4 = do' command4 state4
let player13 = get_player_by_id state4 "1"
let player22 = get_player_by_id state4 "2"
let player31 = get_player_by_id state4 "3"

let command5 = parse "deploy 5 china"
let state5 = init_state 3 file
let state5 = do' command2 state5
let state5 = do' command3 state5
let state5 = do' command4 state5
let state5 = do' command5 state5
let player14 = get_player_by_id state5 "1"
let player23 = get_player_by_id state5 "2"
let player32 = get_player_by_id state5 "3"

let command6 = parse "deploy 23 china"
let state6 = init_state 3 file
let state6 = do' command2 state6
let state6 = do' command3 state6
let state6 = do' command4 state6
let state6 = do' command5 state6
let state6 = do' command6 state6
let player15 = get_player_by_id state6 "1"
let player24 = get_player_by_id state6 "2"
let player33 = get_player_by_id state6 "3"

let command7 = parse "attack china japan"
let state7 = init_state 3 file
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
  "available1" >:: (fun _ -> assert_equal true
                       (disregard_order ["JAPAN"; "CHINA"; "INDIA"] (available state1))) ;
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
  "get_msg5" >:: (fun _ -> assert_equal "CHINA has gained 5 troop!" (get_msg state5));

  "takenby15" >:: (fun _ -> assert_equal ["CHINA"] (taken_by state6 player15));
  "takenby24" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state6 player24));
  "takenby32" >:: (fun _ -> assert_equal ["JAPAN"] (taken_by state6 player33));
  "win6" >:: (fun _ -> assert_equal false (win state6));
  "get_msg6" >:: (fun _ -> assert_equal "CHINA has gained 23 troop!" (get_msg state6));

  "takenby16" >:: (fun _ -> assert_equal ["JAPAN"; "CHINA"] (taken_by state7 player16));
  "takenby25" >:: (fun _ -> assert_equal ["INDIA"] (taken_by state7 player25));
  "takenby33" >:: (fun _ -> assert_equal [] (taken_by state7 player34));
  "win7" >:: (fun _ -> assert_equal false (win state7));
  "get_msg7" >:: (fun _ -> assert_equal "1 has conquered JAPAN!" (get_msg state7));

]

let suite =
  "OCamlRisk test suite"
  >::: tests

let _ = run_test_tt_main suite

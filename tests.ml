open OUnit2
open State
open Command

let file = Yojson.Basic.from_file "dummy.json"

let state1 = init_state 3 file
let player1 = get_player_of_state state1
let string1 = "Welcome to Risk! Your game creators are Milan Shah, Jonvi Rollins, Robert Li, and Abdullah Islam!"

let rec remove lst elem =
  match lst with
  | [] -> [];
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
  "available1" >:: (fun _ -> assert_equal ["Japan", "China", "India"] (available state1));
  "win1" >:: (fun _ -> assert_equal false (win state1));
  "cards_owned_1" >:: (fun _ -> assert_equal [] (cards_owned player1));
  "getcplayer1" >:: (fun _ -> assert_equal "1" (get_cplayer state1));
  "get_msg" >:: (fun _ -> assert_equal string1 (get_msg state1));
]

let suite =
  "OCamlRisk test suite"
  >::: tests

let _ = run_test_tt_main suite

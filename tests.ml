open OUnit2
open State
open Command

let file = Yojson.Basic.from_file "dummy.json"

let available1 = ["Japan"; "China"; "India"]
let state1 = file |> init_state

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
  "test1" >:: (fun _ -> assert_equal true
                        (disregard_order available1
                           (state1 |> current_items))) ;
  "test1" >:: (fun _ -> assert_equal true
                                                 (disregard_order available1
                                                    (state1 |> current_items))) ;
]

let suite =
  "OCamlRisk test suite"
  >::: tests

let _ = run_test_tt_main suite

open OUnit2
open State
open Command

let tests = [] 

let suite =
  "OCamlRisk test suite"
  >::: tests

let _ = run_test_tt_main suite

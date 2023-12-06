open OUnit2
open Game
open Card

let t1 =
  [ (* "get_color" >:: assert_equal "Red" (get_color (make_card "Red" "2"
       "None")); *) ]

let tests = "cards test suite" >::: t1
let () = run_test_tt_main tests

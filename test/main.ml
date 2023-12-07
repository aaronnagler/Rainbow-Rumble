open OUnit2
open Game
open Card

let card_tests =
  [
    ( "get_color trivial case" >:: fun _ ->
      assert_equal "Red" (get_color (make_card "Red" "2" "None")) );
    ( "get_color card with property" >:: fun _ ->
      assert_equal "Blue" (get_color (make_card "Blue" "2" "Draw 2")) );
    ( "get_color" >:: fun _ ->
      assert_equal "Yellow" (get_color (make_card "Yellow" "2" "Draw 4")) );
    ( "get_number" >:: fun _ ->
      assert_equal "5" (get_number (make_card "Red" "5" "None")) );
  ]

let suite = "test suite for cards" >::: List.flatten [ card_tests ]
let () = run_test_tt_main suite

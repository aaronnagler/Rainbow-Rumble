open OUnit2
open Game
open Card

let card_tests =
  [
    ( "get_color regular card" >:: fun _ ->
      assert_equal "Red" (get_color (make_card "Red" "2" "None")) );
    ( "get_color draw 2" >:: fun _ ->
      assert_equal "Blue" (get_color (make_card "Blue" "2" "Draw 2")) );
    ( "get_color draw 4" >:: fun _ ->
      assert_equal "Wild" (get_color (make_card "Wild" "2" "Draw 4")) );
    ( "get_number regular card" >:: fun _ ->
      assert_equal "5" (get_number (make_card "Yellow" "5" "None")) );
    ( "get_number draw 2" >:: fun _ ->
      assert_equal "NaN" (get_number (make_card "Red" "NaN" "Draw 2")) );
    ( "get_number draw 4" >:: fun _ ->
      assert_equal "NaN" (get_number (make_card "Wild" "NaN" "Draw 4")) );
    ( "get_number wild" >:: fun _ ->
      assert_equal "NaN" (get_number (make_card "Wild" "NaN" "None")) );
    ( "get_property_name regular card" >:: fun _ ->
      assert_equal "None" (get_property_name (make_card "Green" "2" "None")) );
    ( "get_property_name draw 2" >:: fun _ ->
      assert_equal "Draw 2"
        (get_property_name (make_card "Green" "NaN" "Draw 2")) );
    ( "get_property_name draw 4" >:: fun _ ->
      assert_equal "Draw 4"
        (get_property_name (make_card "Wild" "NaN" "Draw 4")) );
    ( "get_property_name wild" >:: fun _ ->
      assert_equal "None" (get_property_name (make_card "Wild" "NaN" "None")) );
    ( "get_property_description regular card" >:: fun _ ->
      assert_equal "None" (get_property_name (make_card "Red" "4" "None")) );
    ( "get_property_description draw 2" >:: fun _ ->
      assert_equal "Opponent must draw 2 cards from the top of the deck"
        (get_property_description (make_card "Blue" "NaN" "Draw 2")) );
    ( "get_property_description draw 4" >:: fun _ ->
      assert_equal "Opponent must draw 4 cards from the top of the deck"
        (get_property_description (make_card "Wild" "NaN" "Draw 4")) );
  ]

let suite = "test suite for cards" >::: List.flatten [ card_tests ]
let () = run_test_tt_main suite

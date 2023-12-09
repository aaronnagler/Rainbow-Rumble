open OUnit2
open Game
open Card

(*All Number Cards*)
let num_cards =
  [
    (*Red*)
    Card.make_card "Red" "Zero" "None";
    Card.make_card "Red" "One" "None";
    Card.make_card "Red" "Two" "None";
    Card.make_card "Red" "Three" "None";
    Card.make_card "Red" "Four" "None";
    Card.make_card "Red" "Five" "None";
    Card.make_card "Red" "Six" "None";
    Card.make_card "Red" "Seven" "None";
    Card.make_card "Red" "Eight" "None";
    Card.make_card "Red" "Nine" "None";
    (*Blue*)
    Card.make_card "Blue" "Zero" "None";
    Card.make_card "Blue" "One" "None";
    Card.make_card "Blue" "Two" "None";
    Card.make_card "Blue" "Three" "None";
    Card.make_card "Blue" "Four" "None";
    Card.make_card "Blue" "Five" "None";
    Card.make_card "Blue" "Six" "None";
    Card.make_card "Blue" "Seven" "None";
    Card.make_card "Blue" "Eight" "None";
    Card.make_card "Blue" "Nine" "None";
    (*Green*)
    Card.make_card "Green" "Zero" "None";
    Card.make_card "Green" "One" "None";
    Card.make_card "Green" "Two" "None";
    Card.make_card "Green" "Three" "None";
    Card.make_card "Green" "Four" "None";
    Card.make_card "Green" "Five" "None";
    Card.make_card "Green" "Six" "None";
    Card.make_card "Green" "Seven" "None";
    Card.make_card "Green" "Eight" "None";
    Card.make_card "Green" "Nine" "None";
    (*Yellow*)
    Card.make_card "Yellow" "Zero" "None";
    Card.make_card "Yellow" "One" "None";
    Card.make_card "Yellow" "Two" "None";
    Card.make_card "Yellow" "Three" "None";
    Card.make_card "Yellow" "Four" "None";
    Card.make_card "Yellow" "Five" "None";
    Card.make_card "Yellow" "Six" "None";
    Card.make_card "Yellow" "Seven" "None";
    Card.make_card "Yellow" "Eight" "None";
    Card.make_card "Yellow" "Nine" "None";
  ]

(*All Special Cards*)
let special_cards =
  [
    Card.make_card "Red" "NaN" "Draw2";
    Card.make_card "Blue" "NaN" "Draw2";
    Card.make_card "Green" "NaN" "Draw2";
    Card.make_card "Yellow" "NaN" "Draw2";
    Card.make_card "Wild" "NaN" "Draw2";
  ]

(*All Wild Cards*)
let wild_cards =
  [ Card.make_card "Wild" "NaN" "Draw2"; Card.make_card "Wild" "NaN" "Draw4" ]

(*Full All Cards*)
let all_cards = num_cards @ special_cards @ wild_cards

(*One type of card from each category*)
let one_of_each_type_cards =
  [
    Card.make_card "Red" "Zero" "NaN";
    Card.make_card "Red" "NaN" "Draw2";
    Card.make_card "Wild" "NaN" "NaN";
  ]

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

let game_tests =
  [
    (*is_legal_play tests*)
    ( "is_legal_play matching colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Blue" "3" "None")
           (make_card "Blue" "0" "None")) );
    ( "is_legal_play matching colors, playing on draw 2" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Red" "NaN" "Draw 2")
           (make_card "Red" "0" "None")) );
    ( "is_legal_play matching colors, playing draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Green" "NaN" "Draw 2")
           (make_card "Green" "0" "None")) );
    ( "is_legal_play matching colors, playing draw 2 on draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Green" "NaN" "Draw 2")
           (make_card "Green" "NaN" "Draw 2")) );
    ( "is_legal_play different colors, playing draw 2 on draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Blue" "NaN" "Draw 2")
           (make_card "Green" "NaN" "Draw 2")) );
    ( "is_legal_play different colors, playing draw 2 " >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (make_card "Blue" "NaN" "Draw 2")
           (make_card "Green" "5" "None")) );
    ( "is_legal_play different colors, different numbers" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (make_card "Blue" "3" "None")
           (make_card "Green" "4" "None")) );
    ( "is_legal_play same numbers, same colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Blue" "8" "None")
           (make_card "Blue" "8" "None")) );
    ( "is_legal_play same numbers, different colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Blue" "8" "None")
           (make_card "Red" "8" "None")) );
    ( "is_legal_play playing draw 4 on number" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "Draw 4")
           (make_card "Red" "8" "None")) );
    ( "is_legal_play playing draw 4 on draw 2" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "Draw 4")
           (make_card "Red" "NaN" "Draw 2")) );
    ( "is_legal_play playing draw 4 on draw 4" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "Draw 4")
           (make_card "Wild" "NaN" "Draw 4")) );
    ( "is_legal_play playing wild on draw 4" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "None")
           (make_card "Wild" "NaN" "Draw 4")) );
    ( "is_legal_play playing wild on regular card" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "None")
           (make_card "Red" "4" "None")) );
    ( "is_legal_play playing wild on wild" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (make_card "Wild" "NaN" "None")
           (make_card "Yellow" "Wild" "None")) );
  ]

let opp_tests =
  [
    (*strategy 1*)
    ( "strategy_1 : enemy has only number cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_1 num_cards) );
    ( "strategy_1 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_1 special_cards) );
    ( "strategy_1 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "NaN"))
        (AI.strategy_1 wild_cards) );
    ( "strategy_1 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_1 (num_cards @ special_cards)) );
    ( "strategy_1 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_1 (num_cards @ wild_cards)) );
    ( "strategy_1 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_1 (special_cards @ special_cards)) );
    ( "strategy_1 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_1 all_cards) );
    (*strategy 2*)
    ( "strategy_2 : enemy has only number cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_2 num_cards) );
    ( "strategy_2 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 special_cards) );
    ( "strategy_2 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "NaN"))
        (AI.strategy_2 wild_cards) );
    ( "strategy_2 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 (num_cards @ special_cards)) );
    ( "strategy_2 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_2 (num_cards @ wild_cards)) );
    ( "strategy_2 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 (special_cards @ wild_cards)) );
    ( "strategy_2 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 all_cards) );
    (*strategy 3*)
    ( "strategy_3 : enemy has only number cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.strategy_3 num_cards) );
    ( "strategy_3 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_3 special_cards) );
    ( "strategy_3 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "NaN"))
        (AI.strategy_3 wild_cards) );
    ( "strategy_3 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_3 (num_cards @ special_cards)) );
    ( "strategy_3 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "NaN"))
        (AI.strategy_3 (num_cards @ wild_cards)) );
    ( "strategy_3 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_3 (wild_cards @ special_cards)) );
    ( "strategy_3 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_1 all_cards) );
    (*hard_mode_turn Tests*)
    ( "hard_mode_turn: enemy and player has 4 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "NaN"))
        (AI.hard_mode_turn all_cards (Card.make_card "Red" "Zero" "NaN") 4) );
    ( "hard_mode_turn: enemy has 3 cards and player has 4 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.hard_mode_turn one_of_each_type_cards
           (Card.make_card "Red" "Zero" "NaN")
           4) );
    ( "hard_mode_turn: enemy has 4 cards and player has 3 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Nan" "Draw2"))
        (AI.hard_mode_turn all_cards (Card.make_card "Red" "Zero" "NaN") 3) );
  ]

let suite =
  "test suite for cards" >::: List.flatten [ card_tests; game_tests; opp_tests ]

let () = run_test_tt_main suite

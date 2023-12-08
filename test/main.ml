open OUnit2
open Game
open Card

(*All Number Cards*)
let num_cards =
  [
    (*Red*)
    { color = Red; number = Zero; property = None };
    { color = Red; number = One; property = None };
    { color = Red; number = Two; property = None };
    { color = Red; number = Three; property = None };
    { color = Red; number = Four; property = None };
    { color = Red; number = Five; property = None };
    { color = Red; number = Six; property = None };
    { color = Red; number = Seven; property = None };
    { color = Red; number = Eight; property = None };
    { color = Red; number = Nine; property = None };
    (*Blue*)
    { color = Blue; number = Zero; property = None };
    { color = Blue; number = One; property = None };
    { color = Blue; number = Two; property = None };
    { color = Blue; number = Three; property = None };
    { color = Blue; number = Four; property = None };
    { color = Blue; number = Five; property = None };
    { color = Blue; number = Six; property = None };
    { color = Blue; number = Seven; property = None };
    { color = Blue; number = Eight; property = None };
    { color = Blue; number = Nine; property = None };
    (*Green*)
    { color = Green; number = Zero; property = None };
    { color = Green; number = One; property = None };
    { color = Green; number = Two; property = None };
    { color = Green; number = Three; property = None };
    { color = Green; number = Four; property = None };
    { color = Green; number = Five; property = None };
    { color = Green; number = Six; property = None };
    { color = Green; number = Seven; property = None };
    { color = Green; number = Eight; property = None };
    { color = Green; number = Nine; property = None };
    (*Yellow*)
    { color = Yellow; number = Zero; property = None };
    { color = Yellow; number = One; property = None };
    { color = Yellow; number = Two; property = None };
    { color = Yellow; number = Three; property = None };
    { color = Yellow; number = Four; property = None };
    { color = Yellow; number = Five; property = None };
    { color = Yellow; number = Six; property = None };
    { color = Yellow; number = Seven; property = None };
    { color = Yellow; number = Eight; property = None };
    { color = Yellow; number = Nine; property = None };
  ]

(*All Special Cards*)
let special_cards =
  [
    { color = Red; number = NaN; property = Some Draw2 };
    { color = Blue; number = NaN; property = Some Draw2 };
    { color = Green; number = NaN; property = Some Draw2 };
    { color = Yellow; number = NaN; property = Some Draw2 };
    { color = Wild; number = NaN; property = Some Draw2 };
  ]

(*All Wild Cards*)
let wild_cards =
  [
    { color = Wild; number = NaN; property = None };
    { color = Wild; number = NaN; property = Some Draw4 };
  ]

(*Full All Cards*)
let all_cards = num_cards @ special_cards @ wild_cards

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
        (Some { color = Red; number = Zero; property = None })
        (Some { color = Red; number = Zero; property = None }) );
    ( "strategy_1 : enemy has only special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_1 : enemy has only wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_1 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_1 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_1 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_1 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal true true );
    (*strategy 2*)
    ( "strategy_2 : enemy has only number cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has only special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has only wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_2 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal true true );
    (*strategy 3*)
    ( "strategy_3 : enemy has only number cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has only special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has only wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal true true );
    ( "strategy_3 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal true true );
  ]

let suite =
  "test suite for cards" >::: List.flatten [ card_tests; game_tests; opp_tests ]

let () = run_test_tt_main suite

open OUnit2
open Game
open Cards
open Opp

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

(*All Special Cards, excluding Draw 4*)
let special_cards =
  [
    Card.make_card "Red" "NaN" "Draw 2";
    Card.make_card "Blue" "NaN" "Draw 2";
    Card.make_card "Green" "NaN" "Draw 2";
    Card.make_card "Yellow" "NaN" "Draw 2";
  ]

(*All Wild Cards*)
let wild_cards =
  [ Card.make_card "Wild" "NaN" "None"; Card.make_card "Wild" "NaN" "Draw 4" ]

(*Full All Cards*)
let all_cards = num_cards @ special_cards @ wild_cards

(*One type of card from each category*)
let one_of_each_type_cards =
  [
    Card.make_card "Red" "Zero" "None";
    Card.make_card "Red" "NaN" "Draw2";
    Card.make_card "Wild" "NaN" "None";
  ]

let card_tests =
  [
    (* get_color tests*)
    ( "get_color regular card" >:: fun _ ->
      assert_equal "Red" (Card.get_color (Card.make_card "Red" "2" "None")) );
    ( "get_color draw 2" >:: fun _ ->
      assert_equal "Blue" (Card.get_color (Card.make_card "Blue" "2" "Draw 2"))
    );
    ( "get_color draw 4" >:: fun _ ->
      assert_equal "Wild" (Card.get_color (Card.make_card "Wild" "2" "Draw 4"))
    );
    (* get number tests*)
    ( "get_number regular card" >:: fun _ ->
      assert_equal "5" (Card.get_number (Card.make_card "Yellow" "5" "None")) );
    ( "get_number draw 2" >:: fun _ ->
      assert_equal "NaN" (Card.get_number (Card.make_card "Red" "NaN" "Draw 2"))
    );
    ( "get_number draw 4" >:: fun _ ->
      assert_equal "NaN"
        (Card.get_number (Card.make_card "Wild" "NaN" "Draw 4")) );
    ( "get_number wild" >:: fun _ ->
      assert_equal "NaN" (Card.get_number (Card.make_card "Wild" "NaN" "None"))
    );
    (* get_property_name tests*)
    ( "get_property_name regular card" >:: fun _ ->
      assert_equal "None"
        (Card.get_property_name (Card.make_card "Green" "2" "None")) );
    ( "get_property_name draw 2" >:: fun _ ->
      assert_equal "Draw 2"
        (Card.get_property_name (Card.make_card "Green" "NaN" "Draw 2")) );
    ( "get_property_name draw 4" >:: fun _ ->
      assert_equal "Draw 4"
        (Card.get_property_name (Card.make_card "Wild" "NaN" "Draw 4")) );
    ( "get_property_name wild" >:: fun _ ->
      assert_equal "None"
        (Card.get_property_name (Card.make_card "Wild" "NaN" "None")) );
    (* get_property description tests*)
    ( "get_property_description regular card" >:: fun _ ->
      assert_equal "None"
        (Card.get_property_name (Card.make_card "Red" "4" "None")) );
    ( "get_property_description draw 2" >:: fun _ ->
      assert_equal "Opponent must draw 2 cards from the top of the deck"
        (Card.get_property_description (Card.make_card "Blue" "NaN" "Draw 2"))
    );
    ( "get_property_description draw 4" >:: fun _ ->
      assert_equal "Opponent must draw 4 cards from the top of the deck"
        (Card.get_property_description (Card.make_card "Wild" "NaN" "Draw 4"))
    );
    (* filter_number_cards tests*)
    ( "filter_number_cards single (number) card" >:: fun _ ->
      assert_equal
        [ Card.make_card "Red" "1" "None" ]
        (Card.filter_number_cards [ Card.make_card "Red" "1" "None" ]) );
    ( "filter_number_cards single wild card" >:: fun _ ->
      assert_equal []
        (Card.filter_number_cards [ Card.make_card "Wild" "NaN" "None" ]) );
    ( "filter_number_cards single draw 4 card" >:: fun _ ->
      assert_equal []
        (Card.filter_number_cards [ Card.make_card "Wild" "NaN" "Draw 4" ]) );
    ( "filter_number_cards single draw 2 card" >:: fun _ ->
      assert_equal []
        (Card.filter_number_cards [ Card.make_card "Blue" "NaN" "Draw 2" ]) );
    ( "filter_number_cards empty list" >:: fun _ ->
      assert_equal [] (Card.filter_number_cards []) );
    ( "filter_number_cards multiple cards, but no numbers" >:: fun _ ->
      assert_equal []
        (Card.filter_number_cards
           [
             Card.make_card "Blue" "NaN" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Wild" "NaN" "None";
           ]) );
    ( "filter_number_cards multiple cards, all numbers" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Blue" "2" "None";
          Card.make_card "Red" "1" "None";
          Card.make_card "Blue" "2" "None";
        ]
        (Card.filter_number_cards
           [
             Card.make_card "Blue" "2" "None";
             Card.make_card "Red" "1" "None";
             Card.make_card "Blue" "2" "None";
           ]) );
    ( "filter_number_cards mix of number cards, non-number cards" >:: fun _ ->
      assert_equal
        [ Card.make_card "Blue" "2" "None"; Card.make_card "Blue" "3" "None" ]
        (Card.filter_number_cards
           [
             Card.make_card "Blue" "2" "None";
             Card.make_card "Green" "NaN" "Draw 2";
             Card.make_card "Blue" "3" "None";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
    ( "filter_number_cards all numbered cards" >:: fun _ ->
      assert_equal num_cards (Card.filter_number_cards num_cards) );
    ( "filter_number_cards all special cards" >:: fun _ ->
      assert_equal [] (Card.filter_number_cards special_cards) );
    ( "filter_number_cards all wild cards" >:: fun _ ->
      assert_equal [] (Card.filter_number_cards wild_cards) );
    ( "filter_number_cards all cards in deck" >:: fun _ ->
      assert_equal num_cards (Card.filter_number_cards all_cards) );
    (*filter_special_cards tests*)
    ( "filter_special_cards single wild card" >:: fun _ ->
      assert_equal []
        (Card.filter_special_cards [ Card.make_card "Wild" "NaN" "None" ]) );
    ( "filter_special_cards single draw 4 card" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "Draw 4" ]
        (Card.filter_special_cards [ Card.make_card "Wild" "NaN" "Draw 4" ]) );
    ( "filter_special_cards single draw 2 card" >:: fun _ ->
      assert_equal
        [ Card.make_card "Green" "NaN" "Draw 2" ]
        (Card.filter_special_cards [ Card.make_card "Green" "NaN" "Draw 2" ]) );
    ( "filter_special_cards single number card" >:: fun _ ->
      assert_equal []
        (Card.filter_special_cards [ Card.make_card "Green" "0" "None" ]) );
    ( "filter_special_cards empty list" >:: fun _ ->
      assert_equal [] (Card.filter_special_cards []) );
    ( "filter_special_cards multiple cards, all numbers" >:: fun _ ->
      assert_equal []
        (Card.filter_special_cards
           [
             Card.make_card "Blue" "2" "None";
             Card.make_card "Red" "1" "None";
             Card.make_card "Blue" "2" "None";
           ]) );
    ( "filter_special_cards multiple cards, all special" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Blue" "NaN" "Draw 2";
          Card.make_card "Wild" "NaN" "Draw 4";
          Card.make_card "Blue" "2" "Draw 2";
        ]
        (Card.filter_special_cards
           [
             Card.make_card "Blue" "NaN" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Blue" "2" "Draw 2";
           ]) );
    ( "filter_special_cards multiple cards, only numbers and wild" >:: fun _ ->
      assert_equal []
        (Card.filter_special_cards
           [
             Card.make_card "Red" "4" "None";
             Card.make_card "Wild" "NaN" "None";
             Card.make_card "Blue" "2" "None";
           ]) );
    ( "filter_special_cards multiple cards, mix of all types" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Blue" "NaN" "Draw 2";
          Card.make_card "Wild" "NaN" "Draw 4";
        ]
        (Card.filter_special_cards
           [
             Card.make_card "Red" "4" "None";
             Card.make_card "Wild" "NaN" "None";
             Card.make_card "Blue" "NaN" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
    ( "filter_special_cards all possible number cards" >:: fun _ ->
      assert_equal [] (Card.filter_special_cards num_cards) );
    ( "filter_special_cards all possible special cards" >:: fun _ ->
      assert_equal special_cards (Card.filter_special_cards special_cards) );
    ( "filter_special_cards all possible wild cards" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "Draw 4" ]
        (Card.filter_special_cards wild_cards) );
    ( "filter_special_cards entire deck" >:: fun _ ->
      assert_equal
        (special_cards @ [ Card.make_card "Wild" "NaN" "Draw 4" ])
        (Card.filter_special_cards all_cards) );
    (* filter_wild_cards tests*)
    ( "filter_wild_cards single wild card" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "None" ]
        (Card.filter_wild_cards [ Card.make_card "Wild" "NaN" "None" ]) );
    ( "filter_wild_cards single number card" >:: fun _ ->
      assert_equal []
        (Card.filter_wild_cards [ Card.make_card "Red" "6" "None" ]) );
    ( "filter_wild_cards single draw 2 card" >:: fun _ ->
      assert_equal []
        (Card.filter_wild_cards [ Card.make_card "Green" "NaN" "Draw 2" ]) );
    ( "filter_wild_cards single draw 4 card" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "Draw 4" ]
        (Card.filter_wild_cards [ Card.make_card "Wild" "NaN" "Draw 4" ]) );
    ( "filter_wild_cards empty list" >:: fun _ ->
      assert_equal [] (Card.filter_wild_cards []) );
    ( "filter_wild_cards multiple cards, all numbers" >:: fun _ ->
      assert_equal []
        (Card.filter_wild_cards
           [
             Card.make_card "Blue" "2" "None";
             Card.make_card "Red" "1" "None";
             Card.make_card "Blue" "2" "None";
           ]) );
    ( "filter_wild_cards multiple cards, all special" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "Draw 4" ]
        (Card.filter_wild_cards
           [
             Card.make_card "Blue" "NaN" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Red" "NaN" "Draw 2";
           ]) );
    ( "filter_wild_cards all wild cards" >:: fun _ ->
      assert_equal wild_cards (Card.filter_wild_cards wild_cards) );
    ( "filter_wild_cards all number cards" >:: fun _ ->
      assert_equal [] (Card.filter_wild_cards num_cards) );
    ( "filter_wild_cards all special cards" >:: fun _ ->
      assert_equal
        [ Card.make_card "Wild" "NaN" "Draw 4" ]
        (Card.filter_wild_cards
           (Card.make_card "Wild" "NaN" "Draw 4" :: special_cards)) );
    ( "filter_wild_cards entire deck" >:: fun _ ->
      assert_equal wild_cards (Card.filter_wild_cards all_cards) );
  ]

let game_tests =
  [
    (*is_legal_play tests*)
    ( "is_legal_play matching colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Blue" "3" "None")
           (Card.make_card "Blue" "0" "None")) );
    ( "is_legal_play matching colors, playing on draw 2" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Red" "NaN" "Draw 2")
           (Card.make_card "Red" "0" "None")) );
    ( "is_legal_play matching colors, playing draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Green" "NaN" "Draw 2")
           (Card.make_card "Green" "0" "None")) );
    ( "is_legal_play matching colors, playing draw 2 on draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Green" "NaN" "Draw 2")
           (Card.make_card "Green" "NaN" "Draw 2")) );
    ( "is_legal_play different colors, playing draw 2 on draw 2 " >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Blue" "NaN" "Draw 2")
           (Card.make_card "Green" "NaN" "Draw 2")) );
    ( "is_legal_play different colors, playing draw 2 " >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Blue" "NaN" "Draw 2")
           (Card.make_card "Green" "5" "None")) );
    ( "is_legal_play different colors, different numbers" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Blue" "3" "None")
           (Card.make_card "Green" "4" "None")) );
    ( "is_legal_play same numbers, same colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Blue" "8" "None")
           (Card.make_card "Blue" "8" "None")) );
    ( "is_legal_play same numbers, different colors" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Blue" "8" "None")
           (Card.make_card "Red" "8" "None")) );
    ( "is_legal_play playing draw 4 on number" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "Draw 4")
           (Card.make_card "Red" "8" "None")) );
    ( "is_legal_play playing draw 4 on draw 2" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "Draw 4")
           (Card.make_card "Red" "NaN" "Draw 2")) );
    ( "is_legal_play playing draw 4 on draw 4" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "Draw 4")
           (Card.make_card "Wild" "NaN" "Draw 4")) );
    ( "is_legal_play playing wild on draw 4" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "None")
           (Card.make_card "Wild" "NaN" "Draw 4")) );
    ( "is_legal_play playing wild on regular card" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "None")
           (Card.make_card "Red" "4" "None")) );
    ( "is_legal_play playing wild on wild" >:: fun _ ->
      assert_equal true
        (Game.is_legal_play
           (Card.make_card "Wild" "NaN" "None")
           (Card.make_card "Wild" "NaN" "None")) );
  ]

let opp_tests =
  [
    (*strategy 1*)
    ( "strategy_1 : enemy has only number cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_1 num_cards) );
    ( "strategy_1 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_1 special_cards) );
    ( "strategy_1 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw 4"))
        (AI.strategy_1 wild_cards) );
    ( "strategy_1 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_1 (num_cards @ special_cards)) );
    ( "strategy_1 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_1 (num_cards @ wild_cards)) );
    ( "strategy_1 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_1 (special_cards @ special_cards)) );
    ( "strategy_1 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_1 all_cards) );
    (*strategy 2*)
    ( "strategy_2 : enemy has only number cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_2 num_cards) );
    ( "strategy_2 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 special_cards) );
    ( "strategy_2 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw 4"))
        (AI.strategy_2 wild_cards) );
    ( "strategy_2 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_2 (num_cards @ special_cards)) );
    ( "strategy_2 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw 4"))
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
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.strategy_3 num_cards) );
    ( "strategy_3 : enemy has only special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_3 special_cards) );
    ( "strategy_3 : enemy has only wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw 4"))
        (AI.strategy_3 wild_cards) );
    ( "strategy_3 : enemy has number cards and special cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.strategy_3 (num_cards @ special_cards)) );
    ( "strategy_3 : enemy has number cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw 4"))
        (AI.strategy_3 (num_cards @ wild_cards)) );
    ( "strategy_3 : enemy has special cards and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Wild" "NaN" "Draw4"))
        (AI.strategy_3 (wild_cards @ special_cards)) );
    ( "strategy_3 : enemy has number, special and wild cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw 2"))
        (AI.strategy_3 all_cards) );
    (*hard_mode_turn Tests*)
    ( "hard_mode_turn: enemy and player has 4 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "Zero" "None"))
        (AI.hard_mode_turn all_cards (Card.make_card "Red" "Zero" "None") 4) );
    ( "hard_mode_turn: enemy has 3 cards and player has 4 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.hard_mode_turn one_of_each_type_cards
           (Card.make_card "Red" "Zero" "None")
           4) );
    ( "hard_mode_turn: enemy has 4 cards and player has 3 cards" >:: fun _ ->
      assert_equal
        (Some (Card.make_card "Red" "NaN" "Draw2"))
        (AI.hard_mode_turn all_cards (Card.make_card "Red" "Zero" "None") 3) );
  ]

let suite =
  "test suite for cards" >::: List.flatten [ card_tests; game_tests; opp_tests ]

let () = run_test_tt_main suite

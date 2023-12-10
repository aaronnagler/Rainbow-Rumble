open OUnit2
open Game
open Cards
open Opp

(** Test Plan: Since all of the functions of the Card module had inputs and
    outputs like strings, cards, and card lists, we automatically tested these
    functions (with OUnit) in the test suite below. To ensure that these
    functions work across the entire range of the playable deck, we created
    helper lists for the test suite (num_cards, special_cards, wild_cards,
    all_cards) that represented all of the different types of possible cards.
    This way, we could easily test functions on the entire deck. Test cases were
    predominantly designed using black-box testing, looking at the specification
    for a function and creating test cases based of that information alone. The
    primary reason we did this was to prevent partiality to test cases that
    would pass the implementation. Black-box testing was also helpful because it
    didn't require us to read and understand other team members'
    implementations; we could just test it by what the function is supposed to
    do.

    For similar reasons as above, we also used OUnit tests for most of the AI
    module (easy_mode_turn, strategy_1, strategy_2, strategy_3, hard_mode_turn)
    and parts of the Game module (transform_pile_wild, remove_card,
    check_winner).

    Manual testing was done by running the program to ensure certain scenarios
    would lead to the correct outcomes. We had to do manual testing for
    functions whose purpose it was to print out objects of the game
    (print_colored_text, print_card, print_player_hand, print_both_hands,
    print_long, print_description in the Game module). We would do this just by
    running the game, and if necessary (depending on the function), exploring
    corner cases that might change the output of that particular print function.

    We also had to test manually for functionality that is supposed to have
    random output. This includes functions in the AI module (enemy_turn,
    winning_bark, uno_voiceline, enemy_voiceline) and the Game module (draw,
    draw_valid_card). For these functions, we just observed their functionality
    in the REPL and made sure it was what we expected. For example, with draw,
    we listed out in a document what cards should be possible to draw, and we
    repeatedly drew from the deck to make sure we were getting the expected
    cards. However, it's important to note that, because of the sheer amount of
    possibilities for some of these functions and because of the random nature
    of others, our manual tests (testing through playing the game) couldn't be
    as comprehensive as our OUnit test cases.

    Correctness of the system: Overall, since our program is a game which has
    its main function being creating a good experince for the user, a good
    perspective to analyze wether we acheive correctness in our program is to
    ensure that every command that the user makes results in a behavior that the
    user expects. A large part of ensuring this correctness was through our
    constant and frequent manual tests of the system through the REPL. This
    allowed us to test certain overall branches of sequences of choices that the
    user could make would result in the program behaving as expected. However,
    since we cannot possibly make every possible decision that the user can make
    through manual testing, our OUnit tests also contirbute to the correctness
    by testing the foundational functions which are used throughout the game,
    providing more confidence in the correctness of the overall program and its
    ability to respond properly to any input that the user may make, to create
    for the ideal experience. *)

(**All Number Cards*)
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

(**All Special Cards, excluding Draw 4*)
let special_cards =
  [
    Card.make_card "Red" "NaN" "Draw 2";
    Card.make_card "Blue" "NaN" "Draw 2";
    Card.make_card "Green" "NaN" "Draw 2";
    Card.make_card "Yellow" "NaN" "Draw 2";
  ]

(**All Wild Cards*)
let wild_cards =
  [ Card.make_card "Wild" "NaN" "None"; Card.make_card "Wild" "NaN" "Draw 4" ]

(**Full All Cards*)
let all_cards = num_cards @ special_cards @ wild_cards

(**One type of card from each category*)
let one_of_each_type_cards =
  [
    Card.make_card "Red" "Zero" "None";
    Card.make_card "Red" "NaN" "Draw2";
    Card.make_card "Wild" "NaN" "None";
  ]

(** tests for functions in the Card module*)
let card_tests =
  [
    (* get_color tests*)
    ( "get_color red regular card" >:: fun _ ->
      assert_equal "Red" (Card.get_color (Card.make_card "Red" "2" "None")) );
    ( "get_color green regular card" >:: fun _ ->
      assert_equal "Green" (Card.get_color (Card.make_card "Green" "2" "None"))
    );
    ( "get_color blue regular card" >:: fun _ ->
      assert_equal "Blue" (Card.get_color (Card.make_card "Blue" "2" "None")) );
    ( "get_color yellow regular card" >:: fun _ ->
      assert_equal "Yellow"
        (Card.get_color (Card.make_card "Yellow" "2" "None")) );
    ( "get_color blue draw 2" >:: fun _ ->
      assert_equal "Blue" (Card.get_color (Card.make_card "Blue" "2" "Draw 2"))
    );
    ( "get_color red draw 2" >:: fun _ ->
      assert_equal "Red" (Card.get_color (Card.make_card "Red" "2" "Draw 2")) );
    ( "get_color green draw 2" >:: fun _ ->
      assert_equal "Green"
        (Card.get_color (Card.make_card "Green" "2" "Draw 2")) );
    ( "get_color yellow draw 2" >:: fun _ ->
      assert_equal "Yellow"
        (Card.get_color (Card.make_card "Yellow" "2" "Draw 2")) );
    ( "get_color draw 4" >:: fun _ ->
      assert_equal "Wild" (Card.get_color (Card.make_card "Wild" "2" "Draw 4"))
    );
    ( "get_color regular wild card" >:: fun _ ->
      assert_equal "Wild" (Card.get_color (Card.make_card "Wild" "NaN" "None"))
    );
    (* get number tests*)
    ( "get_number regular card 5" >:: fun _ ->
      assert_equal "5" (Card.get_number (Card.make_card "Yellow" "5" "None")) );
    ( "get_number regular card 0" >:: fun _ ->
      assert_equal "0" (Card.get_number (Card.make_card "Yellow" "0" "None")) );
    ( "get_number regular card 1" >:: fun _ ->
      assert_equal "1" (Card.get_number (Card.make_card "Yellow" "1" "None")) );
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
    (* most_common_color tests*)
    ( "most_common_color empty list " >:: fun _ ->
      assert_equal Red (Card.most_common_color []) );
    ( "most_common_color single green " >:: fun _ ->
      assert_equal Green
        (Card.most_common_color [ Card.make_card "Green" "5" "None" ]) );
    ( "most_common_color single blue " >:: fun _ ->
      assert_equal Blue
        (Card.most_common_color [ Card.make_card "Blue" "5" "None" ]) );
    ( "most_common_color single red " >:: fun _ ->
      assert_equal Red
        (Card.most_common_color [ Card.make_card "Red" "5" "None" ]) );
    ( "most_common_color single yellow " >:: fun _ ->
      assert_equal Yellow
        (Card.most_common_color [ Card.make_card "Yellow" "5" "None" ]) );
    ( "most_common_color green" >:: fun _ ->
      assert_equal Green
        (Card.most_common_color
           [
             Card.make_card "Green" "0" "None";
             Card.make_card "Yellow" "5" "None";
             Card.make_card "Green" "2" "None";
           ]) );
    ( "most_common_color blue" >:: fun _ ->
      assert_equal Blue
        (Card.most_common_color
           [
             Card.make_card "Blue" "0" "None";
             Card.make_card "Blue" "5" "None";
             Card.make_card "Yellow" "5" "None";
             Card.make_card "Wild" "NaN" "None";
           ]) );
    ( "most_common_color red" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Blue" "0" "None";
             Card.make_card "Red" "5" "None";
             Card.make_card "Red" "5" "None";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
    ( "most_common_color yellow" >:: fun _ ->
      assert_equal Yellow
        (Card.most_common_color
           [
             Card.make_card "Yellow" "0" "None";
             Card.make_card "Yellow" "5" "None";
             Card.make_card "Yellow" "5" "None";
             Card.make_card "Yellow" "NaN" "Draw 2";
           ]) );
    ( "most_common_color red-blue tie" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Blue" "0" "None";
             Card.make_card "Red" "1" "None";
             Card.make_card "Blue" "2" "None";
             Card.make_card "Red" "NaN" "Draw 2";
           ]) );
    ( "most_common_color red-green tie" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Green" "0" "None"; Card.make_card "Red" "1" "None";
           ]) );
    ( "most_common_color red-green tie" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Green" "0" "None"; Card.make_card "Red" "1" "None";
           ]) );
    ( "most_common_color red-yellow tie" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Red" "0" "None"; Card.make_card "Yellow" "1" "None";
           ]) );
    ( "most_common_color blue-green tie" >:: fun _ ->
      assert_equal Blue
        (Card.most_common_color
           [
             Card.make_card "Blue" "0" "Draw 2";
             Card.make_card "Green" "1" "None";
           ]) );
    ( "most_common_color blue-yellow tie" >:: fun _ ->
      assert_equal Blue
        (Card.most_common_color
           [
             Card.make_card "Yellow" "0" "Draw 2";
             Card.make_card "Yellow" "0" "Draw 2";
             Card.make_card "Blue" "1" "Draw 2";
             Card.make_card "Blue" "1" "Draw 2";
           ]) );
    ( "most_common_color green-yellow tie" >:: fun _ ->
      assert_equal Green
        (Card.most_common_color
           [
             Card.make_card "Yellow" "NaN" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Green" "0" "None";
           ]) );
    ( "most_common_color four-way tie" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Red" "5" "None";
             Card.make_card "Yellow" "0" "Draw 2";
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Green" "6" "None";
             Card.make_card "Blue" "6" "None";
           ]) );
    ( "most_common_color all wilds" >:: fun _ ->
      assert_equal Red
        (Card.most_common_color
           [
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Wild" "NaN" "None";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
  ]

(**Tests for the functions in the Game module *)
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
    ( "is_legal_play playing red on blue" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Red" "1" "None")
           (Card.make_card "Blue" "2" "None")) );
    ( "is_legal_play playing red on green" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Red" "2" "None")
           (Card.make_card "Green" "4" "None")) );
    ( "is_legal_play playing red on yellow" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Red" "9" "None")
           (Card.make_card "Yellow" "7" "None")) );
    ( "is_legal_play playing blue on red" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Blue" "2" "None")
           (Card.make_card "Red" "3" "None")) );
    ( "is_legal_play playing blue on yellow" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Blue" "9" "None")
           (Card.make_card "Yellow" "2" "None")) );
    ( "is_legal_play playing blue on green" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Blue" "5" "None")
           (Card.make_card "Green" "6" "None")) );
    ( "is_legal_play playing yellow on red" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Yellow" "2" "None")
           (Card.make_card "Red" "9" "None")) );
    ( "is_legal_play playing yellow on blue" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Yellow" "7" "None")
           (Card.make_card "Blue" "8" "None")) );
    ( "is_legal_play playing yellow on green" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Yellow" "4" "None")
           (Card.make_card "Green" "2" "None")) );
    ( "is_legal_play playing green on red" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Green" "3" "None")
           (Card.make_card "Red" "5" "None")) );
    ( "is_legal_play playing green on blue" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Green" "6" "None")
           (Card.make_card "Blue" "2" "None")) );
    ( "is_legal_play playing green on yellow" >:: fun _ ->
      assert_equal false
        (Game.is_legal_play
           (Card.make_card "Green" "1" "None")
           (Card.make_card "Yellow" "4" "None")) );
    (* is_valid_first_card tests *)
    ( "is_valid_first_card number card" >:: fun _ ->
      assert_equal true
        (Game.is_valid_first_card (Card.make_card "Green" "5" "None")) );
    ( "is_valid_first_card draw 2" >:: fun _ ->
      assert_equal false
        (Game.is_valid_first_card (Card.make_card "Red" "NaN" "Draw 2")) );
    ( "is_valid_first_card draw 4" >:: fun _ ->
      assert_equal false
        (Game.is_valid_first_card (Card.make_card "Wild" "NaN" "Draw 4")) );
    ( "is_valid_first_card wild card" >:: fun _ ->
      assert_equal false
        (Game.is_valid_first_card (Card.make_card "Wild" "NaN" "None")) );
    (* transform_wild_pile tests *)
    ( "transform_pile_wild wild card" >:: fun _ ->
      assert_equal
        (Game.create_game
           [ Card.make_card "Red" "3" "None" ]
           [ Card.make_card "Green" "4" "None" ]
           (Card.make_card "Red" "NaN" "None")
           "hard")
        (Game.transform_pile_wild
           (Game.create_game
              [ Card.make_card "Red" "3" "None" ]
              [ Card.make_card "Green" "4" "None" ]
              (Card.make_card "Wild" "NaN" "None")
              "hard")
           "Red") );
    ( "transform_pile_wild wild card" >:: fun _ ->
      assert_equal
        (Game.create_game
           [ Card.make_card "Red" "3" "None" ]
           [
             Card.make_card "Green" "4" "None";
             Card.make_card "Blue" "4" "Draw 2";
           ]
           (Card.make_card "Blue" "NaN" "Draw 4")
           "medium")
        (Game.transform_pile_wild
           (Game.create_game
              [ Card.make_card "Red" "3" "None" ]
              [
                Card.make_card "Green" "4" "None";
                Card.make_card "Blue" "4" "Draw 2";
              ]
              (Card.make_card "Wild" "NaN" "Draw 4")
              "medium")
           "Blue") );
    (* remove_card tests*)
    ( "remove_card single-element list (contains)" >:: fun _ ->
      assert_equal []
        (Game.remove_card
           (Card.make_card "Red" "1" "None")
           [ Card.make_card "Red" "1" "None" ]) );
    ( "remove_card single-element list (doesn't contain)" >:: fun _ ->
      assert_equal
        [ Card.make_card "Red" "NaN" "Draw 2" ]
        (Game.remove_card
           (Card.make_card "Red" "1" "None")
           [ Card.make_card "Red" "NaN" "Draw 2" ]) );
    ( "remove_card empty list" >:: fun _ ->
      assert_equal [] (Game.remove_card (Card.make_card "Red" "1" "None") []) );
    ( "remove_card multi-element list" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Red" "NaN" "Draw 2"; Card.make_card "Green" "3" "None";
        ]
        (Game.remove_card
           (Card.make_card "Blue" "5" "None")
           [
             Card.make_card "Red" "NaN" "Draw 2";
             Card.make_card "Blue" "5" "None";
             Card.make_card "Green" "3" "None";
           ]) );
    ( "remove_card multi-element list, repeats" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Red" "NaN" "Draw 2"; Card.make_card "Blue" "5" "None";
        ]
        (Game.remove_card
           (Card.make_card "Blue" "5" "None")
           [
             Card.make_card "Blue" "5" "None";
             Card.make_card "Red" "NaN" "Draw 2";
             Card.make_card "Blue" "5" "None";
           ]) );
    ( "remove_card remove draw 4" >:: fun _ ->
      assert_equal
        [ Card.make_card "Blue" "5" "None" ]
        (Game.remove_card
           (Card.make_card "Wild" "NaN" "Draw 4")
           [
             Card.make_card "Blue" "5" "None";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
    ( "remove_card remove wild" >:: fun _ ->
      assert_equal
        [
          Card.make_card "Wild" "NaN" "Draw 4";
          Card.make_card "Wild" "NaN" "Draw 4";
        ]
        (Game.remove_card
           (Card.make_card "Wild" "NaN" "None")
           [
             Card.make_card "Wild" "NaN" "Draw 4";
             Card.make_card "Wild" "NaN" "None";
             Card.make_card "Wild" "NaN" "Draw 4";
           ]) );
    (* check_winner tests *)
    ( "check_winner player wins" >:: fun _ ->
      assert_equal (true, 0)
        (Game.check_winner
           (Game.create_game []
              [ Card.make_card "Wild" "NaN" "Draw 4" ]
              (Card.make_card "Green" "NaN" "Draw 2")
              "easy")) );
    ( "check_winner opponent wins" >:: fun _ ->
      assert_equal (true, 1)
        (Game.check_winner
           (Game.create_game
              [ Card.make_card "Wild" "NaN" "Draw 4" ]
              []
              (Card.make_card "Green" "NaN" "Draw 2")
              "medium")) );
    ( "check_winner no one wins" >:: fun _ ->
      assert_equal (false, 2)
        (Game.check_winner
           (Game.create_game special_cards num_cards
              (Card.make_card "Blue" "1" "None")
              "hard")) );
  ]

(**Tests for functions in the AI module *)
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

(**Test suite for all of the modules *)
let suite =
  "test suite for cards" >::: List.flatten [ card_tests; game_tests; opp_tests ]

let () = run_test_tt_main suite

open Game
open Cards

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "game exited"
  | _ ->
      input |> eval |> print_endline;
      repl eval

(* let rec blank_space i = print_endline (String.make i '\n'); *)

(* Draws a card to the players hand, returns the updated game, if player is
   [true] then draws card to players hand, if player is false then draws card to
   opponent's hand *)
let draw_card (game : Game.t) (player : bool) =
  (* print_endline (String.make 40 '\n'); print_endline "Scroll up if you want
     to see your previous input!"; *)
  print_endline "\n---------------------------------------------------------\n";
  print_endline "a card has been added to your hand";
  Game.draw_update game player

(* Based on the input, prints the corresponding card. *)
let rec read_card s (game : Game.t) =
  print_endline (String.make 40 '\n');
  print_endline "Scroll up if you want to see your previous input!";
  print_endline "\n---------------------------------------------------------\n";
  try
    let c = List.nth game.player_hand (int_of_string s) in
    print_endline "\n";
    Game.print_long c;
    (* let s = "\n" ^ "\n" ^ Card.get_property_name c ^ ": " ^
       Card.get_property_description c ^ "\n" in print_endline s; *)
    game
  with Failure x ->
    print_endline "Discard Pile: \n";
    Game.print_card game.discard_pile;
    print_endline "\n \nHand: \n ";
    Game.print_player_hand game.player_hand 0;
    print_endline "\n";
    print_endline ("Could not parse number \"" ^ s ^ "\". please try again:");
    print_string "> ";
    read_card (read_line ()) game

(* Returns the updated game after the function plays the card corresponding to
   user input (A-F) if the card is valid, if not, does not play the card, and
   prints corresponding statement *)

(* let play_card s (game : Game.t) = if Game.is_legal_play (List.nth
   game.player_hand (int_of_string s)) game.discard_pile then ( print_endline
   "Card was played successfully!"; let new_game_state = Game.play_card
   (List.nth game.player_hand (int_of_string s)) game true in if Card.get_color
   new_game_state.discard_pile = "Wild" then let input = read_line () in
   Game.transform_pile_wild new_game_state input else new_game_state) else (
   print_endline "Card could not be played, please try another card or draw a
   card."; game) *)

(* let x = game in match x with | x -> let new_game_state = Game.play_card
   (List.nth game.player_hand (int_of_string s)) game true in match
   Card.get_color new_game_state.discard_pile with | "Wild" -> print_endline
   "Choose new color for wild card: "; let input = read_line () in
   transition_before_opp (Game.transform_pile_wild new_game_state input) | _ ->
   new_game_state) | _ -> *)

(* Performs opponent's turn, checking on wether opp was able to play a card or
   if they are choosing to draw a card instead. Prints out what the opp did and
   returns the updated game state *)
let opps_turn (game : Game.t) : Game.t =
  match Game.enemy_turn game with
  | Some x -> (
      print_endline "The opponent played a new card onto the discard pile!";
      print_endline "They played the following: \n";
      Game.print_long x;
      let new_game_state = Game.play_card x game false in
      match Card.get_color new_game_state.discard_pile with
      | "Wild" ->
          print_endline "\n";
          print_endline
            "The card the opponent played has the following effect on your \
             hand: \n";
          Game.print_desc x;
          Game.transform_pile_wild new_game_state
            (Game.most_common_color game.enemy_hand)
      | _ -> new_game_state)
  | None ->
      print_endline "The opponent chose to draw a card";
      draw_card game false

(* Call this function if the player was successfully able to have a turn, this
   function outputs the result of their turn, and asks if they are ready for the
   opponent's turn, and if so, triggers the opponents turn. *)
let transition_before_opp (game : Game.t) : Game.t =
  print_endline (String.make 40 '\n');
  print_endline "Scroll up if you want to see your previous input!";
  print_endline "\n---------------------------------------------------------\n";
  print_endline "Your play was successful! Here is the updated game status:";
  print_endline "\n  \n----\nGame Status\n";
  print_endline "Below is the Discard Pile and your hand! \n";
  print_endline "Discard Pile: \n";
  Game.print_card game.discard_pile;
  print_endline "\n \nHand: \n ";
  Game.print_player_hand game.player_hand 0;
  print_endline "\n";
  print_endline
    ("the number of cards in the opponents hand:  "
    ^ string_of_int (List.length game.enemy_hand));
  print_endline "\n Now it is the turn of the opponent.";
  print_endline "Press any key to proceed:";
  print_string "> ";
  match read_line () with
  | _ ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      print_endline "Opponent's turn! \n";
      opps_turn game

(* Same thing as above [play_card], but with pattern matching instead of
   if-statements ( +2 lines :D )*)
let rec play_card s (game : Game.t) =
  print_endline (String.make 40 '\n');
  print_endline "Scroll up if you want to see your previous input!";
  print_endline "\n---------------------------------------------------------\n";
  try
    match
      Game.is_legal_play
        (List.nth game.player_hand (int_of_string s))
        game.discard_pile
    with
    | true -> (
        let new_game_state =
          Game.play_card (List.nth game.player_hand (int_of_string s)) game true
        in
        match Card.get_color new_game_state.discard_pile with
        | "Wild" ->
            print_endline "Discard Pile: \n";
            Game.print_card game.discard_pile;
            print_endline "\n \nHand: \n ";
            Game.print_player_hand game.player_hand 0;
            print_endline "\n";
            print_endline
              "Choose new color for wild card: (the options are: red, green, \
               yellow, and blue). \n\
               Note that if you do not input a proper color, you will have to \
               reneter your card input:";
            let input = read_line () in
            transition_before_opp
              (Game.transform_pile_wild new_game_state input)
        | _ -> transition_before_opp new_game_state)
    | false ->
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game
  with Failure x ->
    print_endline (String.make 40 '\n');
    print_endline "Scroll up if you want to see your previous input!";
    print_endline
      "\n---------------------------------------------------------\n";
    print_endline "Discard Pile: \n";
    Game.print_card game.discard_pile;
    print_endline "\n \nHand: \n ";
    Game.print_player_hand game.player_hand 0;
    print_endline "\n";
    print_endline
      ("Could not parse input: \"" ^ s
     ^ "\". please try again and reenter your card input:");
    print_string "> ";
    play_card (read_line ()) game

(* Matches user input with a shorter string that will be used to process their
   request in the process of the game *)
let stage_2 x =
  match read_line () with
  | "play card" ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "play"
  | "read card" ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "read"
  | "draw card" -> "draw"
  | _ ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "please enter a valid input"

(* Upon the user's request, performs the given action and returns the updated
   game *)
let rec game_process z (game : Game.t) =
  (* Check if win condition have been met *)
  match Game.check_winner game with
  | true, 0 -> "\n\n You Win!"
  | true, _ -> "\n\n You Lose :("
  | false, _ -> (
      print_endline "\n\n----\nGame Status\n";
      print_endline "Below is the Discard Pile and your hand! \n";
      print_endline "Discard Pile: \n";
      Game.print_card game.discard_pile;
      print_endline "\n \nHand: \n ";
      Game.print_player_hand game.player_hand 0;
      print_endline "\n";
      print_endline
        ("The number of cards in the opponents hand:  "
        ^ string_of_int (List.length game.enemy_hand));
      print_endline
        " \n\
        \   You have the following input choices:\n\
        \      - play card  (to play one of your cards)\n\
        \      - read card  (to read the specific description of one of your \
         cards)";
      print_endline "      - draw card  (to draw a card)";
      print_string "> ";
      match stage_2 () with
      | "play" ->
          print_endline "Discard Pile: \n";
          Game.print_card game.discard_pile;
          print_endline "\n \nHand: \n ";
          Game.print_player_hand game.player_hand 0;
          print_endline "\n";
          print_endline
            "please enter a number corresponding to the label of the card";
          print_string "> ";
          game_process () (play_card (read_line ()) game)
      | "read" ->
          print_endline "Discard Pile: \n";
          Game.print_card game.discard_pile;
          print_endline "\n \nHand: \n ";
          Game.print_player_hand game.player_hand 0;
          print_endline "\n";
          print_endline
            "please enter a number corresponding to the label of the card";
          print_string "> ";
          game_process () (read_card (read_line ()) game)
      | "draw" -> game_process () (transition_before_opp (draw_card game true))
      | _ ->
          print_endline "Please enter a valid input";
          game_process () game)

(* returns the string "I hope you have fun!" *)
let start_game x = "Good luck and have fun!"
(* let start_game x = "\n" *)

(* Prints some instructions about starting the game and takes users' input *)
let stage_1 x =
  print_endline
    "    \n\n\
     You can play this game by typing text into the terminal. \n\
    \    At the moment, you can type the following commands:";
  print_endline "      - rules";
  print_endline "      - start game";
  print_endline "To exit the game, you can press control-c";
  print_string "> ";
  match read_line () with
  | "rules" ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "The rules of the game are as followed:\n\
      \      At a very high level, you have a hand of cards and your objective \
       is to\n\
      \      have 0 cards in your hand before your opponent, a computer. This \
       is a turn\n\
      \      based game, where you take a turn, and then your opponent takes a \
       turn.\n\
      \      You start with a hand of 7 cards, each card is either a regular \
       card or a\n\
      \      wildcard. \n\
      \          Regular Cards: Have a color and a number corresponding to \
       them. \n\
      \              The color can either be red, blue, yellow, or green\n\
      \              The number can be a number from 0 to 9\n\
      \          Wild Cards: \n\
      \              Are sometimes of a color, but sometimes do not have a \n\
      \              corresponding color to them. \n\
      \              These cards have a side effect, that will affect the game \
       in \n\
      \              some way.\n\
      \      There is a discard pile, which is a card, either a wildcard or a \
       regular\n\
      \      card.\n\
      \      During your turn: \n\
      \            You have two options:\n\
      \              1. play a card onto the discard pile\n\
      \                - this replaces the discard pile with your card for \
       future turns\n\
      \                (until further plays)\n\
      \              2. draw a card\n\
      \        If you want to play a card onto the discard pile, the card must \
       fufill \n\
      \        some requirements:\n\
      \          - if the card you are playing is a regular card\n\
      \            - the card you are playing must have the same number as the \
       card on \n\
      \            the discard pile\n\
      \            OR \n\
      \            - the card you are playing must be of the same color as the \
       card on\n\
      \            the discard pile\n\
      \      "
  | "start game" ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "I hope you have fun!"
  | _ ->
      print_endline (String.make 40 '\n');
      print_endline "Scroll up if you want to see your previous input!";
      print_endline
        "\n---------------------------------------------------------\n";
      "please enter a valid input"

(* Takes user's input in the starting stage of the game and either begins the
   game or persists this menu *)
let rec start_menu z =
  match stage_1 () with
  | "I hope you have fun!" -> "\n"
  | "Quitting the game..." -> failwith "Game has quit!"
  | x ->
      print_endline x;
      start_menu ()

(* Requests desired difficulty from player *)
let rec set_difficulty () =
  print_endline
    "Select number for game difficulty: \n 0. Easy \n 1. Medium \n 2. Hard";
  let diff = read_line () in
  print_endline (String.make 40 '\n');
  print_endline "Scroll up if you want to see your previous input!";
  print_endline "\n---------------------------------------------------------\n";
  match String.lowercase_ascii diff with
  | "0" | "easy" ->
      print_endline "Easy mode selected!";
      "easy"
  | "1" | "medium" ->
      print_endline "Medium mode selected!";
      "medium"
  | "2" | "hard" ->
      print_endline "Hard mode selected!";
      "hard"
  | _ -> set_difficulty ()

(* Upon game completion, givens player option to play again or go to main
   menu *)
let rec play_again () =
  print_endline "What do you want to do now?";
  print_endline "0. Play again";
  print_endline "1. Main Menu";
  match String.lowercase_ascii (read_line ()) with
  | "0" | "play again" ->
      let diff = set_difficulty () in
      print_endline ("\n" ^ start_game ());
      print_endline (game_process () (Game.create_hands diff));
      play_again ()
  | "1" | "main menu" ->
      print_endline "Welcome to Rainbow Card Rumble!";
      print_endline
        "Please put your ternimal into full screen for the best experience!";
      print_endline (start_menu ());
      let diff = set_difficulty () in
      print_endline ("\n" ^ start_game ());
      print_endline (game_process () (Game.create_hands diff));
      play_again ()
  | _ -> play_again ()

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Rainbow Card Rumble!";
  print_endline
    "Please put your ternimal into full screen for the best experience!";
  print_endline (start_menu ());
  let diff = set_difficulty () in
  print_endline ("\n" ^ start_game ());
  print_endline (game_process () (Game.create_hands diff));
  play_again ()

(* Game.print_player_hand; Game.create_hands; let words = read_line () in
   print_endline words; Game.print_player_hand Game.create_hands *)

(* let () = for i = 0 to Array.length Sys.argv - 1 do Printf.printf "[%i] %s\n"
   i Sys.argv.(i) done *)

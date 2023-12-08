open Game

(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "game exited"
  | _ ->
      input |> eval |> print_endline;
      repl eval

(* Based on the input (A-G) prints the corresponding card. *)
let read_card s (game : Game.t) =
  let c = List.nth game.player_hand (int_of_string s) in
  print_endline "\n";
  Game.print_card c;
  let s =
    "\n" ^ "\n" ^ Card.get_property_name c ^ ": "
    ^ Card.get_property_description c
    ^ "\n"
  in
  print_endline s;
  game

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

(* Same thing as above [play_card], but with pattern matching instead of
   if-statements ( +2 lines :D )*)
let play_card s (game : Game.t) =
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
          print_endline "Choose new color for wild card: ";
          let input = read_line () in
          Game.transform_pile_wild new_game_state input
      | _ -> new_game_state)
  | false ->
      print_endline
        "Card could not be played, please try another card or draw a card.";
      game

(* Draws a card to the players hand, returns the updated game *)
let draw_card (game : Game.t) =
  print_endline "a card has been added to your hand";
  Game.draw_update game true

(* Matches user input with a shorter string that will be used to process their
   request in the process of the game *)
let stage_2 x =
  match read_line () with
  | "play card" -> "play"
  | "read card" -> "read"
  | "draw card" -> "draw"
  | _ -> "please enter a valid input"

(* Upon the user's request, performs the given action and returns the updated
   game *)
let rec game_process z (game : Game.t) =
  (* Check if win condition have been met *)
  print_endline "\n  \nGame Status\n";
  print_endline "    Below is the Discard Pile and your hand! \n";
  print_endline "        Discard Pile: \n";
  Game.print_card game.discard_pile;
  print_endline "\n \n        Hand: \n ";
  Game.print_player_hand game.player_hand 0;
  print_endline "\n";
  print_endline
    "    You have the following input choices:\n\
    \      - play card  (to play one of your cards)\n\
    \      - read card  (to read the specific description of one of your cards)";
  print_endline "      - draw card  (to draw a card)";
  print_string "> ";
  match stage_2 () with
  | "play" ->
      print_endline
        "please enter a number corresponding to the label of the card";
      print_string "> ";
      game_process () (play_card (read_line ()) game)
  | "read" ->
      print_endline
        "please enter a number corresponding to the label of the card";
      print_string "> ";
      game_process () (read_card (read_line ()) game)
  | "draw" -> game_process () (draw_card game)
  | x ->
      print_endline x;
      game_process () (Game.enemy_turn game)

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
  | "start game" -> "I hope you have fun!"
  | _ -> "please enter a valid input"

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
let set_difficulty () =
  print_endline "Select game difficulty: \n Easy \n Medium \n Hard";
  let diff = read_line () in
  String.lowercase_ascii diff

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Rainbow Card Rumble!";
  print_endline
    "Please put your ternimal into full screen for the best experience!";
  print_endline (start_menu ());
  let diff = set_difficulty () in
  print_endline ("\n" ^ start_game ());
  print_endline (game_process () (Game.create_hands diff))

(* Game.print_player_hand; Game.create_hands; let words = read_line () in
   print_endline words; Game.print_player_hand Game.create_hands *)

(* let () = for i = 0 to Array.length Sys.argv - 1 do Printf.printf "[%i] %s\n"
   i Sys.argv.(i) done *)

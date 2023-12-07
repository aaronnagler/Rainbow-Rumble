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
  match s with
  | "A" ->
      Game.print_card (List.nth game.player_hand 1);
      game
  | "B" ->
      Game.print_card (List.nth game.player_hand 2);
      game
  | "C" ->
      Game.print_card (List.nth game.player_hand 3);
      game
  | "D" ->
      Game.print_card (List.nth game.player_hand 4);
      game
  | "E" ->
      Game.print_card (List.nth game.player_hand 5);
      game
  | "F" ->
      Game.print_card (List.nth game.player_hand 6);
      game
  | "G" ->
      Game.print_card (List.nth game.player_hand 7);
      game
  | _ ->
      print_endline "please enter a proper card label";
      game

(* Returns the updated game after the function plays the card corresponding to
   user input (A-F) if the card is valid, if not, does not play the card, and
   prints corresponding statement *)
let play_card s (game : Game.t) =
  match s with
  | "A" ->
      if Game.is_legal_play (List.nth game.player_hand 1) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 1) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "B" ->
      if Game.is_legal_play (List.nth game.player_hand 2) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 2) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "C" ->
      if Game.is_legal_play (List.nth game.player_hand 3) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 3) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "D" ->
      if Game.is_legal_play (List.nth game.player_hand 4) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 4) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "E" ->
      if Game.is_legal_play (List.nth game.player_hand 5) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 5) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "F" ->
      if Game.is_legal_play (List.nth game.player_hand 6) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 6) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | "G" ->
      if Game.is_legal_play (List.nth game.player_hand 7) game.discard_pile then (
        print_endline "Card was played successfully!";
        Game.play_card (List.nth game.player_hand 7) game true)
      else (
        print_endline
          "Card could not be played, please try another card or draw a card.";
        game)
  | _ ->
      print_endline "please enter a proper card label";
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
  print_endline "\n  \nGame Status\n";
  print_endline "    Below is the Discard Pile and your hand! \n";
  print_endline "        Discard Pile: \n";
  Game.print_card game.discard_pile;
  print_endline "\n \n        Hand: \n ";
  Game.print_player_hand game;
  print_endline "\n";
  print_endline
    "    You have the following input choices:\n\
    \      - play card  (to play one of your cards)\n\
    \      - read card  (to read the specific description of one of your cards)";
  print_endline "      - draw card  (to draw a card)";
  print_string "> ";
  match stage_2 () with
  | "play" ->
      print_endline "select a card A-G";
      print_string "> ";
      game_process () (play_card (read_line ()) game)
  | "read" ->
      print_endline "select a card A-G";
      print_string "> ";
      game_process () (read_card (read_line ()) game)
  | "draw" -> game_process () (draw_card game)
  | x ->
      print_endline x;
      game_process () (Game.enemy_turn game)

(* returns the string "I hope you have fun!" *)
let start_game x = "I hope you have fun!"

(* Prints some instructions about starting the game and takes users' input *)
let stage_1 x =
  print_endline
    "    \n\n\
     You can play this game by typing text into the terminal. \n\
    \    At the moment, you can type the following commands:";
  print_endline "      - rules";
  print_endline "      - start game";
  print_endline "      - quit";
  print_endline
    "To exit the game, you can press enter without entering any command";
  print_string "> ";
  match read_line () with
  | "rules" ->
      "\n\
      \ The rules of the game are as followed:\n\
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
      \            the discard pile. If the card on the discard pile is a wild\n\
      \                    card, then it is every color and so your card can \
       always be \n\
      \                    played. \n\
      \                - if the card is a wild card:\n\
      \                    - you can play it onto any card"
  | "start game" -> "I hope you have fun!"
  | "quit" -> "Quitting the game..."
  | _ -> "please enter a valid input"

(* Takes user's input in the starting stage of the game and either begins the
   game or persists this menu *)
let rec start_menu z =
  match stage_1 () with
  | "I hope you have fun!" -> start_game ()
  | "Quitting the game..." -> failwith "Game has quit!"
  | x ->
      print_endline x;
      start_menu ()

(*********** command line interface ***********)
let () =
  print_endline "Welcome to Rainbow Card Rumble!";
  print_endline (start_menu ());
  print_endline (game_process () Game.create_hands)

(* Game.print_player_hand; Game.create_hands; let words = read_line () in
   print_endline words; Game.print_player_hand Game.create_hands *)

(* let () = for i = 0 to Array.length Sys.argv - 1 do Printf.printf "[%i] %s\n"
   i Sys.argv.(i) done *)

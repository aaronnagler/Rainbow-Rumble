open Cards
open Opp

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    difficulty : string;
    mutable discard_pile : Card.t;
  }

  (* Prints the color and number of [card]. Example output: "[Yellow 5]"*)
  let print_card (card : Card.t) : unit =
    let num = Card.get_number card in
    let col = Card.get_color card in
    let prop = Card.get_property_name card in
    print_string ("[" ^ col ^ " " ^ num ^ " " ^ prop ^ "]")

  (* Prints the player's hand. *)
  let print_player_hand (game : t) : unit =
    print_string "Player's hand:";
    List.iter print_card game.player_hand

  (* Prints the hands of the player_hand and the enemy hand. *)
  let print_both_hands (game : t) : unit =
    print_string "Player's hand:";
    List.iter print_card game.player_hand;
    print_string "\nEnemy's hand:";
    List.iter print_card game.player_hand

  (* Returns the hand with n random cards added to the hand. *)
  let rec draw (hand : Card.t list) (n : int) : Card.t list =
    let local_rng = Random.State.make_self_init () in
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card local_rng in
      draw (hand @ [ random_card ]) (n - 1)

  (* Returns true if the given card is a valid card to initialize the
     discard_pile. *)
  let is_valid_first_card (card : Card.t) : bool =
    Card.get_color card <> "Wild"
    && Card.get_number card <> "NaN"
    && Card.get_property_name card = "None"

  (* Keeps drawing a card at random until the card satisfies a given predicate,
     then returns that card. *)
  let rec draw_valid_card (f : Card.t -> bool) : Card.t =
    let c = draw [] 1 in
    if f (List.hd c) then List.hd c else draw_valid_card f

  (* Creates the hands for the player and enemy hands, drawing 7 cards for
     each. *)
  let create_hands : t =
    let h1 = draw [] 7 in
    let h2 = draw [] 7 in
    {
      player_hand = h1;
      enemy_hand = h2;
      discard_pile = draw_valid_card is_valid_first_card;
      difficulty = "";
    }

  (* Returns true if a card can legally be played on the discard_pile. *)
  let is_legal_play (card : Card.t) (discard_pile : Card.t) : bool =
    if Card.get_color card = Card.get_color discard_pile then true
    else if Card.get_number card = Card.get_number discard_pile then true
    else Card.get_color card = "Wild"

  (* Given a LEGAL card to play, and the game, and a boolean "player" which
     indicates whether the card is being played by a person [true] or by the
     opponent [false], this function will play the card onto the discard_pile,
     and will force any side effects of the card onto the other player, if
     applicable. Returns a game with the updated hands and discard_piles for the
     players. *)
  let play_card (card : Card.t) (game : t) (player : bool) : t = game

  (* let side_effect card = match Card.get_property card with | None -> None |
     in side_effect card *)
  (* if person then () else *)
  (*The AI plays*)

  (* draws a card to a players hand, takes in a boolean "player" which indicates
     whether the card is being drawn to the person's hand [true] or by the
     opponent [false] *)
  let draw_update (game : t) (player : bool) : t =
    match player with
    | true ->
        let h' = draw game.player_hand 1 in
        {
          player_hand = h';
          enemy_hand = game.enemy_hand;
          discard_pile = game.discard_pile;
          difficulty = "";
        }
    | false ->
        let h' = draw game.enemy_hand 1 in
        {
          player_hand = game.player_hand;
          enemy_hand = h';
          discard_pile = draw_valid_card is_valid_first_card;
          difficulty = "";
        }

  (* checks to see if enemy can play a card, if so, the enemy plays the card,
     else the enemy draws a card and sees if they can play card, else they do
     not any card during their turn. Returns the updated game. *)
  let enemy_turn (game : t) : t = game
  (* let try_again game =

     let decide_course game = match AI.enemy_turn (game) with | Some enemy_card
     -> (play_card enemy_card game false) | None -> try_again in game.enemy_hand
     = enemy_updated_hand; game *)
end

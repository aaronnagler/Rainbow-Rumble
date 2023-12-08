open Cards
open Opp
include Cards

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    difficulty : string;
    discard_pile : Card.t;
  }

  let rec print_colored_text color s : unit =
    let c =
      match String.lowercase_ascii color with
      | "red" -> "\027[31m"
      | "green" -> "\027[32m"
      | "yellow" -> "\027[33m"
      | "blue" -> "\027[34m"
      | "wild" -> "\027[95m"
      | "white" -> ""
      | _ -> "Not a recognized color"
    in

    let reset = "\027[0m" in
    print_string (c ^ s ^ reset)

  (* Prints the color, number, and property of [card] where applicable. Example
     outputs: "[Red 1]", "[Yellow Draw 2]", "[Draw 4]", "[Wild]"*)
  let print_card (card : Card.t) : unit =
    let c = Card.get_color card in
    let n = Card.get_number card in
    let p = Card.get_property_name card in
    let print_bracket c s = print_colored_text c ("[" ^ s ^ "]") in
    match (c, n, p) with
    | "Wild", _, "Draw 4" -> print_bracket "wild" "Draw 4"
    | "Wild", _, "None" -> print_bracket "wild" "Wild"
    | color, _, "Draw 2" -> print_bracket color (color ^ " Draw 2")
    | color, numb, _ -> print_bracket color (color ^ " " ^ numb)

  (* Prints the player's hand. *)
  let rec print_player_hand (hand : Card.t list) (acc : int) : unit =
    match hand with
    | [] -> ()
    | h :: t ->
        print_string " ";
        print_int acc;
        print_string ".";
        print_card h;
        print_player_hand t (acc + 1)

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
    let same_color, same_number, same_property =
      ( Card.get_color card = Card.get_color discard_pile,
        Card.get_number card = Card.get_number discard_pile,
        Card.get_property_name card = Card.get_property_name discard_pile )
    in
    match (same_color, same_number, same_property) with
    | true, _, _ | _, true, _ -> true
    | false, false, true ->
        Card.get_color card = "None"
        (* If the two cards have the same property, and it's not "None" they can
           be played on each other*)
    | false, false, false -> Card.get_color card = "Wild"

  (* If discard pile card is a card of color Wild, transforms card color to
     user-described color [new_color] *)
  let transform_pile_wild (game : t) (new_color : string) : t =
    let new_prop = Card.get_property_name game.discard_pile in
    let new_card = Card.make_card new_color "Wild" new_prop in
    { game with discard_pile = new_card }

  (* Removes [card] from [hand] Returns: [hand] without [card] *)
  let rec remove_card (card : Card.t) = function
    | [] -> []
    | f :: rest when f = card -> rest
    | f :: rest -> f :: remove_card card rest

  (* Applies effect specified in [card.property] to [applier] hand, [affected]
     hand or both. *)
  let apply_effect (card : Card.t) (applier : Card.t list)
      (affected : Card.t list) =
    match Card.get_property_name card with
    | "Draw 4" -> (applier, draw affected 4)
    | "Draw 2" -> (applier, draw affected 2)
    | _ -> (applier, affected)

  (* Given a LEGAL card to play, the game, and a bool "player" which indicates
     whether the card is being played by the player [true] or by the opponent
     [false], this function will play the card onto the discard_pile, and will
     force any side effects of the card onto the other player, if applicable.
     Returns a game with the updated hands and discard_piles for the players. *)
  let play_card (card : Card.t) (game : t) (player : bool) : t =
    (* create new game state with: 1. Person playing card loses card [card] 2.
       Set [game.discard_pile] to [card] 3. Apply effect to opposing player
       (represented by [player]) *)
    let new_player_hand, new_enemy_hand =
      match player with
      | true ->
          apply_effect card (remove_card card game.player_hand) game.enemy_hand
      | false ->
          apply_effect card (remove_card card game.enemy_hand) game.player_hand
    in
    {
      game with
      player_hand = new_player_hand;
      enemy_hand = new_enemy_hand;
      discard_pile = card;
    }

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

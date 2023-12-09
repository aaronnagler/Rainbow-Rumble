open Cards
open Opp
include Cards
include Opp

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    difficulty : string;
    discard_pile : Card.t;
  }

  let rec print_colored_text (color : string) (s : string) : unit =
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

  let print_card (card : Card.t) : unit =
    let c = Card.get_color card in
    let n = Card.get_number card in
    let p = Card.get_property_name card in
    let print_bracket c s = print_colored_text c ("[" ^ s ^ "]") in
    match (c, n, p) with
    | "Wild", "NaN", "Draw 4" -> print_bracket "wild" "Draw 4"
    | "Wild", "NaN", "None" -> print_bracket "wild" "Wild"
    | color, "NaN", "Draw 4" ->
        print_bracket color (String.capitalize_ascii color ^ " Draw 4")
    | color, "NaN", "None" ->
        print_bracket color (String.capitalize_ascii color ^ " Wild")
    | color, _, "Draw 2" -> print_bracket color (color ^ " Draw 2")
    | color, numb, _ -> print_bracket color (color ^ " " ^ numb)

  let print_long (card : Card.t) : unit =
    let c = Card.get_color card in
    let n = Card.get_number card in
    let p = Card.get_property_name card in
    let ld = Card.get_property_description card in
    match (c, n, p, ld) with
    | "Wild", _, "Draw 4", d ->
        print_string ("Card: Wild; Property: Draw 4; description: " ^ d)
    | "Wild", _, "None", _ ->
        print_string "Card: Wild; Property: none; Description: none"
    | color, _, "Draw 2", d ->
        print_string ("Card: " ^ color ^ "; Property: Draw 2; Description: " ^ d)
    | color, numb, _, _ ->
        print_string
          ("Card: " ^ color ^ " " ^ numb ^ "; Property: none; Description: none")

  let print_desc (card : Card.t) : unit =
    print_string (Card.get_property_description card)

  let rec print_player_hand (hand : Card.t list) (acc : int) : unit =
    match hand with
    | [] -> ()
    | h :: t ->
        print_string " ";
        print_int acc;
        print_string ".";
        print_card h;
        print_player_hand t (acc + 1)

  let print_both_hands (game : t) : unit =
    print_string "Player's hand:";
    List.iter print_card game.player_hand;
    print_string "\nEnemy's hand:";
    List.iter print_card game.player_hand

  let rec draw (hand : Card.t list) (n : int) : Card.t list =
    let local_rng = Random.State.make_self_init () in
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card local_rng in
      draw (hand @ [ random_card ]) (n - 1)

  let is_valid_first_card (card : Card.t) : bool =
    Card.get_color card <> "Wild"
    && Card.get_number card <> "NaN"
    && Card.get_property_name card = "None"

  let rec draw_valid_card (f : Card.t -> bool) : Card.t =
    let c = draw [] 1 in
    if f (List.hd c) then List.hd c else draw_valid_card f

  let create_hands (d : string) : t =
    let h1 = draw [] 7 in
    let h2 = draw [] 7 in
    {
      player_hand = h1;
      enemy_hand = h2;
      discard_pile = draw_valid_card is_valid_first_card;
      difficulty = d;
    }

  let create_game h1 h2 dis dif : t =
    { player_hand = h1; enemy_hand = h2; discard_pile = dis; difficulty = dif }

  let is_legal_play (card : Card.t) (discard_pile : Card.t) : bool =
    let same_color, same_number, same_property =
      ( Card.get_color card = Card.get_color discard_pile,
        Card.get_number card = Card.get_number discard_pile,
        Card.get_property_name card = Card.get_property_name discard_pile )
    in
    match (same_color, same_number, same_property) with
    | true, _, _ | _, true, _ -> true
    | false, false, true ->
        Card.get_color card = "Wild"
        (* If the two cards have the same property, and it's not "None" they can
           be played on each other*)
    | false, false, false -> Card.get_color card = "Wild"

  let transform_pile_wild (game : t) (new_color : string) : t =
    let new_prop = Card.get_property_name game.discard_pile in
    let new_card = Card.make_card new_color "NaN" new_prop in
    {
      discard_pile = new_card;
      difficulty = game.difficulty;
      enemy_hand = game.enemy_hand;
      player_hand = game.player_hand;
    }

  let rec remove_card (card : Card.t) = function
    | [] -> []
    | f :: rest when f = card -> rest
    | f :: rest -> f :: remove_card card rest

  let apply_effect (card : Card.t) (applier : Card.t list)
      (affected : Card.t list) =
    match Card.get_property_name card with
    | "Draw 4" -> (applier, draw affected 4)
    | "Draw 2" -> (applier, draw affected 2)
    | _ -> (applier, affected)

  let play_card (card : Card.t) (game : t) (player : bool) : t =
    (* create new game state with: 1. Person playing card loses card [card] 2.
       Set [game.discard_pile] to [card] 3. Apply effect to opposing player
       (represented by [player]) *)
    let new_player_hand, new_enemy_hand =
      match player with
      | true ->
          apply_effect card (remove_card card game.player_hand) game.enemy_hand
      | false -> (
          match
            apply_effect card
              (remove_card card game.enemy_hand)
              game.player_hand
          with
          | x, y -> (y, x))
    in
    {
      player_hand = new_player_hand;
      enemy_hand = new_enemy_hand;
      discard_pile = card;
      difficulty = game.difficulty;
    }

  let draw_update (game : t) (player : bool) : t =
    match player with
    | true ->
        let h' = draw game.player_hand 1 in
        {
          player_hand = h';
          enemy_hand = game.enemy_hand;
          discard_pile = game.discard_pile;
          difficulty = game.difficulty;
        }
    | false ->
        let h' = draw game.enemy_hand 1 in
        {
          player_hand = game.player_hand;
          enemy_hand = h';
          discard_pile = game.discard_pile;
          difficulty = game.difficulty;
        }

  let rec enemy_turn_helper (enemy_hand : Card.t list) (difficulty : string)
      (discard_pile : Card.t) (player_hand_num : int) : Card.t option =
    match AI.enemy_turn enemy_hand difficulty discard_pile player_hand_num with
    | Some x -> (
        match is_legal_play x discard_pile with
        | true -> Some x
        | false ->
            enemy_turn_helper (remove_card x enemy_hand) difficulty discard_pile
              player_hand_num)
    | None -> None

  let enemy_turn (game : t) : Card.t option =
    enemy_turn_helper game.enemy_hand game.difficulty game.discard_pile
      (List.length game.player_hand)

  let most_common_color (hand : Card.t list) : string =
    match Card.most_common_color hand with
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Wild -> "Wild"

  let check_winner (game : t) : bool * int =
    match (List.length game.player_hand, List.length game.enemy_hand) with
    | 0, _ -> (true, 0)
    | _, 0 -> (true, 1)
    | _, _ -> (false, 2)

  let check_voiceline (game : t) : string option =
    AI.enemy_voiceline game.enemy_hand game.difficulty
end

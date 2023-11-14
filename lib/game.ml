open Cards

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    deck : Card.t;
  }

  (* Prints the color and number of [card]. Example output: "[Yellow 5]"*)
  let print_card (card : Card.t) : unit =
    let num = Card.get_number card in
    let col = Card.get_color card in
    let prop = Card.get_property card in
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
    List.iter print_card game.enemy_hand

  (* Draws a card at random and adds it to the hand provided.*)
  let rec draw (hand : Card.t list) (n : int) : Card.t list =
    let local_rng = Random.State.make_self_init () in
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card local_rng in
      draw (hand @ [ random_card ]) (n - 1)

  (* Returns true if the given card is a valid card to initialize the deck. *)
  let is_valid_first_card (card : Card.t) : bool =
    Card.get_color card <> "Wild"
    && Card.get_number card <> "NaN"
    && Card.get_property card = "None"

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
      deck = draw_valid_card is_valid_first_card;
    }

  (* Returns true if a card can legally be played on the deck. *)
  let is_legal_play (card : Card.t) (deck : Card.t) : bool =
    if Card.get_color card = Card.get_color deck then true
    else if Card.get_number card = Card.get_number deck then true
    else Card.get_color card = "Wild"
end

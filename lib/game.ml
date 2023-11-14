open Cards

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
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
  let rec draw (hand : Cards.Card.t list) (n : int) : Cards.Card.t list =
    let local_rng = Random.State.make_self_init () in
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card local_rng in
      draw (hand @ [ random_card ]) (n - 1)

  (* Creates the hands for the player and enemy hands, drawing 7 cards for
     each. *)
  let create_hands : t =
    let h1 = draw [] 7 in
    let h2 = draw [] 7 in
    { player_hand = h1; enemy_hand = h2 }
end

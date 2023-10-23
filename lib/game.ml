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
    print_string ("[" ^ col ^ " " ^ num ^ "]")

  (* Prints the hands of the player_hand and the enemy hand. *)
  let print_hands (game : t) : unit =
    print_string "Player's hand:";
    List.iter print_card game.player_hand;
    print_string "\nEnemy's hand:";
    List.iter print_card game.enemy_hand

  let rec draw (hand : Cards.Card.t list) (n : int) : Cards.Card.t list =
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card in
      draw (hand @ [ random_card ]) (n - 1)
end

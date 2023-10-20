open Cards

module Game = struct
  type t = {
    player_hand : Cards.Card.t list;
    enemy_hand : Cards.Card.t list;
  }

  let print_hands (game : t) = failwith "unimplemented"

  let rec draw (hand : Cards.Card.t list) (n : int) : Cards.Card.t list =
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card in
      draw (hand @ [ random_card ]) (n - 1)
end

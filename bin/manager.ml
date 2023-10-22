open Cards

let () = print_endline "hello world"

module Game = struct
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
  }

  (* Prints [card] *)
  let print_card (card : Card.t) : unit = print_string "[placeholder]"

  (* Prints the hands of the player_hand and the enemy hand. *)
  let print_hands (game : t) : unit =
    List.iter print_card game.player_hand;
    List.iter print_card game.enemy_hand

  let rec draw (hand : Cards.Card.t list) (n : int) : Cards.Card.t list =
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card in
      draw (hand @ [ random_card ]) (n - 1)
end

open Cards
open Opp

module Game = struct
  type t = {
    player_hand : Card.t array;
    enemy_hand : Card.t array;
  }

  (* Prints the color and number of [card]. Example output: "[Yellow 5]"*)
  let print_card (card : Card.t) : unit =
    let num = Card.get_number card in
    let col = Card.get_color card in
    let prop = Card.get_property card in
    print_string ("[" ^ col ^ " " ^ num ^ " " ^ prop ^ "]")

  (* Prints the hands of the player_hand and the enemy hand. *)
  let print_hands (game : t) : unit =
    print_string "Player's hand:";
    List.iter print_card (Array.to_list game.player_hand);
    print_string "\nEnemy's hand:";
    List.iter print_card (Array.to_list game.enemy_hand)

  let rec draw (hand : Cards.Card.t list) (n : int) : Cards.Card.t list =
    let local_rng = Random.State.make_self_init () in
    if n = 0 then hand
    else
      let random_card = Card.get_rand_card local_rng in
      draw (hand @ [ random_card ]) (n - 1)
  (* let play (hand : Cards.Card.t list) = *)
end

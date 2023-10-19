open cards

module Game = struct
  type t = {player_hand : Card.card list; enemy_hand : Card.card list}

  let print_hands = failwith "unimplemented"
  let rec draw (hand : Card.card list) (n : int) = failwith "unimplemented"

  let begin_game = (draw t.player_hand 7) (draw t.enemy_hand 7) (print_hands)
end



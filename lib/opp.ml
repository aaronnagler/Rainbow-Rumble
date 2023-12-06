open Cards
(* open Game *)

module AI = struct
  (**Given a hand, a random card will be selected to be placed on the pile. If
     the play is illegal, another card will be randomly selected*)
  (* let rec play_card (game) = let ran_card = List.nth (game.player_hand)
     (Random.int (List.Alength hand)) in game. *)

  (** The enemy will attempt to select a playable card from their hand. If
      succesful, return the card it will playx from its hand. Otherwise return
      None, meaning the enemy has no cards to play from its hand *)
  let easy_mode_turn (enemy_hand : Card.t list) (difficulty : string) :
      Card.t option =
    failwith "Unimplemented"

  let hard_mode_turn (enemy_hand : Card.t list) (difficulty : string) :
      Card.t option =
    failwith "Unimplemented"

  let enemy_turn (enemy_hand : Card.t list) (difficulty : string)
      (discard_pile : Card.t) : Card.t option =
    if difficulty = "Easy" then easy_mode_turn enemy_hand difficulty
    else if difficulty = "Hard" then hard_mode_turn enemy_hand difficulty
    else
      match Random.int 2 with
      | 0 -> easy_mode_turn enemy_hand difficulty
      | _ -> hard_mode_turn enemy_hand difficulty
end

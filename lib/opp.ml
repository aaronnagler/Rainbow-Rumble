open Cards
(* open Game *)

module AI = struct
  (*Given a hand, a random card will be selected to be placed on the pile. If
    there's no card left to select from, then return None, meaning none of the
    cards in the enemy's hand can be played*)
  let rec easy_mode_turn (enemy_hand : Card.t list) (discard_pile : Card.t) :
      Card.t option =
    match enemy_hand with
    | [] -> None
    | _ -> Some (List.nth enemy_hand (Random.int (List.length enemy_hand)))

  
  let find_all_ = failwith "Unimplemented"
  
    (*If both enemy and players have 4 or more cards, try to play a normal card
    firsts, followed by non-wild special cards, then wilds*)
  let rec strategy_1 (enemy_hand : Card.t list) : Card.t option =
    match enemy_hand with
    | [] -> None
    | h :: t -> None
  (*If an enemy has 3 or less cards and player has 4 or more, the enemy first
    play cards that can skip the players turn (+2, skip, +4), and then play wild
    cards at the end to guarentee a next turn win (Ex: Wild -> Green 2)*)

  let rec strategy_2 (enemy_hand : Card.t list) : Card.t option =
    failwith "Unimplemented"

  (*If player has 3 or less cards and enemy has 4 or more, the enemy will
    attempt to sabotage the player such as using special cards or using a wild
    to change the player's wild card*)
  let rec strategy_3 (enemy_hand : Card.t list) : Card.t =
    failwith "Unimplemented"

  (*Given a hand, certain cards in the enemy's hand will have priority in being
    picked based on the game state. The enemy will use the 3 main strategys, the
    4th being a random choice between strategy 2 or 3 *)
  let hard_mode_turn (enemy_hand : Card.t list) (discard_pile : Card.t)
      (player_hand_num : int) : Card.t option =
    match enemy_hand with
    | [] -> None
    | _ -> (
        match (List.length enemy_hand <= 4, player_hand_num <= 4) with
        | true, true -> Some (strategy_1 enemy_hand)
        | false, true -> Some (strategy_2 enemy_hand)
        | true, false -> Some (strategy_3 enemy_hand)
        | _ -> (
            match Random.int 2 with
            | 0 -> Some (strategy_2 enemy_hand)
            | _ -> Some (strategy_3 enemy_hand)))

  (* The enemy will attempt to select a playable card from their hand. If
     succesful, return the card it will playx from its hand. Otherwise return
     None, meaning the enemy has no cards to play from its hand *)
  let enemy_turn (enemy_hand : Card.t list) (difficulty : string)
      (discard_pile : Card.t) (player_hand_num : int) : Card.t option =
    if difficulty = "Easy" then easy_mode_turn enemy_hand discard_pile
    else if difficulty = "Hard" then
      hard_mode_turn enemy_hand discard_pile player_hand_num
    else
      match Random.int 2 with
      | 0 -> easy_mode_turn enemy_hand discard_pile
      | _ -> hard_mode_turn enemy_hand discard_pile player_hand_num
end

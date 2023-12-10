open Cards

module AI = struct
  let rec easy_mode_turn (enemy_hand : Card.t list) (discard_pile : Card.t) :
      Card.t option =
    match enemy_hand with
    | [] -> None
    | _ -> Some (List.nth enemy_hand (Random.int (List.length enemy_hand)))

  let rec strategy_1 (enemy_hand : Card.t list) : Card.t option =
    match Card.filter_number_cards enemy_hand with
    | h :: t -> Some h
    | [] -> (
        match Card.filter_special_cards enemy_hand with
        | h :: t -> Some h
        | [] -> (
            match Card.filter_wild_cards enemy_hand with
            | h :: t -> Some h
            | [] -> None))

  let rec strategy_2 (enemy_hand : Card.t list) : Card.t option =
    match Card.filter_special_cards enemy_hand with
    | h :: t -> Some h
    | [] -> (
        match Card.filter_number_cards enemy_hand with
        | h :: t -> Some h
        | [] -> (
            match Card.filter_wild_cards enemy_hand with
            | h :: t -> Some h
            | [] -> None))

  (**If player has 3 or less cards and enemy has 4 or more, the enemy will
     attempt to sabotage the player such as using special cards, wilds, and then
     numbered*)
  let rec strategy_3 (enemy_hand : Card.t list) : Card.t option =
    match Card.filter_special_cards enemy_hand with
    | h :: t -> Some h
    | [] -> (
        match Card.filter_wild_cards enemy_hand with
        | h :: t -> Some h
        | [] -> (
            match Card.filter_number_cards enemy_hand with
            | h :: t -> Some h
            | [] -> None))

  let hard_mode_turn (enemy_hand : Card.t list) (discard_pile : Card.t)
      (player_hand_num : int) : Card.t option =
    match enemy_hand with
    | [] -> None
    | _ -> (
        match (List.length enemy_hand >= 4, player_hand_num >= 4) with
        | true, true -> strategy_1 enemy_hand
        | false, true -> strategy_2 enemy_hand
        | true, false -> strategy_3 enemy_hand
        | _ -> (
            match Random.int 2 with
            | 0 -> strategy_2 enemy_hand
            | _ -> strategy_3 enemy_hand))

  let enemy_turn (enemy_hand : Card.t list) (difficulty : string)
      (discard_pile : Card.t) (player_hand_num : int) : Card.t option =
    if difficulty = "easy" then easy_mode_turn enemy_hand discard_pile
    else if difficulty = "hard" then
      hard_mode_turn enemy_hand discard_pile player_hand_num
    else
      match Random.int 2 with
      | 0 -> easy_mode_turn enemy_hand discard_pile
      | _ -> hard_mode_turn enemy_hand discard_pile player_hand_num

  let winning_bark : string =
    match Random.int 17 with
    | 0 -> "Uno!"
    | 1 -> "Ha, I'm about to win!"
    | 2 -> "It's Joever!"
    | 3 -> "Ur done for!"
    | 4 -> "Bro, are you giving me the win?"
    | 5 -> "Imagine losing to a bot!"
    | 6 -> "Thx for the easy win!"
    | 7 -> "Can't believe I'm gonna win!"
    | 8 -> "I swear on OCaml that One Piece gets good when you get to Ocean 6!"
    | 10 -> "I won and I wasn't even plugged in!"
    | 11 -> "Sorry my lowest power level was too high for you!"
    | 12 ->
        "I always wondered what it would be like to play Uno solitare, the way \
         you gave provided me with nonexistent competition!"
    | 13 -> "Your playstyle reminds me of my toddler days! Hah!"
    | 14 ->
        "Sorry this game is too difficult for you, would 52 pickup be more \
         your speed?"
    | 15 -> "I hope you enjoyed all my draw 4 cards :)!"
    | 16 -> "Your skill level is the UNO reverse of mastery!"
    | 17 -> "That game didn't even make me break a sweat!"
    | _ -> "Heheheha!"

  let uno_voiceline (last_card : Card.t) (difficulty : string) : string =
    match difficulty with
    | "easy" ->
        winning_bark
        ^ " Since I'm going to win, I'll tell the color of my last card: "
        ^ Card.get_number last_card ^ " of " ^ Card.get_color last_card
    | "medium" ->
        winning_bark ^ " I'll give you a hint, the card color in my hand is: "
        ^ Card.get_color last_card
    | _ ->
        winning_bark
        ^ " I'm not tell you anything, but feel free to try and stop me!"

  let enemy_voiceline (enemy_hand : Card.t list) (difficulty : string) :
      string option =
    match (List.length enemy_hand, difficulty) with
    | 1, "easy" -> Some (uno_voiceline (List.nth enemy_hand 0) "easy")
    | 1, "medium" -> Some (uno_voiceline (List.nth enemy_hand 0) "medium")
    | 1, "hard" -> Some (uno_voiceline (List.nth enemy_hand 0) "hard")
    | 3, _ -> Some "Three cards left, I can smell the victory!"
    | _ -> None
end

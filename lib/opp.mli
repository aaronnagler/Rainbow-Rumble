open Cards

(**A module representing the opponent AI and its decision-making functions. *)
module AI : sig
  val easy_mode_turn : Card.t list -> Card.t -> Card.t option
  (**Given a hand, a random card will be selected to be placed on the pile. If
     there's no card left to select from, then return None, meaning none of the
     cards in the enemy's hand can be played*)

  val strategy_1 : Card.t list -> Card.t option
  (**If both enemy and players have 4 or more cards, try to play a normal card
     firsts, followed by special cards, then wilds*)

  val strategy_2 : Card.t list -> Card.t option
  (**If an enemy has 3 or less cards and player has 4 or more, the enemy first
     play cards that can skip the players turn (+2, skip, +4), following
     numbered, and then wild cards*)

  val strategy_3 : Card.t list -> Card.t option
  (**If player has 3 or less cards and enemy has 4 or more, the enemy will
     attempt to sabotage the player such as using special cards, wilds, and then
     numbered*)

  val hard_mode_turn : Card.t list -> Card.t -> int -> Card.t option
  (**Given a hand, certain cards in the enemy's hand will have priority in being
     picked based on the game state. The enemy will use the 3 main strategies,
     the 4th being a random choice between strategy 2 or 3 when both players
     have 3 or less cards*)

  val enemy_turn : Card.t list -> string -> Card.t -> int -> Card.t option
  (**The enemy will attempt to select a playable card from their hand. If
     succesful, return the card it will play from its hand. Otherwise return
     None, meaning the enemy has no cards to play from its hand *)

  val winning_bark : string
  (**Returns a random taunt for the opponent AI to say.*)

  val uno_voiceline : Card.t -> string -> string
  (**Returns an opponent AI voiceline (after they reach 1 card) that may/may not
     give a hint depending on the difficulty of the current game. Such hint will
     reveal information about the last card that the opponent holds.*)

  val enemy_voiceline : Card.t list -> string -> string option
  (**Depending on the enemy's hand and difficulty, certain voicelines will be
     said*)
end

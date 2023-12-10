open Cards

(**The module of the Game and its associated functions. *)
module Game : sig
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    difficulty : string;
    discard_pile : Card.t;
  }
  (**Type that represents the game state with card data and difficulty. *)

  val print_colored_text : string -> string -> unit
  (**Returns [s] with ANSI escape codes that represent the given color. *)

  val print_card : Card.t -> unit
  (**Prints the color, number, and property of [card] where applicable. Example
     outputs: "[Red 1]", "[Yellow Draw 2]", "[Blue Draw 4]", "[Wild]"*)

  val print_long : Card.t -> unit
  (**Prints a long form description of [card], including the effects of a wild
     card if it is appicable *)

  val print_desc : Card.t -> unit
  (**Prints the effect of a card, if it has one, otherwise prints "none" *)

  val print_player_hand : Card.t list -> int -> unit
  (**Prints the player's hand. *)

  val print_both_hands : t -> unit
  (**Prints the hands of the [player_hand] and the [enemy_hand]. *)

  val draw : Card.t list -> int -> Card.t list
  (**Returns [hand] with [n] random cards added to the hand. *)

  val is_valid_first_card : Card.t -> bool
  (**Returns true if [card] is a valid card to initialize the discard_pile. That
     is, card is not wild color, has a number, and has no special properties. *)

  val draw_valid_card : (Card.t -> bool) -> Card.t
  (**Keeps drawing a card at random until the card satisfies a given predicate,
     then returns that card. *)

  val create_hands : string -> t
  (**Initializes game state. Depending on difficulty [d], creates player and
     enemy hands by drawing 7 cards for each. *)

  val create_game : Card.t list -> Card.t list -> Card.t -> string -> t
  (**Returns a game with the specified properties (player hand, enemy hand,
     discard pile, and game difficulty)*)

  val is_legal_play : Card.t -> Card.t -> bool
  (**Returns true if a card can legally be played on [discard_pile]. *)

  val transform_pile_wild : t -> string -> t
  (**If discard pile card is a card of color Wild, transforms card color to
     user-described color [new_color] *)

  val remove_card : Card.t -> Card.t list -> Card.t list
  (**Returns [hand] with the first instance of [card] removed. *)

  val apply_effect :
    Card.t -> Card.t list -> Card.t list -> Card.t list * Card.t list
  (**Applies effect specified in [card.property] to [applier] hand, [affected]
     hand or both. *)

  val play_card : Card.t -> t -> bool -> t
  (**Given a LEGAL card to play, the game, and a bool "player" which indicates
     whether the card is being played by the player [true] or by the opponent
     [false], this function will play the card onto the discard_pile, and will
     force any side effects of the card onto the other player, if applicable.
     Returns a game with the updated hands and discard_pile for the players.*)

  val draw_update : t -> bool -> t
  (**Draws a card to a players hand, takes in a boolean "player" which indicates
     whether the card is being drawn to the person's hand [true] or by the
     opponent [false] *)

  val enemy_turn : t -> Card.t option
  (**Call AI.enemy_turn until Some playable card is returned. If there are no
     playable cards in hand, then return None*)

  val most_common_color : Card.t list -> string
  (**Returns the string representation of the color with the most occurences in
     the cards of the hand. In the result of ties between colors (including the
     absence of any color cards), the preference will be Red, Blue, Green, then
     Yellow.*)

  val check_winner : t -> bool * int
  (**Checks if either player or opponent meets the win condition: if they have 0
     cards in their hand. Returns (true, 0) if player winds, (true, 1) if
     opponent winds, and (false, 2) if there is no current winner. *)

  val check_voiceline : t -> string option
  (**Returns some voiceline depending on the enemy's hand and difficulty.*)
end

open Cards

module Game : sig
  type t = {
    player_hand : Card.t list;
    enemy_hand : Card.t list;
    difficulty : string;
    discard_pile : Card.t;
  }

  val print_colored_text : string -> string -> unit
  val print_card : Card.t -> unit
  val print_long : Card.t -> unit
  val print_desc : Card.t -> unit
  val print_player_hand : Card.t list -> int -> unit
  val print_both_hands : t -> unit
  val draw : Card.t list -> int -> Card.t list
  val is_valid_first_card : Card.t -> bool
  val draw_valid_card : (Card.t -> bool) -> Card.t
  val create_hands : string -> t
  val is_legal_play : Card.t -> Card.t -> bool
  val transform_pile_wild : t -> string -> t
  val remove_card : Card.t -> Card.t list -> Card.t list

  val apply_effect :
    Card.t -> Card.t list -> Card.t list -> Card.t list * Card.t list

  val play_card : Card.t -> t -> bool -> t
  val draw_update : t -> bool -> t

  val enemy_turn_helper :
    Card.t list -> string -> Card.t -> int -> Card.t option

  val enemy_turn : t -> Card.t option
  val most_common_color : Card.t list -> string
  val check_winner : t -> bool * int
  val check_voiceline : t -> string option
end

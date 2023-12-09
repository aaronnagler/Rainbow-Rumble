open Cards

module AI : sig
  val easy_mode_turn : Card.t list -> Card.t -> Card.t option
  val strategy_1 : Card.t list -> Card.t option
  val strategy_2 : Card.t list -> Card.t option
  val strategy_3 : Card.t list -> Card.t option
  val hard_mode_turn : Card.t list -> Card.t -> int -> Card.t option
  val enemy_turn : Card.t list -> string -> Card.t -> int -> Card.t option
end

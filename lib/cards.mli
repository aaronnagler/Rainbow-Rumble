(**The color of the card, or Wild when the card doesn't have one assigned color *)
type color =
  | Red
  | Blue
  | Green
  | Yellow
  | Wild

(**The number the a card has, or NaN if the card doesn't have a number*)
type numb =
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | NaN

(**The special property that a card has *)
type prop =
  | Draw2
  | Draw4

type card = {
  color : color;
  number : numb;
  property : prop option;
}
(**The type representing a card *)

(**The module representing the type a card can be. *)
module type CardType = sig
  type t
  (**Type representing a card *)

  val get_color : t -> string
  (**Returns the color of the card as a string *)

  val get_number : t -> string
  (**Returns the number of the card as a string *)

  val get_property_name : t -> string
  (**If the card has a property, return card property as a string. Returns
     [None] if the card has no property *)

  val get_property_description : t -> string
  (**If the card has a property, return card property as a string. Returns
     "None" if the card has no property *)

  val get_rand_card : Random.State.t -> t
  (**Returns a randomly generated card *)

  val make_numb : string -> numb
  (**Given a string [numb], returns the associated number. *)

  val make_color : string -> color
  (**Given a string [color], returns the associated color. *)

  val make_prop : string -> prop option
  (**Given a string prop, returns the associated number. *)

  val make_card : string -> string -> string -> t
  (**Returns a card with specified color, number, and property *)

  val filter_number_cards : t list -> t list
  (**Returns all number cards in a given hand*)

  val filter_special_cards : t list -> t list
  (**Returns all cards with special property (excluding wild) in a given hand*)

  val filter_wild_cards : t list -> t list
  (**Returns all wild cards (that is, cards with a "wild" color) given a hand*)

  val most_common_color : t list -> color
  (**Returns the most prevalant color in hand.*)
end

(* A module representing a card of a certain type. *)
module Card : CardType

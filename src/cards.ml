type color = Red | Blue | Green | Yellow | Wild
type numb = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | NaN
type card = {
  color : color;
  number : numb;
  property : string option
}

module type CardType = sig
  type t
  val get_color : t -> string
  val get_number : t -> string
  val get_property : t -> string
  val is_compatable : t -> t -> bool
end

module Card:CardType = struct
  type t = card
  let get_color t = 
    match t.color with
    | Red -> "Red"
    | Blue -> "BLue"
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Wild -> "Wild"
  let get_number t = 
    match t.number with
    | Zero -> "0"
    | One -> "1"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | NaN -> "NaN"
  let get_property t = 
    match t.property with
    | None -> "None"
    | Some p -> p
  let is_compatable a b =
    if a.color = Wild then true else
      if get_color a = get_color b || get_number a = get_number b then true else false
end
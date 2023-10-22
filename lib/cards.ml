type color =
  | Red
  | Blue
  | Green
  | Yellow
  | Wild

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

type card = {
  color : color;
  number : numb;
  property : string option;
}

module type CardType = sig
  (* Type representing a card *)
  type t

  (* Returns the color of the card as a string *)
  val get_color : t -> string

  (* Returns the number of the card as a string *)
  val get_number : t -> string

  (* If the card has a property, return card property as a string. Returns
     "None" if the card has no property *)
  val get_property : t -> string

  (* Return true if a card can be played on top of another card. Return false if
     not. A card can be played on top of another if any of the following apply
     a) the card to be played has color = Wild b) both cards have the same color
     c) both cards have the same number *)
  val is_compatable : t -> t -> bool

  (* Returns a randomly generated card *)
  val get_rand_card : t
end

module Card : CardType = struct
  type t = card

  let get_color t =
    match t.color with
    | Red -> "Red"
    | Blue -> "Blue"
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
    if a.color = Wild then true
    else if get_color a = get_color b || get_number a = get_number b then true
    else false

  (* this should maybe be in list and not card *)
  let get_rand_card =
    let color_list = [ Wild; Red; Blue; Green; Yellow ] in
    let numb_list =
      [ NaN; Zero; One; Two; Three; Four; Five; Six; Seven; Eight; Nine ]
    in
    match Random.int 5 with
    | 0 -> { color = Wild; number = NaN; property = Some "draw 4" }
    | x -> (
        match Random.int 11 with
        | 0 ->
            {
              color = List.nth color_list x;
              number = NaN;
              property = Some "draw 2";
            }
        | y ->
            {
              color = List.nth color_list x;
              number = List.nth numb_list y;
              property = None;
            })
end

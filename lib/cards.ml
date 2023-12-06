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

(* tuple (name, description) where name is the name of the property and
   description details the effect of the property *)
type prop =
  | Draw2 of string * string
  | Draw4 of string * string

type card = {
  color : color;
  number : numb;
  property : prop option;
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
  val get_property_name : t -> string

  (* If the card has a property, return card property as a string. Returns
     "None" if the card has no property *)
  val get_property_description : t -> string

  (* Return true if a card can be played on top of another card. Return false if
     not. A card can be played on top of another if any of the following apply
     a) the card to be played has color = Wild b) both cards have the same color
     c) both cards have the same number *)
  val is_compatable : t -> t -> bool

  (* Returns a randomly generated card *)
  val get_rand_card : Random.State.t -> t
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

  let get_property_name t =
    let p = t.property in
    match p with
    | None -> "None"
    | Some pro -> (
        match pro with
        | Draw2 (name, _) | Draw4 (name, _) -> name)

  let get_property_description t =
    let p = t.property in
    match p with
    | None -> "None"
    | Some pro -> (
        match pro with
        | Draw2 (_, desc) | Draw4 (_, desc) -> desc)

  let is_compatable a b =
    if a.color = Wild then true
    else if get_color a = get_color b || get_number a = get_number b then true
    else false

  (* this should maybe be in list and not card *)
  let get_rand_card local_rng_state =
    let color_list = [ Wild; Red; Blue; Green; Yellow ] in
    let numb_list =
      [ NaN; Zero; One; Two; Three; Four; Five; Six; Seven; Eight; Nine ]
    in
    match Random.State.int local_rng_state 5 with
    | 0 ->
        {
          color = Wild;
          number = NaN;
          property =
            Some (Draw4 ("Draw 4", "Opponent must draw 4 cards to their hand"));
        }
    | x -> (
        match Random.State.int local_rng_state 11 with
        | 0 ->
            {
              color = List.nth color_list x;
              number = NaN;
              property =
                Some
                  (Draw2 ("Draw 2", "Opponent must draw 2 cards to their hand"));
            }
        | y ->
            {
              color = List.nth color_list x;
              number = List.nth numb_list y;
              property = None;
            })

  (** Given a string [color], returns the associated color. *)
  let make_color (color : string) =
    match color with
    | "Red" -> Red
    | "Blue" -> Blue
    | "Green" -> Green
    | "Yellow" -> Yellow
    | "Wild" -> Wild
    | _ -> failwith "Not a color"

  (** Given a string [numb], returns the associated number. *)
  let make_numb (numb : string) =
    match numb with
    | "0" -> Zero
    | "1" -> One
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "7" -> Seven
    | "8" -> Eight
    | "9" -> Nine
    | "NaN" -> NaN
    | _ -> failwith "Not a numb"

  (** Given a string prop, returns the associated number. *)

  (* returns a card with specified color, number, and property *)
  let make_card (color : string) (numb : string) (prop : string) : t =
    failwith "skibidi"
end

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
  | WildNum

(* tuple (name, description) where name is the name of the property and
   description details the effect of the property *)
type prop =
  | Draw2
  | Draw4

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

  (* Given a string [numb], returns the associated number. *)
  val make_numb : string -> numb

  (* Given a string prop, returns the associated number. *)
  val make_prop : string -> prop option

  (* returns a card with specified color, number, and property *)
  val make_card : string -> string -> string -> t

  (*Returns all number cards given a hand*)
  val filter_number_cards : t list -> t list

  (*Return all cards with special property given a hand*)
  val filter_special_cards : t list -> t list

  (*Returns all wild cards given a hand*)
  val filter_wild_cards : t list -> t list
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
    | WildNum -> "Wild"

  let get_property_name t =
    let p = t.property in
    match p with
    | None -> "None"
    | Some prop -> (
        match prop with
        | Draw2 -> "Draw 2"
        | Draw4 -> "Draw 4")

  let get_property_description t =
    let p = t.property in
    match p with
    | None -> "None"
    | Some prop -> (
        match prop with
        | Draw2 -> "Opponent must draw 2 cards from the top of the deck"
        | Draw4 -> "Opponent must draw 4 cards from the top of the deck")

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
    match Random.State.int local_rng_state 6 with
    | 0 -> { color = Wild; number = NaN; property = None }
    | 5 -> { color = Wild; number = NaN; property = Some Draw4 }
    | x -> (
        match Random.State.int local_rng_state 11 with
        | 0 ->
            {
              color = List.nth color_list x;
              number = NaN;
              property = Some Draw2;
            }
        | y ->
            {
              color = List.nth color_list x;
              number = List.nth numb_list y;
              property = None;
            })

  (* Given a string [color], returns the associated color. *)
  let make_color (color : string) =
    match String.lowercase_ascii color with
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | "yellow" -> Yellow
    | "wild" -> Wild
    | _ -> failwith "Not a valid color"

  (* Given a string [numb], returns the associated number. *)
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
    | "Wild" -> WildNum
    | _ -> failwith "Not a valid numb"

  let make_prop (prop : string) =
    match prop with
    | "None" -> None
    | "Draw 2" -> Some Draw2
    | "Draw 4" -> Some Draw4
    | _ -> failwith "Not a valid prop"

  let make_card (color : string) (numb : string) (prop : string) : t =
    let c = make_color color in
    let n = make_numb numb in
    let p = make_prop prop in
    { color = c; number = n; property = p }

  let filter_number_cards (hand : t list) : t list =
    List.filter
      (fun card ->
        match card.number with
        | NaN -> false
        | _ -> true)
      hand

  let filter_special_cards (hand : t list) : t list =
    List.filter
      (fun card ->
        match card.property with
        | None -> false
        | _ -> true)
      hand

  let filter_wild_cards (hand : t list) : t list =
    List.filter
      (fun card ->
        match card.color with
        | Wild -> true
        | _ -> false)
      hand
end

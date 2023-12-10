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

type prop =
  | Draw2
  | Draw4

type card = {
  color : color;
  number : numb;
  property : prop option;
}

module type CardType = sig
  type t

  val get_color : t -> string
  val get_number : t -> string
  val get_property_name : t -> string
  val get_property_description : t -> string
  val get_rand_card : Random.State.t -> t
  val make_numb : string -> numb
  val make_color : string -> color
  val make_prop : string -> prop option
  val make_card : string -> string -> string -> t
  val filter_number_cards : t list -> t list
  val filter_special_cards : t list -> t list
  val filter_wild_cards : t list -> t list
  val most_common_color : t list -> color
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

  let make_numb (numb : string) =
    match String.lowercase_ascii numb with
    | "0" | "zero" -> Zero
    | "1" | "one" -> One
    | "2" | "two" -> Two
    | "3" | "three" -> Three
    | "4" | "four" -> Four
    | "5" | "five" -> Five
    | "6" | "six" -> Six
    | "7" | "seven" -> Seven
    | "8" | "eight" -> Eight
    | "9" | "nine" -> Nine
    | "nan" -> NaN
    | _ -> failwith "Not a valid numb"

  let make_color (color : string) =
    match String.lowercase_ascii color with
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | "yellow" -> Yellow
    | "wild" -> Wild
    | _ -> failwith "Not a valid color"

  let make_prop (prop : string) =
    match String.lowercase_ascii prop with
    | "none" -> None
    | "draw 2" | "draw2" -> Some Draw2
    | "draw 4" | "draw4" -> Some Draw4
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

  let most_common_color (hand : t list) : color =
    let rec increment_color acc hand =
      match hand with
      | [] -> ()
      | h :: t -> (
          match h.color with
          | Red -> acc.(0) <- acc.(0) + 1
          | Blue -> acc.(1) <- acc.(1) + 1
          | Green -> acc.(2) <- acc.(2) + 1
          | Yellow -> acc.(3) <- acc.(3) + 1
          | Wild -> ())
    in
    let acc = [| 0; 0; 0; 0 |] in
    increment_color acc hand;
    if acc.(0) >= acc.(1) && acc.(0) >= acc.(2) && acc.(0) >= acc.(3) then Red
    else if acc.(1) >= acc.(2) && acc.(1) >= acc.(3) then Blue
    else if acc.(2) >= acc.(3) then Green
    else Yellow
end

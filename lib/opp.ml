open Cards

module AI = struct
  (**Given a hand, a random card will be selected to be placed on the pile. If
     the play is illegal, another card will be randomly selected*)
  let rec play_card (hand : Card.t array) : unit = 
    let ran_card = hand.(Random.int (Array.length hand)) in 
    
    
    failwith "unimplemented"
end

open Card

module Game = struct
  type t = {player_hand : Card.card list; enemy_hand : Card.card list}
end

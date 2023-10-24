# Installation/Build Instructions

1. Navigate to project directory
2. Run 'dune build'

# Running Game Executable (Just prints the game title)

1. Run 'dune exec bin/manager.exe'

# Generating and Displaying the Cards in the Game

1. Open utop

    make utop

2. Run 'open Game' and 'open Game__Cards'

    open Game
    open Game_Cards

3. Create two hands using Game.draw

   let h1 = Game.draw [] 7
   let h2 = Game.draw [] 5

4. Create a new game (record)

    let g1 = {Game.player_hand = h1; Game.enemy_hand = h2};;

5. Print the hands

    Game.print_hands g1;;

6. Take a gander at the hands you generated :D
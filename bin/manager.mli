open Game
open Cards

val repl : (string -> string) -> unit
(** The read-eval-print loop for the game. *)

val draw_card : Game.t -> bool -> Game.t
(** Draws a card to the player's hand and returns the updated game state. *)

val read_card : string -> Game.t -> Game.t
(** Reads the card based on user input and returns the updated game state. *)

val play_card : string -> Game.t -> Game.t
(** Plays the card corresponding to the user input and returns the updated game
    state. *)

val opps_turn : Game.t -> Game.t
(** Performs the opponent's turn and returns the updated game state. *)

val transition_before_opp : Game.t -> Game.t
(** Transitions before the opponent's turn and returns the updated game state. *)

val stage_2 : unit -> string
(** Matches user input with a shorter string for processing. *)

val game_process : unit -> Game.t -> string
(** Processes the game based on user input and returns the result. *)

val start_game : unit -> string
(** Starts the game and returns a welcome message. *)

val stage_1 : unit -> string
(** Displays instructions for starting the game and takes user input. *)

val start_menu : unit -> string
(** Displays the start menu and returns the selected option. *)

val set_difficulty : unit -> string
(** Requests the desired difficulty from the player. *)

val play_again : unit -> string
(** Asks the player if they want to play again or go to the main menu. *)

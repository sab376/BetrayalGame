open Data
open Dicefile

(*initializes a player from character. Starts in room 0 *)
val new_player : (character -> room -> player)

(*[get_player_from_char plyrs c] returns the player in [plyrs] with
  character field [c] or None if none. *)
val get_player_from_char : player list -> character -> int option

(*returns the value of [stat] in [player] by looking up
index in player's character. Example: a
  player on stat index 2 who has [s] of 10 for all indexes would return 10 *)
val get_stat_value : (player -> stat -> int)

(*[get_stat_index plyr stat] returns the current index of [plyr]'s [stat]
  counter. Example: a
    player on stat index 2 who has [s] of 10 for all indexes would return 2 *)
val get_stat_index : (player -> stat -> int)

(*returns full character list of possible stat vals*)
val get_stat_list : (player -> stat -> int list)

(*returns a list of players stats in alphabeltical order *)
val get_all_sts : (player -> int list)

(* [can_increase_stat p stat] is true if [stat] can be increased for [p], false
   otherwise *)
val can_increase_stat : player -> stat -> bool

(*[let can_change_stat p stat] is true if [stat] of player [p] above index zero,
  false otherwise *)
val can_change_stat : player -> stat -> bool

(*returns [player] with index of [stat] changed by [int]. If result >8, cap
at 8. If result < 1 and haunt not begun, cap at 1. If result < 1 and haunt
then returns [player] with dead.
[int] can be pos or neg.*)
val change_stat: (player -> game_state -> int -> stat -> unit)

(* [set_stat p s stat] sets [stat] in [p] to [s]
   effects: changes mutable [stat] field in [p] *)
val set_stat : (player -> int -> stat -> unit)

(* [can_interact_bool_list gs p same_t] is a indexed list of true or false for each
   whether or not [p] can interact with each other player in [gs].
   If [same_t] is true, then checks according to attack
   rules and if [same_t] is false then checks according to trade rules *)
val can_interact_bool_list : game_state -> player -> bool -> bool list

(*[can_interact gs p same_t] is a indexed list of the players that [p]
   can interact with in [gs]. If [same_t] is true, then checks according to attack
   rules and if [same_t] is false then checks according to trade rules *)
val can_interact : game_state -> player -> bool -> player list


(* [can_attack_range gs p r] is a list of players in player [p]'s' line of sight
   within [r] number of tiles. p.current_room is counted as distance of 0.
   attackable players within the current_room are included in list.
   requires: r >= 0 *)
val can_attack_range : game_state -> player -> int -> player list

(*[get_player_index gs p] is index of [p] in [gs] player list*)
val get_player_index : game_state -> player -> int

(* [can_attack_range_bool gs p r] is a boolean list corresponding to players that
   can be attacked by [p] at range [r] in [gs] *)
val can_attack_range_bool : game_state -> player -> int -> bool list

(*[trade_h p1 p2 item] updates the mutable cards field of [p1] and [p2] so
  [item] is added to [p2]s cards and removed from [p1]s cards *)
val trade_h: (player -> player -> card option -> card option -> unit)

(*[has_item p c] returns true if [c] is in [p]'s card list *)
val has_item : (player -> card -> bool)

(*[execute_tile_end_function gs p] executes the end function of the tile of
  [p]s current room position if the player has not already exited that room *)
val execute_tile_end_function : game_state -> player -> unit

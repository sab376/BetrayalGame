exception Unimplemented
exception Out_of_things

type modifier = Event | Item | Omen of int
type area = Basement | Ground | Upper
type direction = Left | Right | Up | Down
type stat = Sanity | Knowledge | Might | Speed
type 'a ioption = INone | ISome of 'a | Invalid
type welcome = Start | Help | Quit

(*[type point] is used to place in xy coordinates on screen *)
type point = {
  x : int;
  y : int;
}

(* [**] | [++] | [--] are point-by-point addition, multipication, and subtraction.
   For example, (1,2) ++ (3,4) = (4,6) *)
(*[***] | [///] are point-by-integer multipication and division. For example,
  (2,3) *** 2 = (4,6) *)
let ( ** ) (p1 : point) (p2 : point) = {x = p1.x * p2.x; y = p1.y * p2.y}
let ( ++ ) (p1 : point) (p2 : point) = {x = p1.x + p2.x; y = p1.y + p2.y}
let ( -- ) (p1 : point) (p2 : point) = {x = p1.x - p2.x; y = p1.y - p2.y}
let ( *** ) (p1 : point) (i : int) = {x = p1.x*i; y = p1.y*i}
let ( /// ) (p1 : point) (i : int) = {x = p1.x/i; y = p1.y/i}

(*[drop lst n] returns [lst] with first [n] elements removed *)
let rec drop lst n =
  if n <= 0 then lst else
  match lst with
  | [] -> []
  | h :: t -> drop t (n-1)

(*[take lst n] returns the first [n] elements of [lst] *)
let rec take lst n =
  if n <= 0 then [] else
  match lst with
  | [] -> []
  | h :: t -> h :: take t (n-1)

(*[int_to_point i] converts 0..3 into xy coordinates of a direction (right, left,
  up, down) beginning at right and moving counter-clockwise.
  For example, [int_to_point 0] represents "right" and returns {x=1, y=0} *)
let int_to_point i =
  match i with
  | 0 -> {x = 1; y = 0} (* right *)
  | 1 -> {x = 0; y = -1} (* down *)
  | 2 -> {x = -1; y = 0} (* left *)
  | 3 -> {x = 0; y = 1} (* up *)
  | _ -> {x = 0; y = 0}

(*[int_to_direction i] converts 0..3 into a direction (right, left,
  up, down) beginning at right and moving counter-clockwise. *)
let int_to_direction i =
  match i with
  | 0 -> Right
  | 1 -> Down
  | 2 -> Left
  | 3 -> Up
  | _ -> failwith "precondition violated"

(*[direction_to_int dir] returns the int 0..3 that represents [dir]
beginning at right and moving counter-clockwise.*)
let direction_to_int dir =
  match dir with
  | Right -> 0
  | Down -> 1
  | Left -> 2
  | Up -> 3

(*[direction_to_opposite dir] returns the direction opposite [dir]. For example,
  [direction_to_opposite Down] return Up *)
let direction_to_opposite dir =
  match dir with
  | Right -> Left
  | Down -> Up
  | Left -> Right
  | Up -> Down

(*[char_to_direction c] returns the direction *)
let char_to_direction c =
  match c with
  | 'w' -> Some Up
  | 'a' -> Some Left
  | 's' -> Some Down
  | 'd' -> Some Right
  | _ -> None

let string_of_stat_type stat =
  match stat with
  | Might -> "Might"
  | Speed -> "Speed"
  | Knowledge -> "Knowledge"
  | Sanity -> "Sanity"

let print_intlist lst =
  print_string "\n[";
  let rec helper lst =
    match lst with
    | [] -> print_string "]"
    | h :: [] -> print_string (string_of_int h); helper []
    | h :: t -> print_string (string_of_int h ^ "; "); helper t in
  helper lst

let rec print_bool_lst = function
  | [] -> ""
  | h::t -> string_of_bool h ^ " " ^print_bool_lst t

(* [type character] represents the static character data and stats that players
   can choose to play as
 * [sanity|knowledge|might|speed] are the preset stat lists for this character,
   with initial values also included as properties *)
type character = {
  name : string;
  desc : string;
  sanity : int list;
  knowledge : int list;
  might : int list;
  speed : int list;
  sanity_initial : int;
  knowledge_initial : int;
  might_initial : int;
  speed_initial : int;
}

(* [type player] represents a controllable player
 * [character] is the static character with base stats
 * [sanity|knowledge|might|speed_index] represent the index of a player's stat
   in respect to their character's corresponding stat list
 * [alive] tracks whether player is still in game
 * [cards] are the cards in players possession
 * [team_id] tracks teams during haunt, while not in haunt default team is 0
 * [special_rooms] tracks rooms the player has been in, so that one time Effects
      don't happen again after re-entry into the room *)
and player = {
  mutable character : character;
  mutable sanity_index : int;
  mutable knowledge_index : int;
  mutable might_index : int;
  mutable speed_index : int;
  mutable alive : bool;
  mutable cards : card list;
  mutable current_room : room;
  mutable team_id : int;
  mutable special_rooms : int list;
}

(* [type card] represents the 'cards' in the game, of type Event, Item or Omen.
 * [modifier] is the aforementioned type
 * [weapon] is if card can be used as a weapon
 * [damage] is damage done if card is weapon
 * [consumable] marks the card as single time use
 * [range] tracks range of card
 * [ownable] describes whether this card can be picked up by players
 * [usable] describes whether this card can be used by players
 * [draw|use_function|passive] are called when this card is first drawn, used during
   a player's turn, or every turn it is owned, respectively *)
and card = {
  card_name : string;
  card_desc : string;
  card_modifier : modifier;
  weapon : bool;
  damage : int;
  consumable : bool;
  range : int;
  ownable : bool;
  usable : bool;
  draw_function : game_state -> unit;
  use_function : game_state -> unit;
  passive_function : game_state -> unit;
}

(* [type tile] represents the static deck of tiles used in the game
 * [modifier] represents the type of card to be drawn when first placing this room
 * [areas] represents a list of areas where this tile can be placed on the map
 * [left|right|up|down] are bools representing whether this direction has a door
 * [enter|exit|end_function] are called when a player enters this tile, exits
   this tile, or ends their room in this tile respectively *)
and tile = {
  tile_name : string;
  tile_name_abbrev : string;
  tile_desc : string;
  tile_modifier : modifier option;
  areas : area list;
  tile_left : bool;
  tile_right : bool;
  tile_up : bool;
  tile_down : bool;
  enter_function : game_state -> unit;
  exit_function : game_state -> unit;
  end_function : game_state -> unit;
}

(* [type room] represents the pieces of the game board that have been placed.
   updated to reflect tile rotation
 * [id] is the int id of the room
 * [tile] is the static tile this room represents
 * [orientation] is the orientation of this room
 * [left|right|up|down] are options that point to adjacent rooms*)
and room = {
  id : int;
  tile : tile;
  orientation : int;
  area : area;
  mutable left : room ioption;
  mutable right : room ioption;
  mutable up : room ioption;
  mutable down : room ioption;
}

(* [type game_state] represents the overarching state of the game that will be
   constantly passed forward to progress the game
   [initialized] determines whether the opening screen or game board is rendered
   [tile_deck | card_deck] is a randomized set of the static tile and card data as in
   use in current game
   [current_player_index] represents the player whose turn it is as index in list of players
   [start_room] used to render the board as a 4-way linked list
   [adding_info] is used in rendering tile rotaion
   [haunt_activated | haunt_counter] track whether the haunt is currently occuring
      and current role count needed to activate haunt
   [round] represents the number of complete rounds defined as each alive player taking
   a turn
   [win_cond] is rules needed to win in current haunt if haunt activated
   [winner] is an option representing the winner of game (None for most of the game)
   [force_end_turn] determines whether the current player has run out of moves
   [start_round_fun]
   [start_turn_fun]*)
and game_state = {
  (*variables : game_state; *)
  mutable players : player list;
  mutable initialized : bool;
  mutable tile_deck : tile list;
  mutable card_deck : card list;
  mutable current_player_index : int;
  start_room : room;
  mutable adding_info : (room * room * int) option;
  mutable haunt_activated : bool;
  mutable haunt_counter : int;
  mutable haunt : haunt option;
  mutable round : int;
  mutable winner : int option;
  mutable force_end_turn : bool;
  mutable start_round_fun : game_state -> unit;
  mutable start_turn_fun : game_state -> unit;
  mutable on_died_fun : game_state -> player -> card option -> unit;
  mutable display_teams : bool;
  (* map : BoardModule.t; *)
}

(*[type haunt] stores the static data to change game play when a "haunt is
  activated" in the game.
  [haunt_rules] describes the changes for players and is displayed
  [teams] function changes team_id of players
  [win_condition] returns the winning team, if any, from a given game state
  [rules] changes game_state to change game play. This often includes changing
  players stats, names, and abilites.
*)
and haunt = {
  haunt_name : string;
  haunt_rules : string;
  teams : game_state -> unit;
  win_condition : game_state -> int option;
  win_text : int -> string;
  rules : game_state -> unit
}

(*returns the player whose turn it is in [gs] *)
let curr_plyr (state : game_state) =
  let ind = state.current_player_index in
  List.nth state.players ind

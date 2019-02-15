open Data
open Dicefile

let new_player ch st_rm = {
  character = ch;
  sanity_index = ch.sanity_initial;
  knowledge_index = ch.knowledge_initial;
  might_index = ch.might_initial;
  speed_index = ch.speed_initial;
  alive = true;
  cards = [];
  current_room = st_rm;
  special_rooms = [];
  team_id = 0;
}

let execute_tile_end_function state plyr =
  match List.find_opt (fun id -> id = plyr.current_room.id)
          plyr.special_rooms with
  | Some _ -> ()
  | None -> (
      plyr.special_rooms <- (plyr.current_room.id :: plyr.special_rooms);
      plyr.current_room.tile.end_function state
    )

let get_player_from_char plyrs c =
  let rec helper lst i =
    match lst with
    | [] -> None
    | h :: t ->
      if h.character = c then Some i else helper t (i+1) in
  helper plyrs 0

(*[get_stat_value p s] is current value of [s] for [p]*)
let get_stat_value p s =
  match s with
  | Sanity -> List.nth p.character.sanity p.sanity_index
  | Knowledge -> List.nth p.character.knowledge p.knowledge_index
  | Might -> List.nth p.character.might p.might_index
  | Speed -> List.nth p.character.speed p.speed_index

(*[get_stat_index plyr stat] is current index of [stat] in [plyr]*)
let get_stat_index plyr stat =
  match stat with
  | Might -> plyr.might_index
  | Speed -> plyr.speed_index
  | Knowledge -> plyr.knowledge_index
  | Sanity -> plyr.sanity_index

(*[get_stat_list plyr stat] is int list representing values of [stat] for
  the character of [plyr] *)
let get_stat_list plyr stat =
  match stat with
  | Might -> plyr.character.might
  | Speed -> plyr.character.speed
  | Knowledge -> plyr.character.knowledge
  | Sanity -> plyr.character.sanity


let get_all_sts p =
  List.map (fun x -> get_stat_value p x) [Sanity; Knowledge; Might; Speed]

(*[set_stat p s stat] changes [stat] index of [p] to [s]
  requires: [s] is within bounds of [stat] list*)
let set_stat p s stat =
  match stat with
  |Sanity -> p.sanity_index <- s
  |Knowledge -> p.knowledge_index <- s
  |Might -> p.might_index <- s
  |Speed -> p.speed_index <- s

(* [can_increase_stat p stat] is true if [stat] can be increased for [p], false
   otherwise *)
let can_increase_stat p stat =
  let index = get_stat_index p stat in
  let len = List.length (get_stat_list p stat) in
  if index >= len - 1 then false
  else true

(*[can_change_stat p stat] is true if [stat] in [p] greater than 0, false otherwise*)
let can_change_stat p stat =
  let index = get_stat_index p stat in
  if index <= 0  then false
  else true

let change_stat p (game: game_state) i (stat: stat) : unit =
  let s = get_stat_index p stat in
  let s' = s + i in
  if s' < 0 then if game.haunt_activated then p.alive <- false else
      set_stat p 0 stat
  else if s' > 7 then set_stat p 7 stat
  else set_stat p s' stat

let can_interact_bool_list gs p (same_t: bool) =
  let cur_room = p.current_room.id in
  let pl_lst = gs.players in
  List.map(fun player ->
      let team_check =
        match same_t with
        |true -> p.team_id <> player.team_id
        |_ -> true
      in
      player.current_room.id = cur_room && (p.character <> player.character) &&
      player.alive && team_check
    ) pl_lst

let can_interact gs p same_t : player list =
  let cur_room = p.current_room.id in
  let pl_lst = gs.players in
  List.filter(fun player ->
    let team_check =
      match same_t with
      |true -> p.team_id <> player.team_id
      |false -> (List.length player.cards) <> 0
    in
    (player.current_room.id) = cur_room && player.alive && team_check
     && player.character <> p.character
  ) pl_lst

(*[get_player_index gs p] is index of [p] in [gs] player list*)
let get_player_index gs p=
  let rec loop lst p n =
    if n >3 then None
    else match lst with
      |h::t -> if h == p then Some n else loop t p (n+1)
      |[] -> None
  in let res = loop gs.players p 0 in
  match res with
  | Some i -> i
  | None -> 0

(*[players_in_room gs r] returns a list of players in room [r] in [gs]*)
let players_in_room gs r =
  let pl_lst = gs.players in
  List.filter (fun player -> player.current_room == r) pl_lst

(*[room_in_direction r dir] returns the room in direction [dir] from room [r] or
  returns None if no room added yet or returns Invalid if no room can be added in
that direction because of a blocked exit *)
let room_in_direction r dir =
  match dir with
  | Up -> r.up
  | Right -> r.right
  | Down -> r.down
  | Left -> r.left

(*[string_of_bool_list lst] converts the bool list to a list of strings of those
bools *)
let rec string_of_bool_list lst =
  match lst with
  | [] -> ""
  | h::t -> string_of_bool h ^ " " ^ string_of_bool_list t

(*[string_of_plyr_list lst] converts the player list to a list of strings of those
  players *)
let rec string_of_plyr_list lst =
  match lst with
  |[] -> ""
  | h::t -> h.character.name ^ " " ^ string_of_plyr_list t


let can_attack_range gs p (range:int) =
  let cur_room = p.current_room in
  let rec search room range dir p_list =
    if (range = 0) then (can_interact gs p true) @ p_list
    else
    match room_in_direction room dir with
      | ISome room ->
        let plyrs = (List.filter
                       (fun plyr ->
                          plyr.current_room == room && plyr.alive
                          && plyr.team_id <> p.team_id)
                       gs.players) in
        search room (range - 1) dir (plyrs@p_list)
      | _ -> p_list in
  let in_room = can_interact gs p true in
  let plyr_list = List.map (fun dir -> search cur_room range dir [])
      [Right; Down; Left; Up] in (List.flatten plyr_list) @ in_room

let can_attack_range_bool gs p (range:int) =
  let attackable = can_attack_range gs p range in
  let plyrs = gs.players in
  let fin = List.map (fun player -> List.mem player attackable) plyrs in
  fin

(*[filt_one item lst] returns [lst] with the first copy of [item] removed *)
let filt_one item lst =
  let rec filt_one_h item lst acc =
    match lst with
    | [] -> acc
    | h :: t when h.card_name = item.card_name -> (List.rev acc) @ t
    | h :: t -> filt_one_h item t (h :: acc) in
  filt_one_h item lst []

(* [filt_one' item lst] filters [item] from [lst] at most once *)
let filt_one' item lst =
  let spl = List.partition (fun x -> x.card_name = item.card_name) lst in
  match spl with
  |(its, nots) -> match its with
    |[] -> nots
    |h :: t -> t @ nots

(* [filt_by_name name lst] filters [lst] for [name]*)
let filt_by_name name lst =
    let rec filt_one_h_n name lst acc =
      match lst with
      |[] -> acc
      |h :: t when h.card_name = name -> t @ acc
      |h :: t -> filt_one_h_n name t (h :: acc) in
    filt_one_h_n name lst []

let trade_h p1 p2 (p1_offer : card option) (p2_offer : card option) =
  (match p1_offer with
  | None -> ()
  | Some card ->
    p1.cards <- filt_one card p1.cards;
    p2.cards <- card :: p2.cards);
  (match p2_offer with
  | None -> ()
  | Some card ->
    p2.cards <- filt_one card p2.cards;
    p1.cards <- card :: p1.cards)

(*[move_in_board p r] changes [p]s current room field to [r] *)
let move_in_board p r =
  p.current_room <- r

let has_item p i =
  List.mem i p.cards

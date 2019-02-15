open Data
open OUnit2
open Boardfile
open Staticdata
open Players


let rlud_tile = {
  tile_name = "Name";
  tile_name_abbrev = "Name";
  tile_desc = "Desc";
  tile_modifier = None;
  areas = [Ground];
  tile_left = true;
  tile_right = true;
  tile_up = true;
  tile_down = true;
  enter_function = (fun x -> ());
  exit_function = (fun x -> ());
  end_function = (fun x -> ());
}

let rl_tile = {
  rlud_tile with
  tile_up = false;
  tile_down = false;
}

let ru_tile = {
  rlud_tile with
  tile_left = false;
  tile_down = false;
}

let rd_tile = {
  rlud_tile with
  tile_up = false;
  tile_left = false;
}

let assert_equal_tiles t1 t2 =
  assert_equal t1.tile_name t2.tile_name;
  assert_equal t1.tile_desc t2.tile_desc;
  assert_equal t1.tile_modifier t2.tile_modifier;
  assert_equal t1.areas t2.areas;
  assert_equal t1.tile_left t2.tile_left;
  assert_equal t1.tile_right t2.tile_right;
  assert_equal t1.tile_up t2.tile_up;
  assert_equal t1.tile_down t2.tile_down

let assert_equal_room_opts r1' r2' =
  match (r1', r2') with
  | (ISome r1, ISome r2) -> assert_equal (r1 == r2) true
  | _ -> assert_equal true false

let assert_equal_rooms_not_rotation r1 r2 =
  assert_equal r1.tile r2.tile;
  assert_equal r1.area r2.area;
  assert_equal r1.left r2.left;
  assert_equal r1.right r2.right;
  assert_equal r1.up r2.up;
  assert_equal r1.down r2.down

let til n = List.nth all_tiles n
let base = create_room (til 0) 0 Ground

let test_set_map = [
  "create_room single" >:: (fun _ ->
      let rm = create_room rlud_tile 0 Ground in
      assert_equal_tiles rm.tile rlud_tile
    );
  "single_exits" >:: (fun _ ->
      let rm = create_room rlud_tile 0 Ground in
      assert_equal (List.map
                      (fun x -> get_exit_from_dir rm x) [Up; Down; Left; Right])
        [INone; INone; INone; INone];
    );
  (*"create_room append_right" >:: (fun _ ->
      let main = create_room rlud_tile 0 Ground in
      let right = temp_expand main rl_tile 0 Right in
      assert_equal_room_opts main.right (ISome right);
      assert_equal_room_opts right.left (ISome main)
    );*)
  "create rotate" >:: (fun _ ->
      let rl = create_room rl_tile 0 Ground in
      let rl' = create_room rl_tile 2 Ground in
      assert_equal_tiles rl.tile rl'.tile;
    );

  "create rotate_exits" >:: (fun _ ->
      let rd = create_room rd_tile 1 Ground in
      assert_equal rd.up Invalid;
      assert_equal rd.down INone;
      assert_equal rd.right Invalid;
      assert_equal rd.left INone;
    );

  "available_orient all" >:: (fun _ ->
      assert_equal (available_orientations Up rlud_tile) [0;1;2;3]
    );
  "avail orient rl" >:: (fun _ ->
      assert_equal (available_orientations Up rl_tile) [1;3]
    );
  "avail orient rd" >:: (fun _ ->
      assert_equal (available_orientations Up rd_tile) [0; 1]
    );
  "rotate_room" >:: (fun _ ->
      let rd = (create_room rd_tile 0 Ground) in
      assert_equal rd.orientation 0;
      let rd = rotate_room rd 1 in
      assert_equal rd.orientation 1;
      let rd = rotate_room rd 1 in
      assert_equal rd.orientation 2;
      let rd = rotate_room rd 1 in
      assert_equal rd.orientation 3;
      let rd = rotate_room rd 1 in
      assert_equal rd.orientation 0
    );
]

let default_card : card = {
  card_name = "";
  card_desc = "";
  card_modifier = Event;
  weapon = false;
  damage = 0;
  range = 0;
  consumable = false;
  ownable = false;
  usable = false;
  draw_function = (fun x -> ());
  use_function = (fun x -> ());
  passive_function = (fun x -> ());
}

let pale_ale = {
  default_card with
  card_name = "Pale ale";
  card_desc = "Drink this to and roll 1 die to lose knowledge";
  consumable = true;
  ownable = true;
  usable = true;
  card_modifier = Item;
}

let hatchet = {
  default_card with
  card_name = "hatchet";
  card_desc = "Drink this to and roll 1 die to lose knowledge";
  consumable = true;
  ownable = true;
  usable = true;
  card_modifier = Item;
}

let nate1 = {
  name = "Nate Foster";
  desc = (
    "My research uses ideas from programming \n"^
    "languages to solve problems in networking,\n"^
    "databases, and security. Some topics of\n"^
    "interest include semantics, type systems,\n"^
    "bidirectional languages, data\n"^
    "synchronization, and mechanized proof.\n"^
    "Recently I spend most of my time thinking\n"^
    "about network programming."
  );
  sanity = [1;3;3;4;5;5;6;7];
  knowledge = [4;5;5;5;5;6;7;8];
  might = [2;2;3;4;5;5;6;6];
  speed = [2;2;4;4;5;5;6;6];
  sanity_initial = 2;
  knowledge_initial = 4;
  might_initial = 2;
  speed_initial = 5;
}

let nate2 = {
  name = "Nate Foster";
  desc =
    "My research uses ideas from programming \n"^
    "languages to solve problems in networking,\n"^
    "databases, and security. Some topics of\n"^
    "interest include semantics, type systems,\n"^
    "bidirectional languages, data\n"^
    "synchronization, and mechanized proof.\n"^
    "Recently I spend most of my time thinking\n"^
    "about network programming.";
  sanity = [1;3;3;4;5;5;6;7];
  knowledge = [4;5;5;5;5;6;7;8];
  might = [2;2;3;4;5;5;6;6];
  speed = [2;2;4;4;5;5;6;6];
  sanity_initial = 2;
  knowledge_initial = 4;
  might_initial = 2;
  speed_initial = 5;
}

let dc = {
  name = "N";
  desc = "";
  sanity = [1;3;3;4;5;5;6;7];
  knowledge = [4;5;5;5;5;6;7;8];
  might = [2;2;3;4;5;5;6;6];
  speed = [2;2;4;4;5;5;6;6];
  sanity_initial = 2;
  knowledge_initial = 4;
  might_initial = 2;
  speed_initial = 5;
}

type ps = {p1 : player; p2 : player; p3: player}

(* p1 : [hatchet],
   p2 : [pale_ale],
   p3 : [hatchet; hatchet; hatchet] *)
let test_plys () =
  let p1 = {
    character = nate1;
    sanity_index = 4;
    knowledge_index = 4;
    might_index = 4;
    speed_index = 4;
    alive = true;
    cards = [hatchet];
    current_room = base;
    team_id = 0;
    special_rooms = [];
  } in
  let p2 = {
    character = nate2;
    sanity_index = 4;
    knowledge_index = 4;
    might_index = 4;
    speed_index = 4;
    alive = true;
    cards = [pale_ale];
    current_room = base;
    team_id = 0;
    special_rooms = [];
  }
  in
  let p3 = {
    character = nate2;
    sanity_index = 4;
    knowledge_index = 4;
    might_index = 4;
    speed_index = 4;
    alive = true;
    cards = [hatchet; hatchet; hatchet];
    current_room = base;
    team_id = 0;
    special_rooms = [];
  } in {p1 = p1; p2 = p2; p3 = p3}

let print_stringlist lst =
  print_string "[";
  let rec helper lst =
    match lst with
    | [] -> print_string "]"
    | h :: [] -> print_string h; helper []
    | h :: t -> print_string (h ^ "; "); helper t in
  helper lst

let assert_eq_lists l1 l2 =
  let l1' = List.sort compare l1 in
  let l2' = List.sort compare l2 in
  assert_equal l1' l2'

let string_list_items p =
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (acc @ [h.card_name]) t
  in loop [] p.cards

let string_items p =
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (acc ^ " " ^ h.card_name) t
  in loop "" p.cards

let to_string_lst p =
  List.map (fun x -> x.card_name) p.cards

let trade_test_suite = [
  "trade_one_item" >:: (fun _ ->
      let plyrs = test_plys () in
      let p1 = plyrs.p1 in
      let p2 = plyrs.p2 in
      trade_h p1 p2 (Some hatchet) None;
      assert_eq_lists (to_string_lst p1) [];
      assert_eq_lists (to_string_lst p2) ["hatchet"; "Pale ale"]
    );

  "trade_one_item_other" >:: (fun _ ->
      let plyrs = test_plys () in
      let p1 = plyrs.p1 in
      let p2 = plyrs.p2 in
      trade_h p1 p2 None (Some pale_ale);
      assert_eq_lists (to_string_lst p1) ["hatchet"; "Pale ale"];
      assert_eq_lists (to_string_lst p2) []
    );

  "trade_dups" >:: (fun _ ->
      let plyrs = test_plys () in
      let p3 = plyrs.p3 in
      let p2 = plyrs.p2 in
      trade_h p3 p2 (Some hatchet) None;
      assert_eq_lists (to_string_lst p3) ["hatchet"; "hatchet"];
      assert_eq_lists (to_string_lst p2) ["hatchet"; "Pale ale"];
    );

  "trade_two_way" >:: (fun _ ->
      let plyrs = test_plys () in
      let p2 = plyrs.p2 in
      let p1 = plyrs.p1 in
      trade_h p2 p1 (Some pale_ale) (Some hatchet);
      assert_eq_lists (to_string_lst p2) ["hatchet"];
      assert_eq_lists (to_string_lst p1) ["Pale ale"]
    );

  "trade_nothing" >:: (fun _ ->
      let plyrs = test_plys () in
      let p2 = plyrs.p2 in
      let p1 = plyrs.p1 in
      trade_h p2 p1 None None;
      assert_eq_lists (to_string_lst p2) ["Pale ale"];
      assert_eq_lists (to_string_lst p1) ["hatchet"]
    )
]
let assert_equal_players p1 p2 =
  assert_equal p1.character.name p2.character.name;
  assert_equal p1.sanity_index p2.sanity_index;
  assert_equal p1.knowledge_index p2.knowledge_index;
  assert_equal p1.might_index p2.might_index;
  assert_equal p1.speed_index p2.speed_index;
  assert_equal p1.alive p2.alive;
  assert_equal (List.length p1.cards) (List.length p2.cards);
  assert_equal p1.current_room.tile.tile_name p2.current_room.tile.tile_name;
  assert_equal p1.team_id p2.team_id;
  assert_equal (List.length p1.special_rooms) (List.length p2.special_rooms)

let default_haunt = {
  haunt_name = "default";
  haunt_rules = "No rules";
  win_condition = (fun x -> None);
  win_text = (fun team_id ->
      "Team "^string_of_int team_id^" has won the game!"
    );
  rules = (fun x -> ());
  teams = (fun st -> let pl = curr_plyr st in pl.team_id <-1);
}

let test_set_plyrs = [
  "update_plyr_single_stat_index" >:: (fun _ ->
      let (null_pl : player) = new_player dc base in
      let gs : game_state = {
        players = [null_pl];
        initialized = false;
        tile_deck = Deckfile.tiles;
        card_deck = Deckfile.cards;
        current_player_index = 0;
        start_room = base;
        adding_info = None;
        haunt_activated = false;
        haunt_counter = 20;
        round = 0;
        winner = None;
        force_end_turn = false;
        start_round_fun = (fun st -> ());
        start_turn_fun = (fun st -> ());
        on_died_fun = (fun st item c -> ());
        haunt = Some default_haunt;
        display_teams = true;
      } in
      let _ = change_stat null_pl gs 1 Knowledge in
      let p1 = List.nth gs.players 0 in
      let p2 = (new_player dc base) in
      let p2' = {p2 with knowledge_index = 5}
      in assert_equal_players p1 p2'
    );
  "update_plyr_single_stat_vals" >:: (fun _ ->
      let (null_pl : player) = new_player dc base in
      let gs : game_state = {
        players = [null_pl];
        initialized = false;
        haunt = Some default_haunt;
        tile_deck = Deckfile.tiles;
        card_deck = Deckfile.cards;
        current_player_index = 0;
        start_room = base;
        adding_info = None;
        haunt_activated = false;
        haunt_counter = 20;
        round = 0;
        winner = None;
        force_end_turn = false;
        start_round_fun = (fun st -> ());
        start_turn_fun = (fun st -> ());
        on_died_fun = (fun st item c -> ());
        display_teams = true;
      } in
      let _ = change_stat null_pl gs 1 Knowledge in
      let p1 = List.nth gs.players 0 in
      assert_equal (get_stat_value p1 Knowledge) 6;
      assert_equal (get_stat_index p1 Knowledge) 5
    );
  "item_not_have" >:: (fun _ ->
      let pl = new_player dc base
      in assert_equal (has_item pl pale_ale ) false);
]

let _ = run_test_tt_main ("Map Test Suite" >::: test_set_plyrs @ test_set_map @ trade_test_suite)

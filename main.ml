open Data
open Boardfile
open Graphics
open Staticdata
open Players
open Deckfile
open Dicefile
open Command
open Hauntfile

let start_room = create_room (List.hd all_tiles) 0 Ground

let global_state : game_state = {
  players = [];
  initialized = false;
  tile_deck = Deckfile.tiles;
  card_deck = Deckfile.cards;
  current_player_index = 0;
  start_room = start_room;
  adding_info = None;
  haunt_activated = false;
  haunt_counter = 0;
  round = 0;
  haunt = None;
  winner = None;
  force_end_turn = false;
  start_round_fun = (fun st -> ());
  start_turn_fun = (fun st -> ());
  on_died_fun = (fun st plyr item -> ());
  display_teams = false;
}

module ButtonHandler = Uifile.ButtonHandler

type action = Attack | Item | Move | EndTurn | Trade
type confirm = Yes | CounterOffer | EndTrade

(* Used to initialize welcome Buttons *)
let welcome_buttons = ButtonHandler.empty
    {x = Uifile.screen_size.x/2; y = Uifile.screen_size.y/2}
    {x = 200; y = 60} 16 |>
                      ButtonHandler.add "Help" "Help" Uifile.blue |>
                      ButtonHandler.add "Start" "Start Game" Graphics.yellow |>
                      ButtonHandler.add "Quit" "Quit Game" Graphics.red

let decide_list = [Yes; CounterOffer; EndTrade]

let decide_buttons = ButtonHandler.empty
    {x = Uifile.screen_size.x / 2; y = Uifile.screen_size.y /2}
    {x = 100; y = 60} 16 |> ButtonHandler.add Yes "Yes" Graphics.green |>
                     ButtonHandler.add CounterOffer "CounterOffer" Uifile.blue |>
                     ButtonHandler.add EndTrade "EndTrade" Graphics.red

let action_buttons = ButtonHandler.empty
    {x = Uifile.screen_size.x/2; y = 40}
    {x = 120; y = 40} 16 |>
                     ButtonHandler.add Attack "Attack" Graphics.red |>
                     ButtonHandler.add Item "Item" Graphics.yellow |>
                     ButtonHandler.add Move "Move" Uifile.blue |>
                     ButtonHandler.add Trade "Trade" Graphics.magenta |>
                     ButtonHandler.add EndTurn "End Turn" Graphics.green

let stat_buttons = ButtonHandler.empty
    {x = Uifile.screen_size.x/2; y = Uifile.screen_size.y*3/4}
    {x = 80; y = 40} 16 |>
                   ButtonHandler.add Might "Might" Graphics.red |>
                   ButtonHandler.add Speed "Speed" Uifile.blue |>
                   ButtonHandler.add Knowledge "Knowledge" Graphics.green |>
                   ButtonHandler.add Sanity "Sanity" Graphics.yellow

let cancel_button = ButtonHandler.empty
    {x = Uifile.screen_size.x/2; y = Uifile.screen_size.y/4 - 60}
    {x = 120; y = 40} 16 |>
                    ButtonHandler.add 0 "Cancel" Graphics.red

let player_buttons : (int ButtonHandler.t ref) =
  ref (ButtonHandler.empty
         {x = Uifile.screen_size.x/2; y = Uifile.screen_size.y*3/4}
         {x = 100; y = 40} 16)

let char_buttons = Uifile.create_char_buttons ()

let rec make_card_buttons ?col:(col = Graphics.yellow) cards =
  let buttons : (int ButtonHandler.t ref) =
    ref (ButtonHandler.empty
           {x = Uifile.screen_size.x/2; y = Uifile.screen_size.y/4}
           {x = 120; y = 40} 16) in
  List.iteri (fun i card ->
      buttons := !buttons |> ButtonHandler.add i card.card_name col
    ) cards;
  !buttons

let index_of_find f lst =
  let rec find index lst = (
    match lst with
    | [] -> failwith "index_of_find Index not found"
    | h :: t ->
      if f h then index else find (index + 1) t
  ) in find 0 lst

(* [stat_type_to_int stat] is an integer representing [stat] *)
let stat_type_to_int stat =
  match stat with
  | Might -> 0
  | Speed -> 1
  | Knowledge -> 2
  | Sanity -> 3

(*[int_to_stat_type i] is the stat represented by [i]*)
let int_to_stat_type i =
  match i with
  | 0 -> Might
  | 1 -> Speed
  | 2 -> Knowledge
  | 3 -> Sanity
  | _ -> failwith "Invalid int for stat type"

(* Prompts the user with the selection of buttons in accordance with the ButtonHandler
   passed in.
   [enabled_list] determines which buttons may be clicked.
   [text] determines what text is display via popup message
   [can_cancel] is a bool that, if true, allows the user to cancel instead of selecting a button
   Returns:
   INone if there are no enabled buttons to press
   ISome 'a of the 'a of the button clicked by the user
   Invalid if can_cancel is true and the user clicked cancel *)
let prompt_button_choice (enabled_list : bool list) (text : string) (button_set : 'a ButtonHandler.t) (can_cancel : bool) : 'a ioption =
  let avail_count = List.fold_left (fun acc x -> acc + if x then 1 else 0) 0 enabled_list in
  if avail_count = 0 && not can_cancel then INone else
    (*if avail_count = 1 then Some (button_set.fetch_index (index_of_find (fun x -> x) enabled_list)) else*) (
    List.iteri (fun i b -> ButtonHandler.set_enabled_index i b button_set) enabled_list;
    let rec prompt_repl () =
      if (global_state.initialized) then Uifile.render_game global_state else ();
      ButtonHandler.render button_set;
      (if can_cancel then ButtonHandler.render cancel_button);
      set_color white;
      Uifile.render_popup_text text "";
      let state = wait_next_event [Button_down;Key_pressed] in
      if state.button then (
        if can_cancel &&
           (match ButtonHandler.mouse_input (state.mouse_x, state.mouse_y) cancel_button with
            | Some id -> true
            | _ -> false) then
          Invalid
        else
          match ButtonHandler.mouse_input (state.mouse_x, state.mouse_y) button_set with
          | Some id ->
            ISome id
          | _ -> prompt_repl ()
      ) else prompt_repl () in
    prompt_repl ()
  )

(* [welcome_repl ()] initializes the welcome buttons, progressing the game if
   start selected, or exiting if "exit" chosen*)
let rec welcome_repl () =
  set_color Graphics.black;
  fill_rect 0 0 10000 10000;
  Uifile.render_popup_text "Welcome to betrayal!" "";
  let click = prompt_button_choice [true; true] "Welcome to Betrayal" welcome_buttons false in
  match click with
  | ISome index -> (
      match index with
      | "Help" -> Uifile.render_popup_text "Welcome to Betrayal" help_text;
        wait_for_click ();
        welcome_repl ()
      | "Start" -> ()
      | _ -> exit 0
    )
  | _ -> welcome_repl ()

(* [choose_character_repl loc] implements character choice from the list in
   Staticdata. [loc] is the currently indexed location in the list
   Requires: loc > 0, and < length of characterlist*)
let rec choose_character_repl loc =
  set_color Graphics.black;
  fill_rect 0 0 10000 10000;
  let characters = Staticdata.all_characters in
  let num_chars = List.length characters in
  let player_list = global_state.players in
  let char_in_list c =
    match get_player_from_char player_list c with
    | Some i -> true
    | None -> false in
  (* [char_bool_list loc lst n] builds a list of selectable characters
     based on which have already been selected. *)
  let rec char_bool_list loc lst n =
    if n < 0 then lst else (
      let b = if loc >= num_chars then false else (
          let character = List.nth characters loc in
          not (char_in_list character)
        ) in
      char_bool_list (loc + 1) (b :: lst) (n-1)
    ) in
  let can_select =
    let can_prev = (loc <> 0) in
    let can_next = (loc < num_chars - 3) in
    let chars = List.rev (can_next :: (char_bool_list loc [] 2)) in
    can_prev :: chars in
  Uifile.render_char_selection (take (drop characters loc) 3) global_state;
  match prompt_button_choice can_select ("Player "^string_of_int(List.length player_list + 1)^" - Select a character") char_buttons false with
  | ISome i ->
    if i = 0 then
      choose_character_repl (max (loc - 3) 0)
    else if i = 4 then
      choose_character_repl (loc + 3)
    else
      (* i is between 1 and 3, a character was selected *)
      (List.nth characters (loc + i - 1), loc)
  | _ -> exit 0

let initiate_game_repl () =
  let player_count = 4 in
  let rec repeat f n =
    if n <= 0 then () else (
      f ();
      repeat f (n-1)
    )  in
  let loc_ref = ref 0 in
  repeat (fun () ->
      let (ch, new_loc) = choose_character_repl !loc_ref in
      loc_ref := new_loc;
      global_state.players <- global_state.players @ [new_player ch start_room]
    ) player_count;
  global_state.initialized <- true

let draw_tile attr =
  let (new_deck, next_tile) = TileDeck.next global_state.tile_deck attr in
  global_state.tile_deck <- (new_deck @ [next_tile]);
  next_tile

let draw_card attr =
  let (new_deck, next_card) = CardDeck.next global_state.card_deck attr in
  global_state.card_deck <- (new_deck @ [next_card]);
  next_card

let rec rotate_repl cur_room new_room exit_int oris index =
  global_state.adding_info <- Some (cur_room, new_room, exit_int);
  Uifile.render_game global_state;
  Uifile.render_popup_text "Press r to rotate room" "Click to confirm";
  let state = wait_next_event [Key_pressed; Button_up] in
  if state.keypressed then (
    match state.key with
    | 'r' ->
      let index' = (index + 1) mod (List.length oris) in
      let rotate_delta = (List.nth oris index') - new_room.orientation in
      let new_room' = rotate_room new_room rotate_delta in
      rotate_repl cur_room new_room' exit_int oris index'
    | _ -> rotate_repl cur_room new_room exit_int oris index
  ) else (
    global_state.adding_info <- None;
    new_room
  )


let rec lose_stat_repl p s damage item_used =
  if damage <= 0 then (
    Uifile.render_popup_text "No damage points left, click to continue" "";
    wait_for_click ()
  ) else
    let damage_abs = abs damage in
    let str = s ^ (string_of_int damage_abs) ^ ". Click a stat to decrease" in
    match prompt_button_choice [can_change_stat p Might; can_change_stat p Speed; false; false] str stat_buttons false with
    | INone | Invalid ->
      change_stat p global_state (-1) Might;
      if not p.alive then (
        Uifile.render_popup_text ("R.I.P. " ^ p.character.name)
          (p.character.name ^ " has met a bitter end.\n\nClick to continue");
        global_state.on_died_fun global_state p item_used;
        wait_for_click ()
      )
    | ISome stat ->
      change_stat p global_state (-1) stat;
      Uifile.render_game global_state;
      lose_stat_repl p (s) (damage_abs - 1) item_used

(*[attack_helper p1 p2] is the result of p1 attacking p2. positive if p1
  does more damage, negative if p2 does more.
  Effects: prints attacker and defender dice.*)
let attack_helper p1 p2 =
  let att = get_stat_value p1 Might in
  let def = get_stat_value p2 Might in
  let att_rolls = roll att in
  let tot_att = sum att_rolls in
  let def_rolls = roll def in
  let tot_def = sum def_rolls in
  Uifile.animated_draw_dice_with_string 0 50 att (p1.character.name ^ " Rolls: ") att_rolls;
  Uifile.animated_draw_dice_with_string 0 0 def (p2.character.name ^ " Rolls: ") def_rolls;
  Command.wait_for_click ();
  tot_att - tot_def

(*[attack_repl p] implements attacking for current player [p]*)
let rec attack_repl p =
  Uifile.render_game global_state;
  match prompt_button_choice (can_interact_bool_list global_state p true)
          "Select a player to attack" !player_buttons true with
  | INone | Invalid -> false
  | ISome index ->
    let p1 = p in
    let p2 = List.nth global_state.players index in
    let diff = attack_helper p1 p2 in
    if diff = 0 then (
      (Uifile.render_popup_text "Neither side takes damage, click to continue" "");
      wait_for_click ()
    ) else (
      Uifile.render_popup_text
        ((if diff < 0 then p1.character.name else p2.character.name)^" takes "^(string_of_int (abs diff))^" damage, click to continue") "";
      wait_for_click ();
      lose_stat_repl (if diff < 0 then p1 else p2) "Allocate damage points: " (abs diff) None
    );
    true

(*[make_trus len counter acc] returns a [len] length list of true *)
let rec make_trus len counter acc =
  match (len = counter) with
  |true -> acc
  |false -> make_trus len (counter +1) (true :: acc)

(*[offer_helper p] displays buttons of [p]'s items, prompts, and records the choice *)
let rec offer_helper p s =
  let p_color = List.nth Uifile.player_colors (get_player_index global_state p) in
  let card_buttons = make_card_buttons ~col:(p_color) p.cards |>
                     ButtonHandler.add (List.length p.cards) "Nothing" p_color
  in
  prompt_button_choice (make_trus ((List.length p.cards)+1) 0 [])
    (s) card_buttons false

(*prompts and records [p1]'s choices of items for a trade, then asks [p2] if
  they agree. Based on answer, either removes the item from [p1]s cards and adds
  to [p2]s cards, allows [p2] to make a counter offer, or
  ends trading by quitting repl*)
let rec offer_repl p1 p2 =
  let card_opt_to_string = function
    | Some card -> card.card_name
    | None -> "Nothing" in
  let none1 = List.length p1.cards in
  let none2 = List.length p2.cards in
  match offer_helper p1 (p1.character.name ^ " which item would you like to trade?" ) with
  | INone | Invalid -> ()
  | ISome ind1 ->
    match offer_helper p2 ("Choose one of " ^p2.character.name ^" items to trade for") with
    | INone | Invalid -> ()
    | ISome ind2 ->
      let p1_offer = (if ind1 = none1 then None else Some (List.nth p1.cards ind1)) in
      let p2_offer = (if ind2 = none2 then None else Some (List.nth p2.cards ind2)) in
      Uifile.render_popup_text "Offer"
        (p1.character.name ^ " would you like to trade "^
         (card_opt_to_string p1_offer) ^ " for your " ^ (card_opt_to_string p2_offer));
      match prompt_button_choice (make_trus 2 0 []) (p2.character.name ^ " do you accept this offer?")
              decide_buttons false with
      | INone | Invalid -> ()
      | ISome Yes -> trade_h p1 p2 p1_offer p2_offer
      | ISome CounterOffer -> offer_repl p2 p1
      | ISome EndTrade -> ()


(*displays buttons of players [p] can trade with, then begins
  offer repl to complete trade *)
let rec trade_repl p =
  match prompt_button_choice (can_interact_bool_list global_state p false)
          "Select a player to trade with" !player_buttons true with
  | INone | Invalid -> ()
  (*Uifile.render_popup_text "Can't Trade" "Sorry, no one in this room has an item to trade with you";
    wait_for_any global_state*)
  | ISome i ->
    let p1 = p in
    let p2 = List.nth global_state.players i in
    offer_repl p1 p2


let draw_card pl rm m =
  let card = draw_card m in
  Uifile.render_popup_text card.card_name card.card_desc;
  let _ = wait_for_click () in
  card.draw_function global_state;
  let pl = curr_plyr global_state in
  if card.ownable then pl.cards <- (card :: pl.cards);
  if not global_state.haunt_activated then (
    match m with
    | Omen _ ->
      let haunt_count = global_state.haunt_counter in
      (* Need to render haunt counter here *)
      Uifile.render_popup_text (string_of_int haunt_count)
        "If you roll less than the number shown the Haunt will begin";
      let roll_val = sum (roll_repl 7) in
      wait_for_click ();
      Uifile.render_game global_state;
      if roll_val >= haunt_count then (
        Uifile.render_popup_text "You are safe from the haunt" (string_of_int roll_val ^ " vs " ^ string_of_int haunt_count);
        global_state.haunt_counter <- global_state.haunt_counter + 1
      ) else (
        Uifile.render_popup_text "The haunt begins!" (string_of_int roll_val ^ " vs " ^ string_of_int haunt_count);
        activate_haunt global_state card;
        List.iteri (fun i p ->
            ButtonHandler.set_text i p.character.name !player_buttons
          ) global_state.players
      );
      wait_for_click ()
    | _ -> ()
  )

let check_items pl =
  List.map (fun x ->
      (not x.weapon || global_state.haunt_activated) && x.usable) pl.cards

(* [attack_at_distance player damage range] implements attacking using
   ranged items. Both [player] and selected defender role might, however, only
   defender can take damage. Ends immediately if no players are attackable within
   [range] of [player]*)
let rec attack_at_distance player damage (range:int) item_used =
  match prompt_button_choice (can_attack_range_bool global_state player range)
          "Select a player to attack" !player_buttons true with
  | INone | Invalid -> false (* no attackable players *)
  | ISome index ->
    let p1 = player in
    let p2 = List.nth global_state.players index in
    let diff = attack_helper p1 p2 in
    if diff>0 then lose_stat_repl p2 (p2.character.name ^ " you've been hit! ") damage item_used
    else Uifile.render_popup_text "Attack has been blocked" "";
    true

let rec use_item_repl plyr =
  (* Uifile.render_item_list items; *)
  let card_buttons = make_card_buttons plyr.cards in
  match prompt_button_choice (check_items plyr) "Select an item to use" card_buttons true with
  | INone | Invalid -> false
  | ISome index ->
    let item = List.nth plyr.cards index in
    if (item.weapon && not global_state.haunt_activated) then ()
    else
      (if item.consumable then
         plyr.cards <- List.filter (fun card -> item.card_name != card.card_name) plyr.cards);
    if item.weapon then
      attack_at_distance plyr item.damage item.range (Some item)
    else (
      item.use_function global_state;
      true
    )

let rec move_repl plyr speed =
  if global_state.force_end_turn then global_state.force_end_turn <- false else
  if not plyr.alive then () else (
    Uifile.render_game global_state;
    Uifile.render_popup_text plyr.current_room.tile.tile_name plyr.current_room.tile.tile_desc;
    ButtonHandler.set_enabled Attack ((can_interact global_state plyr true) <> []) action_buttons;
    (*ButtonHandler.set_enabled Item ((List.fold_left (fun acc b -> acc + if b then 1 else 0) 0 (check_items plyr)) > 0) action_buttons;*)
    ButtonHandler.set_enabled Trade (can_interact global_state plyr false <> []) action_buttons;
    ButtonHandler.set_enabled Move (speed > 0) action_buttons;
    ButtonHandler.set_text Move ("Move: ["^string_of_int speed^"]") action_buttons;
    ButtonHandler.render action_buttons;
    let state = wait_next_event [Button_down;Key_pressed] in
    if state.button then
      match ButtonHandler.mouse_input (state.mouse_x, state.mouse_y) action_buttons with
      | Some EndTurn -> ()
      | Some Trade ->
        trade_repl plyr;
        move_repl plyr speed
      | Some Attack ->
        if attack_repl plyr then () else
          move_repl plyr speed
      | Some Item ->
        if use_item_repl plyr then () else
          move_repl plyr speed
      | _ -> move_repl plyr speed
    else (
      match state.key with
      | 'q' -> exit 0
      | c ->
        if speed = 0 then move_repl plyr speed else
          match char_to_direction c with
          | Some dir ->
            if not (can_travel plyr.current_room dir) then move_repl plyr speed else (
              plyr.current_room.tile.exit_function(global_state);
              if global_state.force_end_turn then (
                global_state.force_end_turn <- false;
                move_repl plyr (speed -1)
              ) else
                match get_exit_from_dir plyr.current_room dir with
                | INone ->
                  let next_tile = draw_tile plyr.current_room.area in
                  let avil_oris = available_orientations dir next_tile in
                  let new_room = create_room next_tile (List.nth avil_oris 0) Ground in
                  let final_room = (
                    match avil_oris with
                    | [] -> failwith "that's bad"
                    | lst when List.length lst = 1 || List.length lst = 4 -> new_room
                    | [1;3] | [0;2] -> new_room
                    | _ -> rotate_repl plyr.current_room new_room (direction_to_int dir) avil_oris 0
                  ) in
                  plyr.current_room <- temp_expand plyr.current_room final_room dir;
                  plyr.current_room.tile.enter_function(global_state);
                  begin
                    match plyr.current_room.tile.tile_modifier with
                    | Some m ->
                      Uifile.render_popup_text "Entering room - draw card" "";
                      wait_for_any () ;
                      draw_card plyr plyr.current_room m;
                      if global_state.haunt_activated then
                        move_repl plyr (speed - 1)
                    | None -> move_repl plyr (speed - 1)
                  end
                | ISome next_room -> (*kasdf*)
                  plyr.current_room <- next_room;
                  plyr.current_room.tile.enter_function(global_state);
                  move_repl plyr (speed - 1)
                | Invalid -> failwith "Impossible"
            )
          | None -> move_repl plyr speed (*invalid action, try again*)
    )
  )

let win_action i =
  let win_text = match global_state.haunt with
    | Some haunt ->
      haunt.win_text i
    | None -> ("CONGRATS TEAM " ^ string_of_int i ^ "\n" ^
               "You successfully survived\n"^
               "Betrayal at the House on the Hill") in
  Uifile.render_popup_text "GAME OVER" win_text;
  wait_for_any ();
  Uifile.render_popup_text "Press 'q' to quit the game" "";
  let rec quit_repl () =
    let state = wait_next_event [Key_pressed] in
    match state.key with
    | 'q' -> exit 0
    | _ -> quit_repl () in
  quit_repl ()

let check_win st =
  (match st.haunt with
   | Some haunt -> (
       match haunt.win_condition st with
       | Some i ->
         st.winner <- Some i
       | None -> ()
     )
   | None -> ());
  match st.winner with
  | None -> ()
  | Some i -> win_action i


let rec player_turn_repl () =
  check_win global_state;
  let cur_player = curr_plyr global_state in
  move_repl cur_player (get_stat_value cur_player Speed);
  execute_tile_end_function global_state cur_player;
  global_state.current_player_index <- (global_state.current_player_index + 1) mod (List.length global_state.players);
  if (global_state.current_player_index = 0) then (
    global_state.round <- global_state.round + 1;
    global_state.start_round_fun global_state;
  );
  global_state.start_turn_fun global_state;
  Uifile.render_game global_state;
  player_turn_repl ()

let main () =
  Uifile.initialize_render ();
  welcome_repl ();
  initiate_game_repl ();
  List.iteri (fun i p ->
      player_buttons := !player_buttons |> ButtonHandler.add i p.character.name (List.nth Uifile.player_colors i)
    ) global_state.players;
  player_turn_repl ()


let _ = main ()

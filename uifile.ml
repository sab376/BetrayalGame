open Data
open Graphics
open Players

let room_size : point = {x = 50; y = 50}
let door_size : point = {x = 4; y = 6}
let room_padding : point = {x = 1; y = 1}
let screen_size : point = {x = 1250; y = 625}
let player_stat_size : point = {x = 144; y = 80}
let dice_point : point = {x = 50; y = 300}
let dice_text_point : point = {x = 0; y = 300}

let blue = rgb 104 182 255
let grey = rgb 100 100 100

let player_colors : int list = [Graphics.red; Graphics.green; blue; Graphics.yellow]
let player_radius : int = 5

type 'a button = {
  mutable text : string;
  identifier : 'a;
  color : int;
  mutable enabled : bool;
}

(* Returns true if the given x,y point is within the box of pos,size *)
let point_in_box x y pos size =
  (x >= pos.x && x <= pos.x + size.x) &&
  (y >= pos.y && y <= pos.y + size.y)

let initialize_render () =
  open_graph (" "^string_of_int(screen_size.x)^"x"^string_of_int(screen_size.y));
  set_window_title "Betrayal Game"

(* [text_fit_coords pt str] is tuple representing (x,y) coordinates that make [str]
   fit on screen given starting point [pt]*)
let text_fit_coords pt str =
   let (x, y) = text_size str in
   let temp_x = pt.x - x/2 in
   let x' = (if temp_x < 0 then 0
             else if (temp_x + x) > screen_size.x then (screen_size.x - x)
             else temp_x) in
   let temp_y = pt.y - y/2 in
   let y' = (if temp_y < 0 then 0
             else if (temp_y + y) > screen_size.y then (screen_size.y - y)
             else temp_y) in
   (x', y')

(* Draw fitted text *)
let draw_fitted_text pt str =
 let (x, y) = text_fit_coords pt str in
 moveto x y;
 draw_string str

(* Draws text [str] centered at point [pt] *)
let draw_centered_text pt str =
  let (x, y) = text_size str in
  moveto (pt.x - x/2) (pt.y - y/2);
  draw_string str

(* Draws text aligned in accordance with a scale offset from the bottom-left corner.
   0, 0 = bottom left
   1, 1 = top right *)
(* x_align and y_align are 0-1 values *)
let draw_aligned_text pt str x_align y_align =
  let (x, y) = text_size str in
  moveto
    (pt.x - int_of_float(float_of_int(x) *. x_align))
    (pt.y - int_of_float(float_of_int(y) *. y_align));
  draw_string str

(* Calls a draw function [f] on the centered adjustment of x y w h *)
let draw_centered f x y w h = f (x - w/2) (y - h/2) w h

(* render_popup_text with additional arguments of size, center, color *)
let render_popup_text_special size center title text col =
  set_color Graphics.black;
  draw_centered fill_rect center.x center.y size.x size.y;
  set_color col;
  draw_centered draw_rect center.x center.y size.x size.y;
  draw_aligned_text {x = center.x; y = center.y + size.y/2 - 4} title 0.5 1.0;
  let lines = String.split_on_char '\n' text in
  let total = List.length lines in
  List.iteri (fun i txt ->
      draw_centered_text {x = center.x; y = center.y - (i - total/2) * 16 - 16} txt
    ) lines

let render_popup_text title text =
  let size = {x = 400; y = (if text = "" then 20 else 150)} in
  let center = {x = screen_size.x/2; y = screen_size.y - size.y/2 - 8} in
  render_popup_text_special size center title text Graphics.white

(* Returns the corner of an index, where
   0 = top left
   1 = top right
   2 = bottom right
   3 = bottom left *)
let index_to_corner i =
  let ind = i mod 4 in
  match ind with
  | 0 -> {x = -1; y = 1}
  | 1 -> {x = 1; y = 1}
  | 2 -> {x = 1; y = -1}
  | _ -> {x = -1; y = -1}

(* draws any players currently residing in room [rm] *)
let draw_plyr_loc (rm : room) (pt : point) (state : game_state) =
  List.iteri (fun i p ->
      if p.current_room == rm then
        let corner = index_to_corner i in
        if i = state.current_player_index then (
          set_color Graphics.white;
          fill_circle
            (pt.x + corner.x * room_size.x/2 - corner.x * player_radius*2)
            (pt.y + corner.y * room_size.y/2 - corner.y * player_radius*2)
            (player_radius+2)
        );
        set_color (if p.alive then List.nth player_colors i else grey);
        fill_circle
          (pt.x + corner.x * room_size.x/2 - corner.x * player_radius*2)
          (pt.y + corner.y * room_size.y/2 - corner.y * player_radius*2)
          player_radius
      else ()
    ) state.players

(* Return true if room_opt is a valid exit, else false *)
let is_door room_opt =
  match room_opt with
  | INone | ISome _ -> true
  | Invalid -> false

(* Renders a single room *)
let render_room_only rm state pt col =
  if pt.x < -room_size.x || pt.y < -room_size.y
     || pt.x > screen_size.x + room_size.x
     || pt.y > screen_size.y + room_size.y then () else (
  set_color col;
  fill_rect (pt.x - room_size.x/2) (pt.y - room_size.y/2) room_size.x room_size.y;
  set_color Graphics.black;
  draw_rect (pt.x - room_size.x/2) (pt.y - room_size.y/2) room_size.x room_size.y;
  set_color Graphics.white;
  let tname_len = String.length (rm.tile.tile_name) in
  draw_centered_text pt (if tname_len > 5 then rm.tile.tile_name_abbrev else rm.tile.tile_name);
  set_color (rgb 245 245 220);
  if is_door rm.right then
    fill_rect (pt.x + room_size.x/2 - door_size.x) (pt.y - door_size.y/2) door_size.x door_size.y;
  if is_door rm.left then
    fill_rect (pt.x - room_size.x /2) (pt.y - door_size.y/2) door_size.x door_size.y;
  if is_door rm.down then
    fill_rect (pt.x - door_size.y/2) (pt.y - room_size.y/2) door_size.y door_size.x;
  if is_door rm.up then
    fill_rect (pt.x - door_size.y/2) (pt.y + room_size.y/2 - door_size.x) door_size.y door_size.x;
  draw_plyr_loc rm pt state
)

(* Renders the entire board, with [adding_info] being a room potentially being placed *)
let render_board start_rm plyrs adding_info =
  set_color Graphics.black;
  fill_rect 0 0 10000 10000;
  let (seen : int list ref) = ref [] in
  let rec draw (rm : room) (pt : point) =
    let _ = match adding_info with
      | Some (from_room, to_room, i) when from_room == rm ->
        let dir = int_to_point i in
        render_room_only to_room plyrs (pt ++ (dir ** (room_size ++ room_padding))) (rgb 203 163 152)
      (*draw to_room )*)
      | _ -> () in
    render_room_only rm plyrs pt (rgb 153 113 102);
    List.iteri (fun i (r' : room ioption) ->
        match r' with
        | ISome room when (List.find_opt (fun x -> room.id = x) !seen) = None ->
          let dir = int_to_point i in
          seen := (room.id :: !seen);
          draw room (pt ++ (dir ** (room_size ++ room_padding)))
        | _ -> ()
      ) [rm.right; rm.down; rm.left; rm.up] in
  draw start_rm (screen_size///2)

(* Draws a text bar at position [x, y] with width [w] [h]
   of all the elements of [lst] with index [highlighted] highlighted
   of color [col] *)
let draw_text_bar x y w h lst highlighted col =
  Graphics.set_color col;
  let len = List.length lst in
  List.iteri (fun i el ->
      if i = highlighted then (
        fill_rect (x + i * w/len) y (w/len) h;
        Graphics.set_color Graphics.black;
        draw_centered_text
          {x = (x + i * w/len + w/len/2); y = (y + h/2)}
          (string_of_int el);
        Graphics.set_color col
      ) else (
        draw_rect (x + i * w/len) y (w/len) h;
        draw_centered_text
          {x = (x + i * w/len + w/len/2); y = (y + h/2)}
          (string_of_int el)
      )
    ) lst

(* [string_of_stat st] is string representation of [st] *)
let string_of_stat st =
  match st with
  | Knowledge -> "Knowledge:"
  | Might -> "Might:"
  | Sanity -> "Sanity:"
  | Speed -> "Speed:"

(* Draws a die with a given int value *)
let draw_dice_h x y = function
  | 0 -> ()
  | 1 -> fill_circle (x+20) (y+20) 5
  | _ -> fill_circle (x+10) (y+10) 5;
    fill_circle (x+25) (y+25) 5

(* Draws multiple dice *)
let rec draw_dice x y (is: int list) =
  match is with
  | [] -> ()
  | h :: t -> begin
      set_color white;
      draw_rect x y 40 40;
      set_color black;
      fill_rect (x+1) (y+1) 38 38;
      set_color white;
      draw_dice_h x y h;
      draw_dice (x+40) (y) t
    end

(* Renders a character table of all their stats *)
let render_char name get_index get_list corner col px py w h =
  set_color col;
  draw_rect px py w h;
  List.iteri (fun i stat ->
      let stat_ind = get_index stat in
      let stat_list = get_list stat in
      draw_text_bar px (py + h/4*i) w (h/4) stat_list stat_ind col;
      draw_aligned_text {x = px + w + 4 - (1 + corner.x) * (w + 8)/2; y = py + h/4*i + h/8}
        (string_of_stat_type stat)
        ((float_of_int corner.x) /. 2.0 +. 0.5)
        0.5
    ) [Might; Speed; Knowledge; Sanity];
  draw_aligned_text
    {x = px + w / 2; y = py - 4 + (1 - corner.y) * (h + 8) / 2}
    name
    0.5
    ((float_of_int corner.y) /. 2.0 +. 0.5)

(* Renders all the players in the game, in the corners *)
let render_players state =
  let center = screen_size///2 in
  List.iteri (fun i p ->
      set_color Graphics.white;
      let corner = index_to_corner i in
      let xpos = (center.x + corner.x * center.x - corner.x * (player_stat_size.x/2 + 10) - (player_stat_size.x/2)) in
      let ypos = (center.y + corner.y * center.y - corner.y * (player_stat_size.y/2 + 10) - (player_stat_size.y/2)) in
      if i = state.current_player_index then (
        draw_rect (xpos-1) (ypos-1) (player_stat_size.x+2) (player_stat_size.y+2)
      );
      render_char
        (p.character.name ^ if state.display_teams then " [Team: "^string_of_int p.team_id^"]" else "")
        (get_stat_index p) (get_stat_list p) corner
        (if p.alive then List.nth player_colors i else grey)
        xpos ypos
        player_stat_size.x
        player_stat_size.y
    ) state.players

(* [draw_dice_with_string px py s (is: int list)] draws dice to the right of [s]
   with offset in the x and y direction of [px] and [py]*)
let draw_dice_with_string px py s (is: int list)=
draw_fitted_text {x = dice_text_point.x + px; y = dice_text_point.y + py} s;
let new_x = current_x () + 3 in
let new_y = current_y () - 12 in
  draw_dice new_x new_y is

(* ensures that a click must be registered before the code continues*)
let wait_for_click () =
  let _ = wait_next_event [Poll] in
  let _ = wait_next_event [Button_down] in
  let _ = wait_next_event [Button_up] in ()

(* [draw_dice_animated px py n roll] animates [n] dice placed at offset of [px]
   and [py] from dice_point. Ends on [roll]*)
let rec draw_dice_animated px py n roll =
  let x = dice_point.x + px in
  let y = dice_point.y + py in
  Unix.sleepf (0.1);
  let event = wait_next_event [Poll] in
  if event.button then (
    draw_dice x y (roll);
    let _ = wait_for_click () in () )
  else (
    draw_dice x y (Dicefile.roll n);
    draw_dice_animated px py n roll)

let click = ref false

(* [animated_draw_dice_with_string px py n roll] animates [n] dice to right of
   [s] placed at offset of [px] and [py] from dice_point. Ends on [roll]*)
let animated_draw_dice_with_string px py n s roll=
draw_fitted_text {x = dice_text_point.x + px; y = dice_text_point.y + py} s;
let new_x = current_x () + 3 in
let new_y = current_y () - 12 in
let _ = wait_next_event [Button_up] in ();
  click := false;
  while (not !click) do
    Unix.sleepf (0.1);
    draw_dice (new_x) (new_y) (Dicefile.roll n);
    let event = wait_next_event [Poll] in
    click := event.button
  done;
  if !click then (
    draw_dice_with_string px py s (roll);
    let _ = wait_next_event [Button_up] in () )

(* Renders a button of a ButtonHandler *)
let render_button button pos size =
  set_color Graphics.black;
  fill_rect pos.x pos.y size.x size.y;
  set_color (if button.enabled then button.color else grey);
  draw_rect pos.x pos.y size.x size.y;
  draw_centered_text (pos ++ size///2) button.text

module type ButtonHandlerSig = sig
  type 'a t

  val empty : point -> point -> int -> 'a t

  val add : 'a -> string -> int -> 'a t -> 'a t

  val fetch_index : int -> 'a t -> 'a

  val set_enabled : 'a -> bool -> 'a t -> unit

  val set_text : 'a -> string -> 'a t -> unit

  val set_enabled_index : int -> bool -> 'a t -> unit

  val mouse_input : (int * int) -> 'a t -> 'a option

  val render : 'a t -> unit
end

(* Gets the correct position of a button in a ButtonHandler *)
let get_button_pos index total size spacing =
  let x_add = if total mod 2 == 0 then spacing/2 else -size.x/2 in
  {x = (index - total/2) * (size.x + spacing) + x_add; y = -size.y/2}

type 'a handler = {
  size : point;
  center : point;
  spacing : int;
  buttons : 'a button list;
}

module ButtonHandler : ButtonHandlerSig = struct
  type 'a t = 'a handler

  let empty center size spacing = {size = size; center = center; spacing = spacing; buttons = []}

  let add identifier text col handler = {
    handler with
    buttons = (handler.buttons @ [{identifier = identifier; text = text; color = col; enabled = true}])
  }

  let fetch_index index handler = (List.nth handler.buttons index).identifier

  let set_enabled id enabled handler =
    match List.find_opt (fun b -> b.identifier = id) handler.buttons with
    | None -> ()
    | Some b -> b.enabled <- enabled

  let set_text id text handler =
    match List.find_opt (fun b -> b.identifier = id) handler.buttons with
    | None -> ()
    | Some b -> b.text <- text

  let set_enabled_index index enabled handler =
    (List.nth handler.buttons index).enabled <- enabled

  let mouse_input (x, y) handler =
    let len = List.length handler.buttons in
    let rec find index lst = (
      match lst with
      | [] -> None
      | b :: t ->
        if b.enabled && point_in_box x y (handler.center ++ (get_button_pos index len handler.size handler.spacing)) handler.size then
          Some b.identifier
        else find (index + 1) t
    ) in find 0 handler.buttons

  let render handler =
    let len = List.length handler.buttons in
    List.iteri (fun index b ->
        render_button b (handler.center ++ (get_button_pos index len handler.size handler.spacing)) handler.size
      ) handler.buttons

end

(* Gets the index of a stat from a character *)
let get_char_index c stat =
  match stat with
  | Might -> c.might_initial
  | Speed -> c.speed_initial
  | Knowledge -> c.knowledge_initial
  | Sanity -> c.sanity_initial

(* Gets the value of a stat from a character *)
let get_char_list c stat =
  match stat with
  | Might -> c.might
  | Speed -> c.speed
  | Knowledge -> c.knowledge
  | Sanity -> c.sanity

let char_spacing = 120

let render_char_selection chars st =
  let len = 3 in
  List.iteri (fun i c ->
      let col = match get_player_from_char st.players c with
        | None -> Graphics.white
        | Some i -> List.nth player_colors i in
      let pos = {x = screen_size.x/2; y = 120} ++ get_button_pos i len player_stat_size char_spacing in
      render_char "" (get_char_index c) (get_char_list c)
        {x = -1; y = -1} col
        pos.x pos.y player_stat_size.x player_stat_size.y;
      let size = {x = player_stat_size.x + char_spacing - 10; y = screen_size.y - pos.y - player_stat_size.y - 50} in
      render_popup_text_special
        size
        {x = pos.x + player_stat_size.x/2; y = pos.y + player_stat_size.y + size.y/2 + 10}
        c.name c.desc col
    ) chars

let create_char_buttons () =
  ButtonHandler.empty
    {x = screen_size.x/2; y = (120 - player_stat_size.y/2)/2}
    {x = player_stat_size.x; y = 100 - player_stat_size.y/2}
    char_spacing |>
      ButtonHandler.add 0 "Previous" Graphics.white |>
      ButtonHandler.add 1 "Select" Graphics.white |>
      ButtonHandler.add 2 "Select" Graphics.white |>
      ButtonHandler.add 3 "Select" Graphics.white |>
      ButtonHandler.add 4 "Next" Graphics.white

let render_game global_state =
  if not global_state.initialized then () else (
    clear_graph ();
    let rm = (List.nth global_state.players global_state.current_player_index).current_room in
    render_board rm global_state global_state.adding_info;
    render_players global_state;
    match global_state.haunt with
    | Some haunt ->
      set_color Graphics.white;
      draw_centered_text {x = screen_size.x/2; y = 6} ("Haunt Active: "^haunt.haunt_name)
    | None -> ()
   )

open Dicefile
open Graphics
open Data
open Uifile
open Players

(*Change is used in [roll_action] to raise or lower a player's stat by [int]
  Compare used in [roll_action] so if the player's role is greater than
  first [int], the players stat is lowered by the second [int]*)
type effect = Change | Compare of (int* int)

(* ensures that a click must be registered before the code continues*)
let wait_for_click () =
  let _ = wait_next_event [Poll] in
  let _ = wait_next_event [Button_down] in
  let _ = wait_next_event [Button_up] in ()

(* [roll_repl n] implements an animated dice roll and returns the results
   of the dice *)
let roll_repl (n: int) =
  let _ = wait_next_event [Button_up] in
  let lst = roll n in
  Uifile.draw_dice_animated (0) (0) (List.length lst) lst;
  lst

(* [roll_by_stat plyr stat] returns a dice roll with n dice, where n is
   value of [stat] in [plyr]*)
let roll_by_stat plyr stat =
  let lst = roll_repl (get_stat_value plyr stat) in
  sum lst

(* uses wait_next_event from OCaml graphics to maintain screen until user
   action *)
let rec wait_for_any ()=
  let _ = wait_next_event [Button_down;Key_pressed] in ()

(*[stat_increase p s_high s_can stat value state] implements changing [stat] in
  [p]. if [stat] max or min, then s_not printed in popup, else stat increased and
  [s_can] printed.*)
let stat_changes s_not s_can stat value state=
  let plyr = curr_plyr state in
  let index = get_stat_index plyr stat in
  if index > 0 && index < 7 then (
    Uifile.render_popup_text s_can "";
    change_stat plyr state value stat;
    wait_for_click ())
  else (
    Uifile.render_popup_text s_not "";
    wait_for_click () )

(*[roll_action state stat (n: int) (eff : effect)] implements a roll based on player
  [stat] of [n] dice with [eff]
  there are different effects based on [eff]
   Change: modifies [stat] index of current player
   Compare: modifies index if role above threshold
  Effects: draws dice roll*)
let roll_action state stat (n: int) (eff : effect) =
  let lst = roll_repl n in
  let i = sum lst in
  let pl = curr_plyr state in
  let _ = wait_next_event [Button_up] in
  match eff with
  |Change -> change_stat pl state (-i) stat
  |Compare (x, p) -> if i > x then change_stat pl state (-p) stat

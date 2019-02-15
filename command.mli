open Data

type effect = Change | Compare of (int* int)

(*returns a [int] length list of numbers 0..2 and renders animation of
  [int] 0..2 numbered dice rolling while waiting for user click. After
  click, renders the number list that is returned as dice *)
val roll_repl: (int -> int list)

(* [roll_by_stat plyr stat] returns a dice roll with n dice, where n is
   value of [stat] in [plyr] *)
val roll_by_stat : player -> stat -> int


(* uses OCAML Graphics "wait" to keep screen the same until user click or
key press *)
val wait_for_any : (unit -> unit)

(*uses OCaml Graphics "wait" to keep screen the same until user click*)
val wait_for_click : unit -> unit


(*[stat_increase p s_not s_can stat value state] implements changing [stat] in
  [p]. if [stat] cannot change in given direction (ie: decreasing from index 0
  or increasing from 7), then s_high printed in popup, else stat changed and
  [s_can] printed.*)
val stat_changes : string -> string -> stat -> int -> game_state -> unit

(*draws rolls [n] dice, draws them, and then changes current player of
  [state]'s [stat] according to [eff] *)
val roll_action : (game_state -> stat -> int -> effect -> unit)

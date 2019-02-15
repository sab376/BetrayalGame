open Data

(*returns a [game_state] with Haunt rules activates.
  [game_state]: is the state when Haunt is activate
  [card]: is the omen that activates haunt
  [int]: is the room that the haunt activating player was in *)
val activate_haunt : game_state -> card -> unit

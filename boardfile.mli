open Data

(* creates room from [tile] with orientation [int] *)
val create_room : tile -> int -> area -> room


val temp_expand : room -> room -> direction -> room

(* returns list of possible [tile] orientations
   when appending to some room in [direction]. For example, a tile with
   right and left exits and the direction Up would return [1;3] because it
   would need to be rotated to have a down exit to be valid *)
val available_orientations : direction -> tile -> int list

(*[can_travel room dir] returns false if the direction is invalid *)
val can_travel : room -> direction -> bool

(*[get_exit_from_dir room dir] returns the room in the direction [dir] from
  [room], None if there is no room there yet and Invalid if no room can be added *)
val get_exit_from_dir : room -> direction -> room ioption

val reset_map : unit -> unit

val rotate_room : room -> int -> room

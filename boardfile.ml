open Data
open Deckfile

let posMod i m = if i < 0 then m + i mod m else i mod m

let get_exit_from_dir room dir =
  match dir with
  | Right -> room.right
  | Left -> room.left
  | Up -> room.up
  | Down -> room.down

(*[set_exit_from_dir room dir exit] changes [room] so its [dir] exit is [exit] *)
let set_exit_from_dir room dir exit =
  match dir with
  | Right -> room.right <- exit
  | Down -> room.down <- exit
  | Left -> room.left <- exit
  | Up -> room.up <- exit

let rotate_room room rot =
  let rot' = posMod rot 4 in
  match rot' with
  | 1 ->
    {
      room with
      orientation = (room.orientation + rot') mod 4;
      left = room.down;
      down = room.right;
      right = room.up;
      up = room.left
    }
  | 2 -> {
      room with
      orientation = (room.orientation + rot') mod 4;
      left = room.right;
      down = room.up;
      right = room.left;
      up = room.down
    }
  | 3 -> {
      room with
      orientation = (room.orientation + rot') mod 4;
      left = room.up;
      down = room.left;
      right = room.down;
      up = room.right
    }
  | _ -> room

let room_id = ref 0

let create_room til ori area =
  incr room_id;
  rotate_room {
    id = !room_id;
    tile = til;
    orientation = 0;
    area = area;
    left = if til.tile_left then INone else Invalid;
    right = if til.tile_right then INone else Invalid;
    up = if til.tile_up then INone else Invalid;
    down = if til.tile_down then INone else Invalid;
  } ori

let reset_map () =
  room_id := 0

(*[append_room room add dir] appends [room] to [add] in [room]'s [dir] by '
  changing fields of both [room] and [add]. For example, append_room room add
  Right changes room.right to add and add.left to room *)
let append_room room add dir =
  match dir with
  | Right ->
    room.right <- ISome add;
    add.left <- ISome room
  | Down ->
    room.down <- ISome add;
    add.up <- ISome room
  | Left ->
    room.left <- ISome add;
    add.right <- ISome room
  | Up ->
    room.up <- ISome add;
    add.down <- ISome room


(* returns list of [tile] exits clockwise from right. true if
   exit exists, false otherwise. *)
let exits_from_tile tile =
  [tile.tile_right; tile.tile_down; tile.tile_left; tile.tile_up]

(* returns inverted direction of dir in tile list with indices
   [Right, Down, Left, Up].
   Ex: Right -> 2
       Down -> 3 *)
let invert_dir dir =
  match dir with
  | Right -> 2
  | Down -> 3
  | Left -> 0
  | Up -> 1

(* [available_orientations dir tile] is list of available rotations of [tile]
   when appending it from direction [dir]
   Example: a tile with exits to Left and Right would return [1;3] if [dir] is
   Up. *)
let available_orientations dir tile =
  let exits = exits_from_tile tile in
  let inv_index = invert_dir dir in
  let rec build_list lst index acc orientation=
    let i = posMod index 4 in
    if orientation >= 4 then acc else
      let new_lst = if List.nth lst i then orientation :: acc else acc in
      build_list lst (index - 1) new_lst (orientation + 1) in
  (build_list exits inv_index [] 0) |> List.rev

let rec list_iteri_h fnc lst acc =
  match lst with
  |[] -> INone
  |h :: t -> begin
      match fnc acc h with
      |INone -> list_iteri_h fnc t (acc +1)
      |ISome x -> ISome x
      |Invalid -> Invalid
    end

let list_iteri fnc lst = list_iteri_h fnc lst 0

(* Returns ISome room at [point] or None if no such room exists.
   Requires: dir is int representation of direction from 0-3 *)
    (*
let rec search_rms (cr: room) (cr_pt:point) (dir: int) (seen: room list) (target: point)=
  list_iteri (fun i r ->
      match r with
      |ISome room when (List.find_opt (fun x -> room = x) seen) = None ->
        let pt' = cr_pt ++ (int_to_point dir) in
        if pt'= target then (ISome room) else
          search_rms room pt' i (room::seen) target
      |_ -> INone)
    [cr.right; cr.down; cr.left; cr.up] *)

let search_rms (start_rm : room) (dir : direction) =
  let target = (direction_to_int dir) |> int_to_point in
  let seen = ref [] in
  let rec dfs_checker (room_opt : room ioption) (intdir : int) (rm : room) (pt : point) =
    match room_opt with
    | ISome other when (List.find_opt (fun x -> other.id = x) !seen) = None ->
      search other (pt ++ int_to_point intdir)
    | _ -> None
  and search (rm : room) (pt : point) =
    if pt = target then Some rm else (
      seen := (rm.id :: !seen);
      match (dfs_checker rm.right 0 rm pt) with
      | Some goal -> Some goal
      | _ -> (match (dfs_checker rm.down 1 rm pt) with
          | Some goal -> Some goal
          | _ -> (match (dfs_checker rm.left 2 rm pt) with
              | Some goal -> Some goal
              | _ -> (match (dfs_checker rm.up 3 rm pt) with
                  | Some goal -> Some goal
                  | _ -> None)
            )
        )
    )  in
  search start_rm {x = 0; y = 0}

let temp_expand room new_room dir =
  if get_exit_from_dir room dir = Invalid then failwith "That's not a valid expansion" else
    append_room room new_room dir;
    List.iter (fun exit_dir ->
        if exit_dir <> (direction_to_opposite dir) then
          match search_rms new_room exit_dir with
          | Some adjacent -> (
              match get_exit_from_dir adjacent (direction_to_opposite exit_dir) with
              | INone ->
                if (get_exit_from_dir new_room exit_dir) = INone then
                  append_room new_room adjacent exit_dir
                else
                  set_exit_from_dir adjacent (direction_to_opposite exit_dir) Invalid
              | Invalid ->
                set_exit_from_dir new_room exit_dir Invalid
              | _ -> ()
            )
          | None -> ()
      ) [Right; Down; Left; Up];
    new_room

    (*
let rec search_rms (cur : room) (pt : point) (target : point)
let rec check_room (rm : room) (newpt : point) =
  if newpt = target then ISome room else
    let rec iteri lst i =
      match lst with
      | [] -> INone
      | h :: t ->
        match check_room h (newpt ++ int_to_point i) with
        |
    list_iteri (fun i r ->
        match r with
        | ISome room when (List.find_opt (fun x -> room = x) seen) = None ->
          check_room room (newpt ++ int_to_point dir)
        | _ -> INone)
      [rm.right; rm.down; rm.left; rm.up]
*)
(*
  let rec iteri_cond lst i =
  match lst with
  | [] -> INone
  | h :: t ->*)


    (*
let check_place (cr: room) (dir: int) =
  let target = int_to_point dir in
  search_rms cr {x=0; y=0} dir [] target
*)

let can_travel room dir =
  match get_exit_from_dir room dir with
  | INone -> true
  | ISome _ -> true
  | Invalid -> false

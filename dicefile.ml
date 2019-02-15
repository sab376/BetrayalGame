open Data

let min_dice = 0  (* requires: min_val always less than max_val*)
let max_dice = 2

(* [roll n] is a list representing a roll of [n] dice between min_dice and max_dice*)
let roll n =
  let rec roller n roles =
    if n = 0 then roles
    else
      let num = Random.int (max_dice + 1) in
      if num < min_dice then roller n roles
      else roller (n-1) (num :: roles)
  in roller n []

(* [sum l] is sum of [l] *)
let sum l =
  let rec sum_helper s = function
    | [] -> s
    | h::t -> sum_helper (h+s) t
  in sum_helper 0 l

open! Core
open Types

type report =
  { support_center : point
  ; balance_distance : float
  ; horizontally_supported : bool
  ; stability : stability
  }
[@@deriving sexp_of]

let classify d =
  if Float.( < ) d Config.stable_threshold
  then Stable
  else if Float.( < ) d Config.strained_threshold
  then Strained
  else if Float.( < ) d Config.critical_threshold
  then Critical
  else Falling
;;

let report wall limbs ~torso =
  match Player.attached limbs with
  | [] -> None
  | attached ->
    let supports = List.map attached ~f:(fun (_, id) -> Wall.position_exn wall id) in
    let support_center = Geometry.average supports in
    let balance_distance = Geometry.distance torso support_center in
    let xs = List.map supports ~f:(fun p -> p.x) in
    let min_x = List.reduce_exn xs ~f:Float.min in
    let max_x = List.reduce_exn xs ~f:Float.max in
    let horizontally_supported =
      Float.( >= ) torso.x (min_x -. Config.balance_margin)
      && Float.( <= ) torso.x (max_x +. Config.balance_margin)
    in
    let stability =
      if List.length supports < 2 || not horizontally_supported
      then Falling
      else classify balance_distance
    in
    Some { support_center; balance_distance; horizontally_supported; stability }
;;

let stability wall limbs ~torso =
  match report wall limbs ~torso with
  | None -> Falling
  | Some r -> r.stability
;;

open! Core
open Types

type t =
  { current : game_state
  ; history : game_state list
  }

let create ~wall ~start =
  { current =
      { player = start; wall; broken_holds = Set.empty (module Int); status = Playing }
  ; history = []
  }
;;

(* Phase 1 win check — §4.3 simplified: both hands on Finish holds and at
   least one foot attached. "Pose valid" is guaranteed by the gate; the
   "stamina > 0" clause joins in Phase 3 when stamina starts moving. *)
let compute_status (gs : game_state) =
  let hand_on_finish id =
    match Option.bind id ~f:(Wall.find gs.wall) with
    | Some { kind = Finish; _ } -> true
    | Some _ | None -> false
  in
  let foot_attached =
    Option.is_some gs.player.limbs.left_foot || Option.is_some gs.player.limbs.right_foot
  in
  if hand_on_finish gs.player.limbs.left_hand
     && hand_on_finish gs.player.limbs.right_hand
     && foot_attached
  then Won
  else gs.status
;;

let current_state t = t.current

let move t limb ~hold_id =
  let gs = t.current in
  match Wall.find gs.wall hold_id with
  | None -> `Rejected Hold_broken
  | Some hold ->
    (match Movement.attempt_move ~wall:gs.wall ~broken:gs.broken_holds gs.player limb hold with
     | Error reason -> `Rejected reason
     | Ok player ->
       let next = { gs with player } in
       let next = { next with status = compute_status next } in
       `Moved { current = next; history = gs :: t.history })
;;

let undo t =
  match t.history with
  | [] -> None
  | previous :: history -> Some { current = previous; history }
;;

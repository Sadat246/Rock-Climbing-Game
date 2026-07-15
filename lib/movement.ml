open! Core
open Types

(* Both hands may share a Jug/Rest (hand-matching is real climbing), but a
   Crimp cannot hold both hands at once (§4.3). Feet never contend in Phase 0. *)
let illegally_occupied (limbs : limb_positions) limb (hold : hold) =
  match hold.kind with
  | Crimp ->
    (match limb with
     | Left_hand -> [%equal: int option] limbs.right_hand (Some hold.id)
     | Right_hand -> [%equal: int option] limbs.left_hand (Some hold.id)
     | Left_foot | Right_foot -> false)
  | Jug | Sloper | Foothold | Rest | Crumbling | Chalk_refill | Finish -> false
;;

let within_reach limb ~torso ~(target : point) =
  let close_enough =
    Float.( <= ) (Geometry.distance torso target) (Geometry.max_reach limb)
  in
  let vertical_ok =
    if Hold.is_foot limb
    then Float.( <= ) (target.y -. torso.y) Config.max_foot_above_torso
    else Float.( <= ) (torso.y -. target.y) Config.max_hand_below_torso
  in
  close_enough && vertical_ok
;;

let attempt_move ~(wall : wall) ~broken (player : player_state) limb (hold : hold) =
  match Wall.find wall hold.id with
  (* Unknown ids and broken holds both read as "that hold is gone". *)
  | None -> Error Hold_broken
  | Some hold ->
    if Set.mem broken hold.id
    then Error Hold_broken
    else if illegally_occupied player.limbs limb hold
    then Error Hold_occupied
    else if not (Hold.limb_compatible limb hold.kind)
    then Error Wrong_limb_for_hold
    else if not (within_reach limb ~torso:player.torso ~target:hold.position)
    then Error Out_of_reach
    else (
      let limbs = Player.set_limb player.limbs limb hold.id in
      let torso =
        Geometry.average
          (List.map (Player.attached limbs) ~f:(fun (_, id) -> Wall.position_exn wall id))
      in
      Ok { player with limbs; torso; turn = player.turn + 1 })
;;

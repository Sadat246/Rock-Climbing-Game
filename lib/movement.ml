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

(* §6.4 no-teleporting: after the torso shifts, every attached limb must still
   be within its radial reach (the §4.5 vertical limits are placement-time
   rules only, so they are not re-checked here). *)
let all_within_reach (wall : wall) (limbs : limb_positions) ~torso =
  List.for_all (Player.attached limbs) ~f:(fun (limb, id) ->
    Float.( <= )
      (Geometry.distance torso (Wall.position_exn wall id))
      (Geometry.max_reach limb))
;;

(* Body-span constraints (§4.6), on attached pairs only. *)
let span_ok (wall : wall) (limbs : limb_positions) =
  let pos limb =
    Option.map (Player.limb_hold_id limbs limb) ~f:(Wall.position_exn wall)
  in
  let within a b limit =
    match pos a, pos b with
    | Some pa, Some pb -> Float.( <= ) (Geometry.distance pa pb) limit
    | None, _ | _, None -> true
  in
  (* The left limb may not end up more than max_cross_over to the RIGHT of
     its right-side partner. *)
  let no_cross left right =
    match pos left, pos right with
    | Some pl, Some pr -> Float.( <= ) (pl.x -. pr.x) Config.max_cross_over
    | None, _ | _, None -> true
  in
  within Left_hand Right_hand Config.max_hand_span
  && within Left_foot Right_foot Config.max_foot_span
  && within Left_hand Left_foot Config.max_body_length
  && within Right_hand Right_foot Config.max_body_length
  && no_cross Left_hand Right_hand
  && no_cross Left_foot Right_foot
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
      (* Simulate: reattach the limb, then shift the torso toward the
         destination (§4.4). *)
      let limbs = Player.set_limb player.limbs limb hold.id in
      let torso =
        Geometry.shift_toward
          player.torso
          ~target:hold.position
          ~factor:Config.torso_shift_factor
      in
      if not (all_within_reach wall limbs ~torso)
      then Error Out_of_reach
      else if not (span_ok wall limbs)
      then Error Span_violation
      else (
        match Balance.stability wall limbs ~torso with
        | Falling -> Error Would_fall
        | Stable | Strained | Critical ->
          Ok { player with limbs; torso; turn = player.turn + 1 }))
;;

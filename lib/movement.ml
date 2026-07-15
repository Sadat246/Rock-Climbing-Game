open! Core
open Types

type cost =
  { movement : int
  ; penalties : int
  ; upkeep : int
  ; grip : int
  ; total : int
  }
[@@deriving sexp_of]

(* Both hands may share a Jug/Rest (hand-matching is real climbing), but a
   Crimp cannot hold both hands at once (§4.3). Feet never contend in Phase 3. *)
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

let limb_pair_positions (wall : wall) (limbs : limb_positions) a b =
  let pos limb =
    Option.map (Player.limb_hold_id limbs limb) ~f:(Wall.position_exn wall)
  in
  Option.both (pos a) (pos b)
;;

(* Body-span constraints (§4.6), on attached pairs only. *)
let span_ok (wall : wall) (limbs : limb_positions) =
  let within a b limit =
    match limb_pair_positions wall limbs a b with
    | Some (pa, pb) -> Float.( <= ) (Geometry.distance pa pb) limit
    | None -> true
  in
  (* The left limb may not end up more than max_cross_over to the RIGHT of
     its right-side partner. *)
  let no_cross left right =
    match limb_pair_positions wall limbs left right with
    | Some (pl, pr) -> Float.( <= ) (pl.x -. pr.x) Config.max_cross_over
    | None -> true
  in
  within Left_hand Right_hand Config.max_hand_span
  && within Left_foot Right_foot Config.max_foot_span
  && within Left_hand Left_foot Config.max_body_length
  && within Right_hand Right_foot Config.max_body_length
  && no_cross Left_hand Right_hand
  && no_cross Left_foot Right_foot
;;

(* ----- Cost accounting (§4.9) ----- *)

(* Direction is judged from the limb's CURRENT position: up costs more than
   sideways/down. A detached limb (possible after Phase 5 breakage) pays the
   up rate. *)
let movement_cost (wall : wall) (limbs_before : limb_positions) limb ~(target : point) =
  let up, side =
    if Hold.is_hand limb
    then Config.hand_up, Config.hand_side
    else Config.foot_up, Config.foot_side
  in
  match Player.limb_hold_id limbs_before limb with
  | None -> up
  | Some id ->
    let from = Wall.position_exn wall id in
    if Float.( > ) target.y from.y then up else side
;;

(* Cross-body: a left-side limb grabbing right of the (pre-move) torso, or
   vice versa. *)
let cross_body limb ~(torso : point) ~(target : point) =
  match limb with
  | Left_hand | Left_foot -> Float.( > ) target.x torso.x
  | Right_hand | Right_foot -> Float.( < ) target.x torso.x
;;

(* Big stretch: any post-move span beyond 0.85 × its maximum. *)
let stretched (wall : wall) (limbs : limb_positions) =
  let over a b limit =
    match limb_pair_positions wall limbs a b with
    | Some (pa, pb) -> Float.( > ) (Geometry.distance pa pb) (0.85 *. limit)
    | None -> false
  in
  over Left_hand Right_hand Config.max_hand_span
  || over Left_foot Right_foot Config.max_foot_span
  || over Left_hand Left_foot Config.max_body_length
  || over Right_hand Right_foot Config.max_body_length
;;

let stability_multiplier = function
  | Stable -> Config.mult_stable
  | Strained -> Config.mult_strained
  | Critical -> Config.mult_critical
  | Falling -> Config.mult_critical (* unreachable: gate rejects Falling *)
;;

(* Per-hand grip on the POST-move pose, scaled by post-move stability (§4.8). *)
let grip_cost (wall : wall) (limbs : limb_positions) (chalk : chalk_state) ~stability =
  let hand limb chalk_left =
    match Player.limb_hold_id limbs limb with
    | None -> 0
    | Some id ->
      (match Wall.find wall id with
       | None -> 0
       | Some hold -> Hold.grip_cost hold.kind ~chalked:(chalk_left > 0))
  in
  let base = hand Left_hand chalk.left_hand_chalk + hand Right_hand chalk.right_hand_chalk in
  Int.of_float
    (Float.round_nearest (Float.of_int base *. stability_multiplier stability))
;;

(* Pose upkeep on the POST-move pose (§4.9). *)
let upkeep_cost (limbs : limb_positions) ~stability =
  let base =
    match stability with
    | Stable -> Config.upkeep_stable
    | Strained -> Config.upkeep_strained
    | Critical | Falling -> Config.upkeep_critical
  in
  let feet_off =
    match Option.is_some limbs.left_foot, Option.is_some limbs.right_foot with
    | true, true -> 0
    | true, false | false, true -> Config.one_foot_off
    | false, false -> Config.both_feet_off
  in
  base + feet_off
;;

(* §4.10: per-hand chalk decays by one every turn (floor 0). *)
let decay_chalk (c : chalk_state) =
  { c with
    left_hand_chalk = Int.max 0 (c.left_hand_chalk - 1)
  ; right_hand_chalk = Int.max 0 (c.right_hand_chalk - 1)
  }
;;

let preview_move ~(wall : wall) ~broken (player : player_state) limb (hold : hold) =
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
      then Error Limb_stranded
      else if not (span_ok wall limbs)
      then Error Span_violation
      else (
        match Balance.stability wall limbs ~torso with
        | Falling -> Error Would_fall
        | (Stable | Strained | Critical) as stability ->
          let movement = movement_cost wall player.limbs limb ~target:hold.position in
          let penalties =
            (if stretched wall limbs then Config.big_stretch_penalty else 0)
            + (if cross_body limb ~torso:player.torso ~target:hold.position
               then Config.cross_body_penalty
               else 0)
          in
          let upkeep = upkeep_cost limbs ~stability in
          (* grip uses THIS turn's chalk (pre-decay): the chalk on your hand
             when you grab is what you grab with *)
          let grip = grip_cost wall limbs player.chalk ~stability in
          let total = movement + penalties + upkeep + grip in
          let stamina = Int.max 0 (player.stamina - total) in
          let chalk = decay_chalk player.chalk in
          (* grabbing a Chalk_refill hold with a hand tops the bag back up *)
          let chalk =
            match hold.kind, Hold.is_hand limb with
            | Chalk_refill, true ->
              { chalk with
                remaining =
                  Int.min Config.starting_chalk (chalk.remaining + Config.refill_amount)
              }
            | _, _ -> chalk
          in
          Ok
            ( { limbs; torso; stamina; chalk; turn = player.turn + 1 }
            , { movement; penalties; upkeep; grip; total } )))
;;

(* §4.10: chalk the given hand — bag -1, hand chalked for chalk_duration
   turns. Costs a turn (during which the other hand's chalk decays), no
   stamina. Chalking a foot is nonsense; an empty bag is a typed rejection. *)
let attempt_chalk ~wall:(_ : wall) ~broken:(_ : Set.M(Int).t) (player : player_state) limb =
  if not (Hold.is_hand limb)
  then Error Wrong_limb_for_hold
  else if player.chalk.remaining <= 0
  then Error No_chalk_left
  else (
    let chalk = decay_chalk player.chalk in
    let chalk = { chalk with remaining = player.chalk.remaining - 1 } in
    let chalk =
      match limb with
      | Left_hand -> { chalk with left_hand_chalk = Config.chalk_duration }
      | Right_hand -> { chalk with right_hand_chalk = Config.chalk_duration }
      | Left_foot | Right_foot -> chalk
    in
    Ok { player with chalk; turn = player.turn + 1 })
;;

let attempt_move ~wall ~broken (player : player_state) limb hold =
  match preview_move ~wall ~broken player limb hold with
  | Error _ as e -> e
  | Ok (next, cost) ->
    if cost.total > player.stamina then Error Insufficient_stamina else Ok next
;;

let attempt_rest ~(wall : wall) ~broken:_ (player : player_state) =
  let hand_on_rest limb =
    match Player.limb_hold_id player.limbs limb with
    | None -> false
    | Some id ->
      (match Wall.find wall id with
       | Some { kind = Rest; _ } -> true
       | Some _ | None -> false)
  in
  let foot_attached =
    Option.is_some player.limbs.left_foot || Option.is_some player.limbs.right_foot
  in
  let stable =
    match Balance.stability wall player.limbs ~torso:player.torso with
    | Stable -> true
    | Strained | Critical | Falling -> false
  in
  if (hand_on_rest Left_hand || hand_on_rest Right_hand) && foot_attached && stable
  then
    Ok
      { player with
        stamina = Int.min Config.starting_stamina (player.stamina + Config.rest_recovery)
      ; chalk = decay_chalk player.chalk (* §4.10: resting still costs a turn of chalk *)
      ; turn = player.turn + 1
      }
  else Error Cannot_rest
;;

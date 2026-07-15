open! Core
open Types

let is_hand = function
  | Left_hand | Right_hand -> true
  | Left_foot | Right_foot -> false
;;

let is_foot limb = not (is_hand limb)

let grip_cost kind ~chalked =
  match kind with
  | Jug -> Config.jug_grip
  | Crimp -> if chalked then Config.crimp_grip_chalked else Config.crimp_grip
  | Sloper -> if chalked then Config.sloper_grip_chalked else Config.sloper_grip
  | Rest -> Config.rest_grip
  | Finish -> Config.finish_grip
  | Chalk_refill -> Config.chalk_refill_grip
  | Foothold -> 0 (* hands can't be here anyway *)
  | Crumbling ->
    (* inherits a base kind in Phase 5; no earlier wall uses it *)
    Config.jug_grip
;;

let limb_compatible limb kind =
  match kind with
  | Jug | Rest -> true
  | Crimp | Sloper | Chalk_refill | Finish -> is_hand limb
  | Foothold -> is_foot limb
  | Crumbling ->
    (* Crumbling holds inherit a base kind in a later phase ("varies" in the
       §4.3 table); no Phase 0 wall uses them, so allow all limbs for now. *)
    true
;;

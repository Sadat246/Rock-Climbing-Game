open! Core
open Types

let is_hand = function
  | Left_hand | Right_hand -> true
  | Left_foot | Right_foot -> false
;;

let is_foot limb = not (is_hand limb)

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

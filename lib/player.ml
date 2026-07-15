open! Core
open Types

let limb_hold_id (limbs : limb_positions) = function
  | Left_hand -> limbs.left_hand
  | Right_hand -> limbs.right_hand
  | Left_foot -> limbs.left_foot
  | Right_foot -> limbs.right_foot
;;

let set_limb (limbs : limb_positions) limb hold_id =
  match limb with
  | Left_hand -> { limbs with left_hand = Some hold_id }
  | Right_hand -> { limbs with right_hand = Some hold_id }
  | Left_foot -> { limbs with left_foot = Some hold_id }
  | Right_foot -> { limbs with right_foot = Some hold_id }
;;

let attached limbs =
  List.filter_map all_of_limb ~f:(fun limb ->
    Option.map (limb_hold_id limbs limb) ~f:(fun id -> limb, id))
;;

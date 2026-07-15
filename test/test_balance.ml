open! Core
open Climb
open Climb.Types

(* Scripted poses with known expected stability (§6.2 / Phase 2). Torso
   positions are placed explicitly to probe each classification band; the
   ladder-start limbs put the support center at (60, 45). *)

let wall = Wall.test_wall_ladder
let start_limbs = Wall.ladder_start.limbs

let probe ?(limbs = start_limbs) x y =
  match Balance.report wall limbs ~torso:{ x; y } with
  | None -> printf "torso (%3.0f,%3.0f): no supports\n" x y
  | Some r ->
    printf
      !"torso (%3.0f,%3.0f): d %5.1f  horiz %b  %{sexp:stability}\n"
      x
      y
      r.balance_distance
      r.horizontally_supported
      r.stability
;;

let%expect_test "classification bands around the support center (60,45)" =
  probe 60. 45.;
  probe 60. 60.;
  probe 60. 70.;
  probe 60. 90.;
  probe 60. 110.;
  (* threshold boundary: d = 20 exactly is already Strained (strict <) *)
  probe 80. 45.;
  [%expect {|
    torso ( 60, 45): d   0.0  horiz true  Stable
    torso ( 60, 60): d  15.0  horiz true  Stable
    torso ( 60, 70): d  25.0  horiz true  Strained
    torso ( 60, 90): d  45.0  horiz true  Critical
    torso ( 60,110): d  65.0  horiz true  Falling
    torso ( 80, 45): d  20.0  horiz true  Strained
    |}]
;;

let%expect_test "horizontal support check overrides distance" =
  (* d = 50 would be Critical, but the torso is left of every support - margin *)
  probe 10. 45.;
  (* leaning within the margin is fine *)
  probe 90. 45.;
  [%expect {|
    torso ( 10, 45): d  50.0  horiz false  Falling
    torso ( 90, 45): d  30.0  horiz true  Strained
    |}]
;;

let%expect_test "fewer than two supports is always Falling" =
  let one_limb =
    { left_hand = Some 2; right_hand = None; left_foot = None; right_foot = None }
  in
  probe ~limbs:one_limb 40. 60.;
  let none =
    { left_hand = None; right_hand = None; left_foot = None; right_foot = None }
  in
  probe ~limbs:none 40. 60.;
  [%expect {|
    torso ( 40, 60): d   0.0  horiz true  Falling
    torso ( 40, 60): no supports
    |}]
;;

let%expect_test "monotonicity: closer to the support center never worsens (§6.4)" =
  (* walk the torso inward along a line and check the class only improves *)
  let rank = function Stable -> 0 | Strained -> 1 | Critical -> 2 | Falling -> 3 in
  let classes =
    List.map [ 110.; 95.; 80.; 65.; 50.; 45. ] ~f:(fun y ->
      Balance.stability wall start_limbs ~torso:{ x = 60.; y })
  in
  let monotone =
    List.for_all2_exn (List.drop_last_exn classes) (List.tl_exn classes) ~f:(fun a b ->
      rank a >= rank b)
  in
  printf
    !"%{sexp:stability list}\nmonotone improving: %b\n"
    classes
    monotone;
  [%expect {|
    (Falling Critical Strained Strained Stable Stable)
    monotone improving: true
    |}]
;;

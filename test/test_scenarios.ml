open! Core
open Climb
open Climb.Types

(* The canary scenario (CLAUDE.md §6.2): the jug ladder must ALWAYS be
   climbable. Replays the full scripted ascent through the single gate and
   checks the no-teleporting invariant (§6.4) after every accepted move. *)

let ladder_script =
  [ Left_hand, 12
  ; Right_hand, 13
  ; Left_foot, 2
  ; Right_foot, 3
  ; Left_hand, 14
  ; Right_hand, 15
  ; Left_foot, 4
  ; Right_foot, 5
  ; Left_hand, 16
  ; Right_hand, 17
  ; Left_foot, 6
  ; Right_foot, 7
  ; Left_hand, 18
  ; Right_hand, 19
  ]
;;

(* §6.4 no-teleporting: every attached limb within its max reach of the torso,
   feet no higher above the torso (and hands no lower below it) than §4.5
   allows. Must hold after EVERY accepted move. *)
let no_teleporting wall (p : player_state) =
  List.for_all (Player.attached p.limbs) ~f:(fun (limb, id) ->
    let pos = Wall.position_exn wall id in
    let within_radius =
      Float.( <= ) (Geometry.distance p.torso pos) (Geometry.max_reach limb)
    in
    let vertical_ok =
      if Hold.is_foot limb
      then Float.( <= ) (pos.y -. p.torso.y) Config.max_foot_above_torso
      else Float.( <= ) (p.torso.y -. pos.y) Config.max_hand_below_torso
    in
    within_radius && vertical_ok)
;;

let%expect_test "ladder canary: scripted ascent wins, no teleporting" =
  let wall = Wall.test_wall_ladder in
  let broken = Set.empty (module Int) in
  let final =
    List.fold ladder_script ~init:Wall.ladder_start ~f:(fun player (limb, hold_id) ->
      let hold = Option.value_exn (Wall.find wall hold_id) in
      match Movement.attempt_move ~wall ~broken player limb hold with
      | Error reason ->
        (* Dump the board — read it, don't guess from numbers (§6.1). *)
        print_endline
          (Ascii.render { player; wall; broken_holds = broken; status = Playing });
        raise_s
          [%message
            "ladder move rejected" (limb : limb) (hold_id : int) (reason : reject_reason)]
      | Ok next ->
        Expect_test_helpers_core.require
          (no_teleporting wall next)
          ~if_false_then_print_s:(lazy [%message "teleport!" (next : player_state)]);
        printf
          "turn %2d  %-10s -> %2d   torso (%3.0f, %3.0f)\n"
          next.turn
          (Sexp.to_string [%sexp (limb : limb)])
          hold_id
          next.torso.x
          next.torso.y;
        next)
  in
  let on_finish id =
    match Option.bind id ~f:(Wall.find wall) with
    | Some { kind = Finish; _ } -> true
    | Some _ | None -> false
  in
  printf
    "both hands on finish: %b\n"
    (on_finish final.limbs.left_hand && on_finish final.limbs.right_hand);
  [%expect {|
    turn  1  Left_hand  -> 12   torso ( 60,  60)
    turn  2  Right_hand -> 13   torso ( 60,  75)
    turn  3  Left_foot  ->  2   torso ( 60,  90)
    turn  4  Right_foot ->  3   torso ( 60, 105)
    turn  5  Left_hand  -> 14   torso ( 60, 120)
    turn  6  Right_hand -> 15   torso ( 60, 135)
    turn  7  Left_foot  ->  4   torso ( 60, 150)
    turn  8  Right_foot ->  5   torso ( 60, 165)
    turn  9  Left_hand  -> 16   torso ( 60, 180)
    turn 10  Right_hand -> 17   torso ( 60, 195)
    turn 11  Left_foot  ->  6   torso ( 60, 210)
    turn 12  Right_foot ->  7   torso ( 60, 225)
    turn 13  Left_hand  -> 18   torso ( 60, 240)
    turn 14  Right_hand -> 19   torso ( 60, 255)
    both hands on finish: true
    |}]
;;

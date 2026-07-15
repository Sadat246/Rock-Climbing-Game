open! Core
open Climb
open Climb.Types

let wall = Wall.test_wall_ladder
let start = Wall.ladder_start
let no_broken = Set.empty (module Int)

(* Fabricate a hold record for ids missing from the wall — attempt_move must
   trust only what the wall itself contains. *)
let hold_for_id hold_id =
  match Wall.find wall hold_id with
  | Some hold -> hold
  | None ->
    { id = hold_id; position = { x = 0.; y = 0. }; kind = Jug; durability = None }
;;

let try_move ?(player = start) ?(broken = no_broken) limb hold_id =
  Movement.attempt_move ~wall ~broken player limb (hold_for_id hold_id)
;;

let print_result = function
  | Error reason -> printf !"REJECTED %{sexp:reject_reason}\n" reason
  | Ok (p : player_state) ->
    print_s
      [%message
        "ok" ~limbs:(p.limbs : limb_positions) ~torso:(p.torso : point) ~turn:(p.turn : int)]
;;

let%expect_test "legal jug move: limb + torso update, turn increments" =
  print_result (try_move Left_hand 12);
  [%expect {|
    (ok
     (limbs
      ((left_hand (12)) (right_hand (11)) (left_foot (0)) (right_foot (1))))
     (torso ((x 60) (y 60))) (turn 1))
    |}]
;;

let%expect_test "unknown hold id and broken hold both read Hold_broken" =
  print_result (try_move Left_hand 999);
  print_result (try_move ~broken:(Set.of_list (module Int) [ 12 ]) Left_hand 12);
  [%expect {|
    REJECTED Hold_broken
    REJECTED Hold_broken
    |}]
;;

let%expect_test "limb compatibility: foot on Finish, hand on Foothold" =
  print_result (try_move Left_foot 18);
  print_result (try_move Left_hand 2);
  [%expect {|
    REJECTED Wrong_limb_for_hold
    REJECTED Wrong_limb_for_hold
    |}]
;;

let%expect_test "out of reach: too far, and foot above torso limit" =
  (* hold 16 is 195 units from the starting torso — beyond hand_reach *)
  print_result (try_move Left_hand 16);
  (* jug 13 is within foot_reach radius of the torso, but 75 units above it —
     beyond max_foot_above_torso *)
  print_result (try_move Right_foot 13);
  [%expect {|
    REJECTED Out_of_reach
    REJECTED Out_of_reach
    |}]
;;

let%expect_test "both hands may share a jug; not a crimp" =
  (* right hand joins the left on jug 10 *)
  print_result (try_move Right_hand 10);
  [%expect {|
    (ok
     (limbs
      ((left_hand (10)) (right_hand (10)) (left_foot (0)) (right_foot (1))))
     (torso ((x 50) (y 45))) (turn 1))
    |}];
  (* a crimp wall: one crimp between two jugs, feet on footholds below *)
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  let crimp_wall =
    { holds =
        [| mk 0 30. 60. Jug
         ; mk 1 60. 80. Crimp
         ; mk 2 90. 60. Jug
         ; mk 3 30. 10. Foothold
         ; mk 4 60. 10. Foothold
        |]
    ; width = 120
    ; height = 100
    ; finish_y = 100.
    }
  in
  let limbs =
    { left_hand = Some 0; right_hand = Some 1; left_foot = Some 3; right_foot = Some 4 }
  in
  let player =
    { start with
      limbs
    ; torso =
        Geometry.average
          (List.map (Player.attached limbs) ~f:(fun (_, id) ->
             Wall.position_exn crimp_wall id))
    }
  in
  (* left hand tries to join the right hand on the crimp *)
  (match
     Movement.attempt_move
       ~wall:crimp_wall
       ~broken:no_broken
       player
       Left_hand
       (Option.value_exn (Wall.find crimp_wall 1))
   with
   | result -> print_result result);
  [%expect {| REJECTED Hold_occupied |}]
;;

let%expect_test "purity: same inputs, same result; input state unchanged" =
  let before = start in
  let r1 = try_move Left_hand 12 in
  let r2 = try_move Left_hand 12 in
  printf
    "same result: %b\n"
    ([%equal: (player_state, reject_reason) Result.t] r1 r2);
  printf "input unchanged: %b\n" ([%equal: player_state] before Wall.ladder_start);
  [%expect {|
    same result: true
    input unchanged: true
    |}]
;;

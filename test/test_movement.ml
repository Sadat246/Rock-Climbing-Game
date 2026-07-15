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

let%expect_test "legal jug move: limb updates, torso SHIFTS (not average)" =
  print_result (try_move Left_hand 4);
  [%expect {|
    (ok
     (limbs ((left_hand (4)) (right_hand (3)) (left_foot (0)) (right_foot (1))))
     (torso ((x 55) (y 56.25))) (turn 1))
    |}]
;;

let%expect_test "unknown hold id and broken hold both read Hold_broken" =
  print_result (try_move Left_hand 999);
  print_result (try_move ~broken:(Set.of_list (module Int) [ 4 ]) Left_hand 4);
  [%expect {|
    REJECTED Hold_broken
    REJECTED Hold_broken
    |}]
;;

let%expect_test "limb compatibility: foot on Finish, hand on Foothold" =
  print_result (try_move Left_foot 18);
  print_result (try_move Left_hand 0);
  [%expect {|
    REJECTED Wrong_limb_for_hold
    REJECTED Wrong_limb_for_hold
    |}]
;;

let%expect_test "out of reach: too far, and foot above torso limit" =
  (* jug 14 (40,240) is ~196 units from the starting torso — beyond hand_reach *)
  print_result (try_move Left_hand 14);
  (* jug 7 (80,120) is inside foot_reach radius of the torso, but 75 units
     above it — beyond max_foot_above_torso *)
  print_result (try_move Right_foot 7);
  [%expect {|
    REJECTED Out_of_reach
    REJECTED Out_of_reach
    |}]
;;

let%expect_test "no-teleport post-check: a move may not strand another limb" =
  (* Contrived lagged torso high above the start pose: the left hand can grab
     jug 8 (40,150), but the shifted torso would leave the left foot (40,30)
     beyond foot_reach. *)
  let lagged = { start with torso = { x = 60.; y = 110. } } in
  print_result (try_move ~player:lagged Left_hand 8);
  [%expect {| REJECTED Out_of_reach |}]
;;

let%expect_test "both hands may share a jug; not a crimp" =
  (* right hand joins the left on jug 2 *)
  print_result (try_move Right_hand 2);
  [%expect {|
    (ok
     (limbs ((left_hand (2)) (right_hand (2)) (left_foot (0)) (right_foot (1))))
     (torso ((x 55) (y 48.75))) (turn 1))
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

(* Span-constraint wall: left hand parked far left, a jug 151 units away on
   the right. Reaching it is within torso reach but violates max_hand_span;
   the same target grabbed with the LEFT hand violates max_cross_over. *)
let span_wall =
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  { holds =
      [| mk 0 0. 100. Jug
       ; mk 1 60. 100. Jug
       ; mk 2 151. 100. Jug
       ; mk 3 65. 30. Foothold
       ; mk 4 75. 30. Foothold
      |]
  ; width = 200
  ; height = 140
  ; finish_y = 140.
  }
;;

let span_player =
  let limbs =
    { left_hand = Some 0; right_hand = Some 1; left_foot = Some 3; right_foot = Some 4 }
  in
  { start with
    limbs
  ; torso =
      Geometry.average
        (List.map (Player.attached limbs) ~f:(fun (_, id) ->
           Wall.position_exn span_wall id))
  }
;;

let try_span limb hold_id =
  Movement.attempt_move
    ~wall:span_wall
    ~broken:no_broken
    span_player
    limb
    (Option.value_exn (Wall.find span_wall hold_id))
;;

let%expect_test "span violations: hand span, and cross-over" =
  print_result (try_span Right_hand 2);
  print_result (try_span Left_hand 2);
  [%expect {|
    REJECTED Span_violation
    REJECTED Span_violation
    |}]
;;

let%expect_test "would fall: torso ends outside horizontal support + margin" =
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  let ledge_wall =
    { holds =
        [| mk 0 60. 90. Jug
         ; mk 1 80. 90. Jug
         ; mk 2 60. 30. Foothold
         ; mk 3 80. 30. Foothold
         ; mk 4 95. 30. Foothold
        |]
    ; width = 140
    ; height = 120
    ; finish_y = 120.
    }
  in
  (* Contrived: torso lagged far left of the supports. Stepping the right
     foot further right leaves the shifted torso outside
     [min support x - margin]. *)
  let player =
    { start with
      limbs =
        { left_hand = Some 0; right_hand = Some 1; left_foot = Some 2; right_foot = Some 3 }
    ; torso = { x = 25.; y = 60. }
    }
  in
  (match
     Movement.attempt_move
       ~wall:ledge_wall
       ~broken:no_broken
       player
       Right_foot
       (Option.value_exn (Wall.find ledge_wall 4))
   with
   | result -> print_result result);
  [%expect {| REJECTED Would_fall |}]
;;

let%expect_test "purity: same inputs, same result; input state unchanged" =
  let before = start in
  let r1 = try_move Left_hand 4 in
  let r2 = try_move Left_hand 4 in
  printf
    "same result: %b\n"
    ([%equal: (player_state, reject_reason) Result.t] r1 r2);
  printf "input unchanged: %b\n" ([%equal: player_state] before Wall.ladder_start);
  [%expect {|
    same result: true
    input unchanged: true
    |}]
;;

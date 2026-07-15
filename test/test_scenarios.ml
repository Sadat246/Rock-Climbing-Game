open! Core
open Climb
open Climb.Types

(* The canary scenario (CLAUDE.md §6.2): the jug ladder must ALWAYS be
   climbable. Replays the scripted ascent through the single gate and checks
   the §6.4 no-teleporting invariant (radial reach + all span constraints)
   after every accepted move. *)

(* Hands climb two rung-rows ahead; feet follow onto the vacated rungs;
   shake-outs at the y=120 and y=210 Rest rows keep the stamina budget alive
   (the full climb costs ~170 against 100). *)
let ladder_script =
  let cycle c =
    [ `M (Left_hand, (2 * c) + 4)
    ; `M (Right_hand, (2 * c) + 5)
    ; `M (Left_foot, (2 * c) + 2)
    ; `M (Right_foot, (2 * c) + 3)
    ]
  in
  let cycles a b = List.concat_map (List.range a b) ~f:cycle in
  cycles 0 2
  @ [ `R; `R; `R ]
  @ cycles 2 5
  @ [ `R; `R; `R; `R; `R ]
  @ cycles 5 7
  @ [ `M (Left_hand, 18); `M (Right_hand, 19) ]
;;

(* Independent recomputation of the §6.4 invariant — deliberately NOT calling
   Movement's internals, so a bug there can't hide itself. *)
let no_teleporting wall (p : player_state) =
  let pos limb = Option.map (Player.limb_hold_id p.limbs limb) ~f:(Wall.position_exn wall) in
  let reach_ok =
    List.for_all (Player.attached p.limbs) ~f:(fun (limb, id) ->
      Float.( <= )
        (Geometry.distance p.torso (Wall.position_exn wall id))
        (Geometry.max_reach limb))
  in
  let span le a b limit =
    match pos a, pos b with
    | Some pa, Some pb -> le pa pb limit
    | None, _ | _, None -> true
  in
  let dist pa pb limit = Float.( <= ) (Geometry.distance pa pb) limit in
  let cross (pa : point) (pb : point) limit = Float.( <= ) (pa.x -. pb.x) limit in
  reach_ok
  && span dist Left_hand Right_hand Config.max_hand_span
  && span dist Left_foot Right_foot Config.max_foot_span
  && span dist Left_hand Left_foot Config.max_body_length
  && span dist Right_hand Right_foot Config.max_body_length
  && span cross Left_hand Right_hand Config.max_cross_over
  && span cross Left_foot Right_foot Config.max_cross_over
;;

let replay ~wall ~start ~script ~on_accept =
  let broken = Set.empty (module Int) in
  List.fold script ~init:(start, []) ~f:(fun (player, rejects) action ->
    match action with
    | `R ->
      (match Movement.attempt_rest ~wall ~broken player with
       | Error reason -> player, (`Rest, reason) :: rejects
       | Ok next ->
         printf "turn %2d  rest             stamina %3d\n" next.turn next.stamina;
         next, rejects)
    | `M (limb, hold_id) ->
      let hold = Option.value_exn (Wall.find wall hold_id) in
      (match Movement.attempt_move ~wall ~broken player limb hold with
       | Error reason -> player, (`Move (limb, hold_id), reason) :: rejects
       | Ok next ->
         Expect_test_helpers_core.require
           (no_teleporting wall next)
           ~if_false_then_print_s:(lazy [%message "teleport!" (next : player_state)]);
         on_accept next limb hold_id;
         next, rejects))
;;

let%expect_test "ladder canary: scripted ascent wins, no teleporting" =
  let wall = Wall.test_wall_ladder in
  let final, rejects =
    replay ~wall ~start:Wall.ladder_start ~script:ladder_script ~on_accept:(fun next limb hold_id ->
      printf
        "turn %2d  %-10s -> %2d   stamina %3d  torso (%5.1f, %5.1f)  %s\n"
        next.turn
        (Sexp.to_string [%sexp (limb : limb)])
        hold_id
        next.stamina
        next.torso.x
        next.torso.y
        (Sexp.to_string
           [%sexp (Balance.stability wall next.limbs ~torso:next.torso : stability)]))
  in
  printf "rejections: %d\n" (List.length rejects);
  printf "stamina at the top: %d (must be > 0 to count as a win)\n" final.stamina;
  let on_finish id =
    match Option.bind id ~f:(Wall.find wall) with
    | Some { kind = Finish; _ } -> true
    | Some _ | None -> false
  in
  printf
    "both hands on finish: %b\n"
    (on_finish final.limbs.left_hand && on_finish final.limbs.right_hand);
  [%expect {|
    turn  1  Left_hand  ->  4   stamina  94  torso ( 55.0,  56.2)  Stable
    turn  2  Right_hand ->  5   stamina  88  torso ( 61.2,  64.7)  Stable
    turn  3  Left_foot  ->  2   stamina  83  torso ( 55.9,  63.5)  Stable
    turn  4  Right_foot ->  3   stamina  78  torso ( 62.0,  62.6)  Stable
    turn  5  Left_hand  ->  6   stamina  72  torso ( 56.5,  77.0)  Stable
    turn  6  Right_hand ->  7   stamina  66  torso ( 62.3,  87.7)  Stable
    turn  7  Left_foot  ->  4   stamina  61  torso ( 56.8,  88.3)  Stable
    turn  8  Right_foot ->  5   stamina  56  torso ( 62.6,  88.7)  Stable
    turn  9  rest             stamina  71
    turn 10  rest             stamina  86
    turn 11  rest             stamina 100
    turn 12  Left_hand  ->  8   stamina  94  torso ( 56.9, 104.0)  Stable
    turn 13  Right_hand ->  9   stamina  88  torso ( 62.7, 115.5)  Stable
    turn 14  Left_foot  ->  6   stamina  83  torso ( 57.0, 116.6)  Stable
    turn 15  Right_foot ->  7   stamina  78  torso ( 62.8, 117.5)  Stable
    turn 16  Left_hand  -> 10   stamina  72  torso ( 57.1, 133.1)  Stable
    turn 17  Right_hand -> 11   stamina  66  torso ( 62.8, 144.8)  Stable
    turn 18  Left_foot  ->  8   stamina  61  torso ( 57.1, 146.1)  Stable
    turn 19  Right_foot ->  9   stamina  56  torso ( 62.8, 147.1)  Stable
    turn 20  Left_hand  -> 12   stamina  50  torso ( 57.1, 162.8)  Stable
    turn 21  Right_hand -> 13   stamina  44  torso ( 62.8, 174.6)  Stable
    turn 22  Left_foot  -> 10   stamina  39  torso ( 57.1, 176.0)  Stable
    turn 23  Right_foot -> 11   stamina  34  torso ( 62.8, 177.0)  Stable
    turn 24  rest             stamina  49
    turn 25  rest             stamina  64
    turn 26  rest             stamina  79
    turn 27  rest             stamina  94
    turn 28  rest             stamina 100
    turn 29  Left_hand  -> 14   stamina  94  torso ( 57.1, 192.7)  Stable
    turn 30  Right_hand -> 15   stamina  88  torso ( 62.9, 204.5)  Stable
    turn 31  Left_foot  -> 12   stamina  83  torso ( 57.1, 205.9)  Stable
    turn 32  Right_foot -> 13   stamina  78  torso ( 62.9, 206.9)  Stable
    turn 33  Left_hand  -> 16   stamina  72  torso ( 57.1, 222.7)  Stable
    turn 34  Right_hand -> 17   stamina  66  torso ( 62.9, 234.5)  Stable
    turn 35  Left_foot  -> 14   stamina  61  torso ( 57.1, 235.9)  Stable
    turn 36  Right_foot -> 15   stamina  56  torso ( 62.9, 236.9)  Stable
    turn 37  Left_hand  -> 18   stamina  48  torso ( 57.1, 252.7)  Stable
    turn 38  Right_hand -> 19   stamina  38  torso ( 62.9, 264.5)  Stable
    rejections: 0
    stamina at the top: 38 (must be > 0 to count as a win)
    both hands on finish: true
    |}]
;;

(* §6.2 overhang_lean (foot desert): hands climb away from stranded feet,
   then reach back down — that pose must classify Critical — and the next
   upward move must be rejected because the shifting torso would strand a
   foot (no-teleport check). *)
let%expect_test "overhang_lean: critical pose, then the wall says no" =
  let wall = Wall.test_wall_overhang in
  let script =
    [ `M (Left_hand, 4) (* (40,120) *)
    ; `M (Right_hand, 5) (* (60,120) *)
    ; `M (Left_hand, 6) (* (40,150) *)
    ; `M (Right_hand, 7) (* (60,150) *)
    ; `M (Left_hand, 8) (* (50,130) *)
    ; `M (Right_hand, 3) (* back DOWN to (60,90): torso now hangs far off center *)
    ; `M (Left_hand, 9) (* (40,180): must be rejected *)
    ]
  in
  let _final, rejects =
    replay ~wall ~start:Wall.overhang_start ~script ~on_accept:(fun next limb hold_id ->
      let report =
        Option.value_exn (Balance.report wall next.limbs ~torso:next.torso)
      in
      printf
        "turn %d  %-10s -> %d   d %5.1f  %s\n"
        next.turn
        (Sexp.to_string [%sexp (limb : limb)])
        hold_id
        report.balance_distance
        (Sexp.to_string [%sexp (report.stability : stability)]))
  in
  List.iter (List.rev rejects) ~f:(fun (action, reason) ->
    match action with
    | `Rest -> printf !"rejected: rest (%{sexp:reject_reason})\n" reason
    | `Move (limb, hold_id) ->
      printf
        !"rejected: %{sexp:limb} -> %d (%{sexp:reject_reason})\n"
        limb
        hold_id
        reason);
  [%expect {|
    turn 1  Left_hand  -> 4   d   7.9  Stable
    turn 2  Right_hand -> 5   d  11.3  Stable
    turn 3  Left_hand  -> 6   d  19.8  Stable
    turn 4  Right_hand -> 7   d  24.2  Strained
    turn 5  Left_hand  -> 8   d  33.2  Strained
    turn 6  Right_hand -> 3   d  41.1  Critical
    rejected: Left_hand -> 9 (Limb_stranded)
    |}]
;;

(* §6.2 sloper_gate, the unchalked attempt: the only way up crosses two
   slopers; without chalk their grip drains the tank and the gate rejects
   the crossing (Insufficient_stamina) before the top. The chalked crossing
   is proven by the solver in test_tuning.ml (chalk_required = 2). *)
let%expect_test "sloper_gate unchalked: the wall wins" =
  let wall = Wall.test_wall_sloper_gate in
  let script =
    [ `M (Left_hand, 4)
    ; `M (Right_hand, 5)
    ; `M (Left_foot, 2)
    ; `M (Right_foot, 3)
    ; `M (Left_hand, 6) (* sloper, unchalked: grip 8/turn from here on *)
    ; `M (Right_hand, 7) (* both hands on slopers *)
    ; `M (Left_foot, 4)
    ; `M (Right_foot, 5)
    ; `M (Left_hand, 8) (* the exit reach *)
    ; `M (Right_hand, 9)
    ; `M (Left_hand, 10)
    ; `M (Right_hand, 11)
    ]
  in
  let final, rejects =
    replay ~wall ~start:Wall.sloper_gate_start ~script ~on_accept:(fun next limb hold_id ->
      printf
        "turn %2d  %-10s -> %2d   stamina %3d\n"
        next.turn
        (Sexp.to_string [%sexp (limb : limb)])
        hold_id
        next.stamina)
  in
  List.iter (List.rev rejects) ~f:(fun (action, reason) ->
    match action with
    | `Rest -> printf !"rejected: rest (%{sexp:reject_reason})\n" reason
    | `Move (limb, hold_id) ->
      printf !"rejected: %{sexp:limb} -> %d (%{sexp:reject_reason})\n" limb hold_id reason);
  printf "made it to the top: %b\n" (Game.won ~wall final);
  [%expect {|
    turn  1  Left_hand  ->  4   stamina  94
    turn  2  Right_hand ->  5   stamina  88
    turn  3  Left_foot  ->  2   stamina  83
    turn  4  Right_foot ->  3   stamina  78
    turn  5  Left_hand  ->  6   stamina  65
    turn  6  Right_hand ->  7   stamina  45
    turn  7  Left_foot  ->  4   stamina  26
    rejected: Right_foot -> 5 (Insufficient_stamina)
    rejected: Left_hand -> 8 (Out_of_reach)
    rejected: Right_hand -> 9 (Out_of_reach)
    rejected: Left_hand -> 10 (Out_of_reach)
    rejected: Right_hand -> 11 (Out_of_reach)
    made it to the top: false
    |}]
;;

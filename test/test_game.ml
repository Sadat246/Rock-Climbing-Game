open! Core
open Climb
open Climb.Types

(* Fall/exhaustion/rest rules at the Game layer (Phase 3): every fall sends
   the climber straight back to the start pose. *)

let ladder_game ?stamina () =
  let start =
    match stamina with
    | None -> Wall.ladder_start
    | Some stamina -> { Wall.ladder_start with stamina }
  in
  Game.create ~wall:Wall.test_wall_ladder ~start
;;

let status game = (Game.current_state game).status

let back_at_start game (start : player_state) =
  [%equal: player_state] (Game.current_state game).player start
;;

let%expect_test "desperate move: physically fine, unaffordable -> fall to start" =
  (* LH -> jug 4 costs 6 (hand up 3 + upkeep 1 + grip 2). With 5 stamina the
     gate calls it Insufficient_stamina; committing through Game = a fall. *)
  let start = { Wall.ladder_start with stamina = 5 } in
  let game = Game.create ~wall:Wall.test_wall_ladder ~start in
  (match Game.move game Left_hand ~hold_id:4 with
   | `Fell (game, reason) ->
     printf "%s\n" reason;
     printf
       !"status %{sexp:game_status}, back at start %b\n"
       (status game)
       (back_at_start game start);
     (* undo still shows the pre-fall position for learning *)
     (match Game.undo game with
      | Some undone ->
        printf "undo -> pre-fall pose restored: %b\n" (back_at_start undone start)
      | None -> print_endline "no undo?")
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    grip gave out reaching hold 4 (needed 6 stamina, had 5)
    status Playing, back at start true
    undo -> pre-fall pose restored: true
    |}]
;;

let%expect_test "off-balance body positions are attemptable — and drop you" =
  (* The ledge pose from test_movement: torso lagged far left; stepping the
     right foot further right leaves the torso horizontally unsupported.
     Phase-3 rule: that's a FALL, not a rejection. *)
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
  let start =
    { Wall.ladder_start with
      limbs =
        { left_hand = Some 0; right_hand = Some 1; left_foot = Some 2; right_foot = Some 3 }
    ; torso = { x = 25.; y = 60. }
    }
  in
  let game = Game.create ~wall:ledge_wall ~start in
  (match Game.move game Right_foot ~hold_id:4 with
   | `Fell (game, reason) ->
     printf "%s\n" reason;
     printf !"status %{sexp:game_status}, back at start %b\n" (status game) (back_at_start game start)
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    you lost your balance lunging for hold 4
    status Playing, back at start true
    |}]
;;

let%expect_test "stranding another limb is also a fall" =
  (* Lagged-high torso: grabbing jug 8 would drag the torso beyond the left
     foot's reach — the foot pops, the climber falls. *)
  let start = { Wall.ladder_start with torso = { x = 60.; y = 110. } } in
  let game = Game.create ~wall:Wall.test_wall_ladder ~start in
  (match Game.move game Left_hand ~hold_id:8 with
   | `Fell (game, reason) ->
     printf "%s\n" reason;
     printf !"back at start %b\n" (back_at_start game start)
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    reaching hold 8 yanked another limb off the wall
    back at start true
    |}]
;;

let%expect_test "lunging for an out-of-reach hold is a fall, not a rejection" =
  let start = Wall.ladder_start in
  let game = ladder_game () in
  (* jug 16 (40,270) is ~225 units away — a wild lunge *)
  (match Game.move game Left_hand ~hold_id:16 with
   | `Fell (game, reason) ->
     printf "%s\n" reason;
     printf !"back at start %b\n" (back_at_start game start)
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    hold 16 was out of reach - you lunged and missed
    back at start true
    |}]
;;

let%expect_test "overstretching the body span is a fall too" =
  (* span_wall pose from test_movement: grabbing the far jug would put the
     hands 151 apart (max 150) *)
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  let span_wall =
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
  in
  let limbs =
    { left_hand = Some 0; right_hand = Some 1; left_foot = Some 3; right_foot = Some 4 }
  in
  let start =
    { Wall.ladder_start with
      limbs
    ; torso =
        Geometry.average
          (List.map (Player.attached limbs) ~f:(fun (_, id) ->
             Wall.position_exn span_wall id))
    }
  in
  let game = Game.create ~wall:span_wall ~start in
  (match Game.move game Right_hand ~hold_id:2 with
   | `Fell (game, reason) ->
     printf "%s\n" reason;
     printf !"back at start %b\n" (back_at_start game start)
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    you overstretched going for hold 2 and came off
    back at start true
    |}]
;;

let%expect_test "exact-cost move to 0 stamina: Stable = grace, then doom" =
  let game = ladder_game ~stamina:6 () in
  (match Game.move game Left_hand ~hold_id:4 with
   | `Moved (game, cost) ->
     printf
       !"cost %d, stamina %d, status %{sexp:game_status}\n"
       cost.total
       (Game.current_state game).player.stamina
       (status game);
     (* pumped out and not on a Rest hold: the next attempt is desperate *)
     (match Game.move game Right_hand ~hold_id:5 with
      | `Fell (_, reason) -> printf "then: %s\n" reason
      | `Moved _ -> print_endline "unexpectedly moved"
      | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r)
   | `Fell _ -> print_endline "unexpectedly fell"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    cost 6, stamina 0, status Playing
    then: grip gave out reaching hold 5 (needed 6 stamina, had 0)
    |}]
;;

let%expect_test "reaching 0 in a Strained pose falls immediately (to start)" =
  (* On the overhang wall the 4th move lands Strained (see test_scenarios);
     moves cost 6,6,6,9 — with exactly 27 stamina the 4th move hits 0 while
     Strained. *)
  let start = { Wall.overhang_start with stamina = 27 } in
  let game = Game.create ~wall:Wall.test_wall_overhang ~start in
  let script = [ Left_hand, 4; Right_hand, 5; Left_hand, 6; Right_hand, 7 ] in
  let final =
    List.fold script ~init:game ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Moved (game, _) ->
        printf
          "turn %d: stamina %2d\n"
          (Game.current_state game).player.turn
          (Game.current_state game).player.stamina;
        game
      | `Fell (game, reason) ->
        printf "FELL: %s — back at start %b\n" reason (back_at_start game start);
        game
      | `Rejected r ->
        printf !"rejected: %{sexp:reject_reason}\n" r;
        game)
  in
  ignore (final : Game.t);
  [%expect {|
    turn 1: stamina 21
    turn 2: stamina 15
    turn 3: stamina  9
    FELL: exhausted in a Strained pose — back at start true
    |}]
;;

let%expect_test "rest: rejected off Rest holds, works on them, caps at start" =
  let game = ladder_game () in
  (match Game.rest game with
   | `Rejected reason -> printf !"at start: %{sexp:reject_reason}\n" reason
   | `Rested _ | `Fell _ -> print_endline "unexpectedly rested");
  (* climb two cycles so the hands sit on the y=120 Rest row (ids 6,7) *)
  let script =
    [ Left_hand, 4; Right_hand, 5; Left_foot, 2; Right_foot, 3
    ; Left_hand, 6; Right_hand, 7; Left_foot, 4; Right_foot, 5
    ]
  in
  let game =
    List.fold script ~init:game ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Moved (game, _) -> game
      | `Fell _ | `Rejected _ -> failwith "setup move failed")
  in
  printf "before rest: stamina %d\n" (Game.current_state game).player.stamina;
  let rec rest_until_capped game n =
    if n = 0
    then game
    else (
      match Game.rest game with
      | `Rested game ->
        printf "rested: stamina %d\n" (Game.current_state game).player.stamina;
        rest_until_capped game (n - 1)
      | `Fell _ -> failwith "unexpected fall while resting"
      | `Rejected r ->
        printf !"rejected: %{sexp:reject_reason}\n" r;
        game)
  in
  ignore (rest_until_capped game 4 : Game.t);
  [%expect {|
    at start: Cannot_rest
    before rest: stamina 56
    rested: stamina 71
    rested: stamina 86
    rested: stamina 100
    rested: stamina 100
    |}]
;;

let%expect_test "crumbling hold: wears while occupied, breaks, limb detaches" =
  let game = Game.create ~wall:Wall.test_wall_crumble_trap ~start:Wall.crumble_trap_start in
  (* grab the crumbling jug (durability 2), then linger: each turn it is
     occupied wears it by one *)
  let move g limb hold_id =
    match Game.move g limb ~hold_id with
    | `Moved (g, _) -> g
    | `Fell (_, r) -> raise_s [%message "fell" (r : string)]
    | `Rejected r -> raise_s [%message "rejected" (r : reject_reason)]
  in
  let g = move game Left_hand 4 in
  let gs = Game.current_state g in
  printf
    !"after grab:   wear %{sexp:int Map.M(Int).t}  broken %{sexp:Set.M(Int).t}\n"
    gs.hold_wear
    gs.broken_holds;
  let g = move g Right_hand 5 in
  let gs = Game.current_state g in
  printf
    !"after linger: wear %{sexp:int Map.M(Int).t}  broken %{sexp:Set.M(Int).t}  LH %{sexp:int option}  status %{sexp:game_status}\n"
    gs.hold_wear
    gs.broken_holds
    gs.player.limbs.left_hand
    gs.status;
  (* the crumbled hold is gone for good: grabbing it again is Hold_broken *)
  (match Game.move g Left_hand ~hold_id:4 with
   | `Rejected r -> printf !"regrab: %{sexp:reject_reason}\n" r
   | `Moved _ | `Fell _ -> print_endline "regrabbed a broken hold?!");
  (* the detached hand can re-place on a live hold *)
  let g = move g Left_hand 2 in
  printf !"re-placed LH: %{sexp:int option}\n" (Game.current_state g).player.limbs.left_hand;
  [%expect {|
    after grab:   wear ((4 1))  broken ()
    after linger: wear ()  broken (4)  LH ()  status Playing
    regrab: Hold_broken
    re-placed LH: (2)
    |}]
;;

let%expect_test "crumbling under a critical pose is a fall (to the start)" =
  (* Custom wall: the crumbling hold is the ONLY thing keeping the torso
     inside the horizontal support margin — when it breaks, the pose fails
     and the climber falls. *)
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  let wall =
    { holds =
        [| mk 0 90. 30. Foothold
         ; mk 1 110. 30. Foothold
         ; { id = 2; position = { x = 30.; y = 80. }; kind = Crumbling; durability = Some 1 }
         ; mk 3 110. 80. Jug
         ; mk 4 100. 60. Jug
        |]
    ; width = 140
    ; height = 120
    ; finish_y = 120.
    }
  in
  let limbs =
    { left_hand = Some 2; right_hand = Some 3; left_foot = Some 0; right_foot = Some 1 }
  in
  let start =
    { Wall.crumble_trap_start with
      limbs
    ; torso = { x = 45.; y = 60. } (* hanging left, off the crumbling hold *)
    }
  in
  let game = Game.create ~wall ~start in
  (* any action passes a turn; the crumbling hold (durability 1) breaks and
     the torso is left outside [min support x - margin] *)
  (match Game.move game Right_hand ~hold_id:4 with
   | `Fell (g, reason) ->
     printf "%s\n" reason;
     printf
       "back at start %b\n"
       ([%equal: player_state] (Game.current_state g).player start)
   | `Moved _ -> print_endline "unexpectedly survived"
   | `Rejected r -> printf !"rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    hold 2 crumbled under you
    back at start true
    |}]
;;

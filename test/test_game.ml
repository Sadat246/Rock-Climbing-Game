open! Core
open Climb
open Climb.Types

(* Fall/exhaustion/rest rules at the Game layer (Phase 3). *)

let ladder_game ?stamina () =
  let start =
    match stamina with
    | None -> Wall.ladder_start
    | Some stamina -> { Wall.ladder_start with stamina }
  in
  Game.create ~wall:Wall.test_wall_ladder ~start
;;

let status game = (Game.current_state game).status

let%expect_test "desperate move: physically fine, unaffordable -> you FALL" =
  (* LH -> jug 4 costs 6 (hand up 3 + upkeep 1 + grip 2). With 5 stamina the
     gate calls it Insufficient_stamina; committing through Game = a fall. *)
  let game = ladder_game ~stamina:5 () in
  (match Game.move game Left_hand ~hold_id:4 with
   | `Fell game -> print_s [%sexp (status game : game_status)]
   | `Moved _ -> print_endline "unexpectedly moved"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {| (Fallen "grip gave out reaching hold 4 (needed 6 stamina, had 5)") |}]
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
      | `Fell game -> print_s [%sexp (status game : game_status)]
      | `Moved _ -> print_endline "unexpectedly moved"
      | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r)
   | `Fell _ -> print_endline "unexpectedly fell"
   | `Rejected r -> printf !"unexpectedly rejected: %{sexp:reject_reason}\n" r);
  [%expect {|
    cost 6, stamina 0, status Playing
    (Fallen "grip gave out reaching hold 5 (needed 6 stamina, had 0)")
    |}]
;;

let%expect_test "reaching 0 in a Strained pose falls immediately" =
  (* On the overhang wall the 4th move lands Strained (see test_scenarios);
     moves cost 6,6,6,9 — with exactly 27 stamina the 4th move hits 0 while
     Strained. *)
  let game =
    Game.create
      ~wall:Wall.test_wall_overhang
      ~start:{ Wall.overhang_start with stamina = 27 }
  in
  let script = [ Left_hand, 4; Right_hand, 5; Left_hand, 6; Right_hand, 7 ] in
  let final =
    List.fold script ~init:game ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Moved (game, _) ->
        printf
          !"turn %d: stamina %2d  %{sexp:game_status}\n"
          (Game.current_state game).player.turn
          (Game.current_state game).player.stamina
          (status game);
        game
      | `Fell game ->
        printf !"turn fell: %{sexp:game_status}\n" (status game);
        game
      | `Rejected r ->
        printf !"rejected: %{sexp:reject_reason}\n" r;
        game)
  in
  ignore (final : Game.t);
  [%expect {|
    turn 1: stamina 21  Playing
    turn 2: stamina 15  Playing
    turn 3: stamina  9  Playing
    turn 4: stamina  0  (Fallen "exhausted in a Strained pose")
    |}]
;;

let%expect_test "rest: rejected off Rest holds, works on them, caps at start" =
  let game = ladder_game () in
  (match Game.rest game with
   | `Rejected reason -> printf !"at start: %{sexp:reject_reason}\n" reason
   | `Rested _ -> print_endline "unexpectedly rested");
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

let%expect_test "falling sends you back: reset restores the session's start pose" =
  let start = { Wall.ladder_start with stamina = 5 } in
  let game = Game.create ~wall:Wall.test_wall_ladder ~start in
  (match Game.move game Left_hand ~hold_id:4 with
   | `Fell fallen ->
     let fresh = Game.reset fallen in
     printf
       "reset restores the start this session began with: %b\n"
       ([%equal: player_state] (Game.current_state fresh).player start);
     printf !"status %{sexp:game_status}\n" (status fresh);
     (* undo also steps back off the fall *)
     (match Game.undo fallen with
      | Some undone -> printf !"undo -> %{sexp:game_status}\n" (status undone)
      | None -> print_endline "no undo?")
   | `Moved _ | `Rejected _ -> print_endline "expected a fall");
  [%expect {|
    reset restores the start this session began with: true
    status Playing
    undo -> Playing
    |}]
;;

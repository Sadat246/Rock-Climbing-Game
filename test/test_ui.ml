open! Core
open Climb
open Climb.Types

let new_game () = Game.create ~wall:Wall.test_wall_ladder ~start:Wall.ladder_start

let%expect_test "reachable candidates per limb from the ladder start" =
  let gs = Game.current_state (new_game ()) in
  List.iter all_of_limb ~f:(fun limb ->
    let ui = Ui.select_limb gs limb in
    printf
      !"%-10s %{sexp:int list}\n"
      (Sexp.to_string [%sexp (limb : limb)])
      ui.candidates);
  [%expect {|
    Left_hand  (11 12 13)
    Right_hand (10 12 13)
    Left_foot  (1 10 11)
    Right_foot (0 10 11)
    |}]
;;

let%expect_test "cycling wraps both directions" =
  let gs = Game.current_state (new_game ()) in
  let ui = Ui.select_limb gs Left_hand in
  let n = List.length ui.candidates in
  let forward = Fn.apply_n_times ~n Ui.next ui in
  printf "forward full circle back to start: %b\n" (Option.equal Int.equal (Ui.target forward) (Ui.target ui));
  printf
    "prev from start = last candidate: %b\n"
    (Option.equal Int.equal (Ui.target (Ui.prev ui)) (List.last ui.candidates));
  [%expect {|
    forward full circle back to start: true
    prev from start = last candidate: true
    |}]
;;

let%expect_test "game move, reject, undo" =
  let game = new_game () in
  (match Game.move game Left_hand ~hold_id:16 with
   | `Rejected reason -> printf !"rejected: %{sexp:reject_reason}\n" reason
   | `Moved _ -> print_endline "unexpectedly moved");
  (match Game.move game Left_hand ~hold_id:12 with
   | `Rejected _ -> print_endline "unexpectedly rejected"
   | `Moved game' ->
     printf "moved, turn %d\n" (Game.current_state game').player.turn;
     (match Game.undo game' with
      | None -> print_endline "no undo?"
      | Some game'' ->
        printf
          "undo restores start: %b\n"
          ([%equal: player_state]
             (Game.current_state game'').player
             (Game.current_state game).player)));
  printf "undo at start: %b\n" (Option.is_none (Game.undo game));
  [%expect {|
    rejected: Out_of_reach
    moved, turn 1
    undo restores start: true
    undo at start: true
    |}]
;;

let ladder_script =
  [ Left_hand, 12; Right_hand, 13; Left_foot, 2; Right_foot, 3
  ; Left_hand, 14; Right_hand, 15; Left_foot, 4; Right_foot, 5
  ; Left_hand, 16; Right_hand, 17; Left_foot, 6; Right_foot, 7
  ; Left_hand, 18; Right_hand, 19
  ]
;;

let%expect_test "win detection: full ascent through Game ends Won" =
  let final =
    List.fold ladder_script ~init:(new_game ()) ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Moved game -> game
      | `Rejected reason ->
        raise_s [%message "rejected" (limb : limb) (hold_id : int) (reason : reject_reason)])
  in
  print_s [%sexp ((Game.current_state final).status : game_status)];
  (* one move before the last hand reaches the finish, we are still Playing *)
  let all_but_last = List.drop_last_exn ladder_script in
  let almost =
    List.fold all_but_last ~init:(new_game ()) ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Moved game -> game
      | `Rejected _ -> game)
  in
  print_s [%sexp ((Game.current_state almost).status : game_status)];
  [%expect {|
    Won
    Playing
    |}]
;;

let%expect_test "render_with_ui: highlighted target and HUD" =
  let gs = Game.current_state (new_game ()) in
  let ui = Ui.next (Ui.select_limb gs Left_hand) in
  print_endline (Ascii.render_with_ui gs ui);
  [%expect {|
        F   F


        .   .


        .   .


        .   .


        .   .


        .   .


        @   .


        .   .


        h   H
          T

        f   Q



    turn 0  stamina 100  chalk 5  status Playing
    limb Left_hand  target @ hold 12 (Jug)  reachable (11 12 13)
    Left_hand: 3 reachable holds
    |}]
;;

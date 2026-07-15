open! Core
open Climb
open Climb.Types

let start_state =
  { player = Wall.ladder_start
  ; wall = Wall.test_wall_ladder
  ; broken_holds = Set.empty (module Int)
  ; status = Playing
  }
;;

let%expect_test "ladder start pose" =
  print_endline (Ascii.render start_state);
  [%expect {|
        F   F


        .   .


        .   .


        .   .


        .   .


        .   .


        .   .


        .   .


        h   H
          T

        f   Q



    turn 0  status Playing
    |}]
;;

let%expect_test "board after one move; renderer is pure" =
  let gs = start_state in
  let hold = Option.value_exn (Wall.find gs.wall 12) in
  let player =
    Movement.attempt_move ~wall:gs.wall ~broken:gs.broken_holds gs.player Left_hand hold
    |> Result.ok
    |> Option.value_exn
  in
  let gs = { gs with player } in
  print_endline (Ascii.render gs);
  printf "pure: %b\n" (String.equal (Ascii.render gs) (Ascii.render gs));
  [%expect {|
        F   F


        .   .


        .   .


        .   .


        .   .


        .   .


        h   .


        .   .


        . T H


        f   Q



    turn 1  status Playing
    pure: true
    |}]
;;

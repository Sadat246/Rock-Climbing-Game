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


        J   J


        J   J


        R   R


        J   J


        J   J


        R   R


        J   J


        h   H
          T

        f   Q



    turn 0  stamina 100  chalk 5  status Playing
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
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Option.value_exn None" test/test_ascii.ml:57:7)
  Raised at Base__Error.raise in file "src/error.ml", line 17, characters 38-66
  Called from Base__Error.raise in file "src/error.ml" (inlined), line 25, characters 47-66
  Called from Base__Option.value_exn__bits64 in file "src/option.ml", line 87, characters 13-30
  Called from Climb_test__Test_ascii.(fun) in file "test/test_ascii.ml", lines 55-57, characters 4-129
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 359, characters 10-25
  |}]
;;

open! Core
open Climb
open Climb.Types

let new_game () = Game.create ~wall:Wall.test_wall_ladder ~start:Wall.ladder_start

let%expect_test "cursor candidates per limb: all holds except the limb's own" =
  let gs = Game.current_state (new_game ()) in
  List.iter all_of_limb ~f:(fun limb ->
    let ui = Ui.select_limb gs limb in
    printf
      !"%-10s %{sexp:int list}\n"
      (Sexp.to_string [%sexp (limb : limb)])
      ui.candidates);
  [%expect {|
    Left_hand  (0 1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    Right_hand (0 1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    Left_foot  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    Right_foot (0 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
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
  (* a foothold is un-grabbable by a hand: still a polite rejection *)
  (match Game.move game Left_hand ~hold_id:0 with
   | `Rejected reason -> printf !"rejected: %{sexp:reject_reason}\n" reason
   | `Moved _ | `Fell _ -> print_endline "unexpectedly moved");
  (match Game.move game Left_hand ~hold_id:4 with
   | `Rejected _ | `Fell _ -> print_endline "unexpectedly rejected"
   | `Moved (game', _cost) ->
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
    rejected: Wrong_limb_for_hold
    moved, turn 1
    undo restores start: true
    undo at start: true
    |}]
;;

(* Same rest-managed ascent as test_scenarios: hands two rung-rows ahead,
   feet following onto the vacated rungs, shaking out at the Rest rows. *)
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

let run_action game action =
  match action with
  | `R ->
    (match Game.rest game with
     | `Rested game -> game
     | `Rejected reason -> raise_s [%message "rest rejected" (reason : reject_reason)])
  | `M (limb, hold_id) ->
    (match Game.move game limb ~hold_id with
     | `Moved (game, _) -> game
     | `Fell _ -> raise_s [%message "fell" (limb : limb) (hold_id : int)]
     | `Rejected reason ->
       raise_s [%message "rejected" (limb : limb) (hold_id : int) (reason : reject_reason)])
;;

let%expect_test "win detection: full ascent through Game ends Won" =
  let final = List.fold ladder_script ~init:(new_game ()) ~f:run_action in
  print_s [%sexp ((Game.current_state final).status : game_status)];
  (* one move before the last hand reaches the finish, we are still Playing *)
  let all_but_last = List.drop_last_exn ladder_script in
  let almost = List.fold all_but_last ~init:(new_game ()) ~f:run_action in
  print_s [%sexp ((Game.current_state almost).status : game_status)];
  [%expect {|
    Won
    Playing
    |}]
;;

let%expect_test "no hints: the cursor ranges over every hold on the wall" =
  let gs = Game.current_state (new_game ()) in
  let ui = Ui.select_limb gs Left_hand in
  (* everything except the hold the left hand is on (id 2) — reachable or not *)
  printf !"candidates %{sexp:int list}\n" ui.candidates;
  print_endline ui.message;
  [%expect {|
    candidates (0 1 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    Left_hand selected
    |}]
;;

let%expect_test "render_with_ui: highlighted target and HUD" =
  let gs = Game.current_state (new_game ()) in
  let ui = Ui.next (Ui.select_limb gs Left_hand) in
  print_endline (Ascii.render_with_ui gs ui);
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

        f   @



    turn 0  stamina 100  chalk 5  status Playing
    limb Left_hand  target @ hold 1 (Foothold)
    now:   torso (60,45)  support (60,45)  d 0.0  Stable
    Left_hand selected
    |}]
;;

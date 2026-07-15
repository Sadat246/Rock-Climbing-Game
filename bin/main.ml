open! Core
open Climb
open Climb.Types

(* Phase 0: a hardcoded move script up the ladder wall (interactive control is
   Phase 1). Every move goes through Movement.attempt_move — bin never has its
   own legality logic.

   Flags:
     --ascii    headless mode: print ASCII boards to stdout, no window
     --no-wait  don't block on a keypress at the end (for scripted runs) *)

(* The two leading moves are deliberately illegal so the typed-rejection path
   is visible: hold 16 is far out of reach from the start, and hold 18 is a
   Finish hold (hands only) targeted with a foot. *)
let script =
  [ Left_hand, 16 (* jug (40, 240): Out_of_reach demo *)
  ; Right_foot, 18 (* finish (40, 300): Wrong_limb_for_hold demo *)
  ; Left_hand, 12
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

let both_hands_on_finish (gs : game_state) =
  let on_finish = function
    | None -> false
    | Some id ->
      (match Wall.find gs.wall id with
       | Some { kind = Finish; _ } -> true
       | Some _ | None -> false)
  in
  on_finish gs.player.limbs.left_hand && on_finish gs.player.limbs.right_hand
;;

let show ~ascii gs =
  if ascii
  then printf "%s\n\n" (Ascii.render gs)
  else (
    Climb_graphics.Graphics_view.draw gs;
    ignore (Core_unix.nanosleep Config.render_delay_s : float))
;;

let () =
  let args = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  let ascii = List.mem args "--ascii" ~equal:String.equal in
  let no_wait = List.mem args "--no-wait" ~equal:String.equal in
  let wall = Wall.test_wall_ladder in
  let gs =
    { player = Wall.ladder_start
    ; wall
    ; broken_holds = Set.empty (module Int)
    ; status = Playing
    }
  in
  if not ascii then Climb_graphics.Graphics_view.init wall;
  show ~ascii gs;
  let final =
    List.fold script ~init:gs ~f:(fun gs (limb, hold_id) ->
      let hold =
        Option.value_exn (Wall.find wall hold_id) ~message:"script names a missing hold"
      in
      match Movement.attempt_move ~wall ~broken:gs.broken_holds gs.player limb hold with
      | Error reason ->
        printf
          !"turn %d: %{sexp:limb} -> hold %d REJECTED (%{sexp:reject_reason})\n"
          gs.player.turn
          limb
          hold_id
          reason;
        gs
      | Ok player ->
        printf
          !"turn %d: %{sexp:limb} -> hold %d ok, torso (%.0f, %.0f)\n"
          player.turn
          limb
          hold_id
          player.torso.x
          player.torso.y;
        let gs = { gs with player } in
        show ~ascii gs;
        gs)
  in
  if both_hands_on_finish final
  then printf "Both hands on the finish holds after %d turns.\n" final.player.turn
  else printf "Script ended without reaching the finish.\n";
  if not ascii
  then (
    if not no_wait then Climb_graphics.Graphics_view.wait_for_key ();
    Climb_graphics.Graphics_view.close ())
;;

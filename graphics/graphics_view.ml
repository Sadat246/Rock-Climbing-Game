open! Core
open Climb
open Climb.Types

let px f = Int.of_float (f *. Config.pixels_per_unit)
let to_screen (p : point) = Config.window_margin_px + px p.x, Config.window_margin_px + px p.y

let init (wall : wall) =
  let w = px (Float.of_int wall.width) + (2 * Config.window_margin_px) in
  let h = px (Float.of_int wall.height) + (2 * Config.window_margin_px) in
  match Graphics.open_graph (sprintf " %dx%d" w h) with
  | () ->
    Graphics.set_window_title "Rock Climbing — Phase 0";
    (* Double-buffer: we draw to the backing store and flip in [draw]. *)
    Graphics.auto_synchronize false;
    Ok ()
  | exception Graphics.Graphic_failure message -> Error message
;;

let draw_hold ~broken (h : hold) =
  if not (Set.mem broken h.id)
  then (
    let x, y = to_screen h.position in
    match h.kind with
    | Finish ->
      Graphics.set_color Graphics.red;
      Graphics.fill_circle x y 5
    | Jug | Crimp | Sloper | Foothold | Rest | Crumbling | Chalk_refill ->
      Graphics.set_color Graphics.black;
      Graphics.fill_circle x y 3)
;;

let draw_climber (wall : wall) (player : player_state) =
  let tx, ty = to_screen player.torso in
  Graphics.set_color Graphics.blue;
  (* Stick figure: a line from the torso to each attached limb. *)
  List.iter (Player.attached player.limbs) ~f:(fun (_, hold_id) ->
    let x, y = to_screen (Wall.position_exn wall hold_id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  Graphics.fill_circle tx ty 5;
  (* Head *)
  Graphics.draw_circle tx (ty + 9) 4
;;

let draw_scene (gs : game_state) =
  Graphics.clear_graph ();
  Array.iter gs.wall.holds ~f:(draw_hold ~broken:gs.broken_holds);
  draw_climber gs.wall gs.player
;;

let draw (gs : game_state) =
  draw_scene gs;
  Graphics.synchronize ()
;;

let green = Graphics.rgb 0 160 0
let pale_green = Graphics.rgb 130 210 130
let gray = Graphics.rgb 150 150 150

let stability_color = function
  | Stable -> green
  | Strained -> Graphics.rgb 230 140 0
  | Critical -> Graphics.red
  | Falling -> Graphics.rgb 120 0 0
;;

(* Post-move state for the highlighted target, via the single gate (pure). *)
let preview (gs : game_state) (ui : Ui.t) =
  Option.bind (Ui.target ui) ~f:(fun id ->
    Option.bind (Wall.find gs.wall id) ~f:(fun hold ->
      Movement.attempt_move ~wall:gs.wall ~broken:gs.broken_holds gs.player ui.limb hold
      |> Result.ok))
;;

let balance_summary (gs : game_state) ~limbs ~(torso : point) =
  match Balance.report gs.wall limbs ~torso with
  | None -> "no supports", Falling
  | Some r ->
    ( sprintf
        !"torso (%.0f,%.0f) support (%.0f,%.0f) d %.1f %{sexp:stability}"
        torso.x
        torso.y
        r.support_center.x
        r.support_center.y
        r.balance_distance
        r.stability
    , r.stability )
;;

let draw_with_ui (gs : game_state) (ui : Ui.t) =
  draw_scene gs;
  (* All reachable holds for the selected limb: pale rings. *)
  Graphics.set_color pale_green;
  List.iter ui.candidates ~f:(fun id ->
    let x, y = to_screen (Wall.position_exn gs.wall id) in
    Graphics.draw_circle x y 6);
  (* Selected limb's line redrawn in green. *)
  Graphics.set_color green;
  Option.iter (Player.limb_hold_id gs.player.limbs ui.limb) ~f:(fun id ->
    let tx, ty = to_screen gs.player.torso in
    let x, y = to_screen (Wall.position_exn gs.wall id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  (* Highlighted target hold: bold green ring. *)
  Option.iter (Ui.target ui) ~f:(fun id ->
    let x, y = to_screen (Wall.position_exn gs.wall id) in
    Graphics.draw_circle x y 7;
    Graphics.draw_circle x y 8);
  (* Ghost torso: where the torso would shift if the move is confirmed. *)
  let after = preview gs ui in
  Option.iter after ~f:(fun (p : player_state) ->
    let gx, gy = to_screen p.torso in
    Graphics.set_color gray;
    Graphics.draw_circle gx gy 5;
    let tx, ty = to_screen gs.player.torso in
    Graphics.moveto tx ty;
    Graphics.lineto gx gy);
  (* HUD: status, message, then current/preview balance (§ Phase 2 debug HUD),
     each balance line tinted by its stability. *)
  let hud_y = Graphics.size_y () - 14 in
  Graphics.set_color Graphics.black;
  Graphics.moveto 4 hud_y;
  Graphics.draw_string
    (sprintf
       !"turn %d  %{sexp:limb}%s  %{sexp:game_status}"
       gs.player.turn
       ui.limb
       (match Ui.target ui with
        | None -> " (no reachable hold)"
        | Some id -> sprintf " -> hold %d" id)
       gs.status);
  Graphics.moveto 4 (hud_y - 12);
  Graphics.draw_string ui.message;
  let now_line, now_stab =
    balance_summary gs ~limbs:gs.player.limbs ~torso:gs.player.torso
  in
  Graphics.set_color (stability_color now_stab);
  Graphics.moveto 4 (hud_y - 24);
  Graphics.draw_string ("now:   " ^ now_line);
  (match after with
   | None -> ()
   | Some (p : player_state) ->
     let after_line, after_stab = balance_summary gs ~limbs:p.limbs ~torso:p.torso in
     Graphics.set_color (stability_color after_stab);
     Graphics.moveto 4 (hud_y - 36);
     Graphics.draw_string ("after: " ^ after_line));
  Graphics.synchronize ()
;;

(* Screen-position -> nearest hold within the click radius. *)
let hold_at (wall : wall) ~mouse_x ~mouse_y =
  Array.fold wall.holds ~init:None ~f:(fun best h ->
    let x, y = to_screen h.position in
    let d = Float.hypot (Float.of_int (x - mouse_x)) (Float.of_int (y - mouse_y)) in
    if Float.( <= ) d (Float.of_int Config.click_radius_px)
    then (
      match best with
      | Some (_, best_d) when Float.( <= ) best_d d -> best
      | Some _ | None -> Some (h.id, d))
    else best)
  |> Option.map ~f:fst
;;

type event =
  | Key of char
  | Click of int option (* hold id under the pointer, if any *)
  | Hover of int option

let next_event (wall : wall) =
  let st =
    Graphics.wait_next_event [ Graphics.Button_down; Graphics.Key_pressed; Graphics.Mouse_motion ]
  in
  if st.keypressed
  then Key (Graphics.read_key ())
  else if st.button
  then Click (hold_at wall ~mouse_x:st.mouse_x ~mouse_y:st.mouse_y)
  else Hover (hold_at wall ~mouse_x:st.mouse_x ~mouse_y:st.mouse_y)
;;

let read_key () = Graphics.read_key ()
let wait_for_key () = ignore (Graphics.read_key () : char)
let close () = Graphics.close_graph ()

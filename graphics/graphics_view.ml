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

let draw_with_ui (gs : game_state) (ui : Ui.t) =
  draw_scene gs;
  (* Selected limb's line redrawn in green. *)
  Graphics.set_color green;
  Option.iter (Player.limb_hold_id gs.player.limbs ui.limb) ~f:(fun id ->
    let tx, ty = to_screen gs.player.torso in
    let x, y = to_screen (Wall.position_exn gs.wall id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  (* Highlighted target hold: green ring. *)
  Option.iter (Ui.target ui) ~f:(fun id ->
    let x, y = to_screen (Wall.position_exn gs.wall id) in
    Graphics.draw_circle x y 7;
    Graphics.draw_circle x y 8);
  (* HUD: two text lines at the top of the window. *)
  let hud_y = Graphics.size_y () - 14 in
  Graphics.set_color Graphics.black;
  Graphics.moveto 4 hud_y;
  Graphics.draw_string
    (sprintf
       !"turn %d  %{sexp:limb}%s  status %{sexp:game_status}"
       gs.player.turn
       ui.limb
       (match Ui.target ui with
        | None -> " (no reachable hold)"
        | Some id -> sprintf " -> hold %d" id)
       gs.status);
  Graphics.moveto 4 (hud_y - 12);
  Graphics.draw_string ui.message;
  Graphics.synchronize ()
;;

let read_key () = Graphics.read_key ()
let wait_for_key () = ignore (Graphics.read_key () : char)
let close () = Graphics.close_graph ()
